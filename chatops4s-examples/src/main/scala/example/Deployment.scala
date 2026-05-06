package example

import cats.effect.{IO, IOApp}
import scala.concurrent.duration.*
import cats.syntax.all.*
import chatops4s.slack.{ButtonClick, ButtonId, CommandArgCodec, CommandParser, CommandResponse, FormDef, FormSubmission, InitialValues, SlackGateway}
import chatops4s.slack.api.{SlackAppToken, SlackBotToken}
import sttp.client4.httpclient.fs2.HttpClientFs2Backend

object Deployment extends IOApp.Simple {

  private lazy val token    = SlackBotToken.unsafe(sys.env.getOrElse("SLACK_BOT_TOKEN", "xoxb-your-token"))
  private lazy val appToken = SlackAppToken.unsafe(sys.env.getOrElse("SLACK_APP_TOKEN", "xapp-your-app-token"))
  private val channel       = sys.env.getOrElse("SLACK_CHANNEL", "#testing-slack-app")

  override def run: IO[Unit] = {
    HttpClientFs2Backend.resource[IO]().use { backend =>
      for {
        slack      <- SlackGateway.create(backend)
        approveBtn <- slack.registerButton[ServiceVersion]("approve-deploy")(onApprove(slack))
        rejectBtn  <- slack.registerButton[ServiceVersion]("reject-deploy")(onReject(slack))
        deployForm <- slack.registerForm[DeployForm, String]("deploy-form")(onDeploySubmit(slack, approveBtn, rejectBtn))
        // /deploy [service] → opens a form, pre-populating service name if provided
        _          <- slack.registerCommand[String]("deploy", "Deploy a service") { cmd =>
                        val initial = {
                          val base = InitialValues.of[DeployForm]
                          if (cmd.args.trim.nonEmpty) base.set(_.service, cmd.args.trim) else base
                        }
                        slack
                          .openForm(cmd.triggerId, deployForm, "Deploy Service", initialValues = initial)
                          .as(CommandResponse.Silent)
                      }
        // /status <service> → typed command with CommandParser[ServiceName]
        _          <- slack.registerCommand[ServiceName]("service-status", "Check service status") { cmd =>
                        IO.pure(CommandResponse.Ephemeral(s"Service *${cmd.args}* is healthy."))
                      }
        // /scale <service> <replicas> → derived case class CommandParser
        _          <- slack.registerCommand[ScaleArgs]("scale", "Scale a service") { cmd =>
                        IO.pure(CommandResponse.Ephemeral(s"Scaling *${cmd.args.service}* to *${cmd.args.replicas}* replicas."))
                      }
        _          <- slack.validateSetup("ChatOps4sExample", "slack-manifest.yml")
        _          <- slack.start(token, Some(appToken))
      } yield ()
    }
  }

  // Form submission → approval message with typed ServiceVersion buttons
  private def onDeploySubmit(
      slack: SlackGateway[IO],
      approveBtn: ButtonId[ServiceVersion],
      rejectBtn: ButtonId[ServiceVersion],
  )(submission: FormSubmission[DeployForm, String]): IO[Unit] = {
    val form      = submission.values
    val dryRunStr = if (form.dryRun) " (dry run)" else ""
    val label     = s"*${form.service}* *${form.version}*$dryRunStr"
    val sv        = ServiceVersion(form.service, form.version)
    for {
      msg  <- slack.send(channel, s"${submission.userId.mention} requested deploy of $label")
      _    <- slack.addReaction(msg, "hourglass_flowing_sand")
      resp <- slack.reply(
                msg,
                s"Approve deployment of $label?",
                Seq(
                  approveBtn.render("Approve", sv),
                  rejectBtn.render("Reject", sv),
                ),
              )
      _     = println(s"Deploy request reply: $resp")
    } yield ()
  }

  // Button handlers receive ServiceVersion — can destructure safely
  private def onApprove(slack: SlackGateway[IO])(click: ButtonClick[ServiceVersion]): IO[Unit] =
    click.threadId.traverse_ { parent =>
      val (service, version) = ServiceVersion.unapply(click.value)
      for {
        _ <- slack.update(click.messageId, s":white_check_mark: *Approved* by ${click.userId.mention}")
        _ <- slack.reply(parent, s"Deploying *$service* *$version* to production...")
        _ <- IO.sleep(3.seconds)
        _ <- slack.reply(parent, s"Deploy of *$service* *$version* completed.")
        _ <- slack.removeReaction(parent, "hourglass_flowing_sand")
        _ <- slack.addReaction(parent, "white_check_mark")
      } yield ()
    }

  private def onReject(slack: SlackGateway[IO])(click: ButtonClick[ServiceVersion]): IO[Unit] =
    click.threadId.traverse_ { parent =>
      for {
        _ <- slack.update(click.messageId, s":x: *Rejected* by ${click.userId.mention}")
        _ <- slack.removeReaction(parent, "hourglass_flowing_sand")
        _ <- slack.addReaction(parent, "x")
      } yield ()
    }

  // -- Typed button values: buttons carry a ServiceVersion so handlers are guaranteed the format
  opaque type ServiceVersion <: String = String

  object ServiceVersion {
    def apply(service: String, version: String): ServiceVersion = s"$service@$version"

    def unapply(sv: ServiceVersion): (String, String) = {
      val Array(s, v) = sv.split("@", 2): @unchecked
      (s, v)
    }
  }

  // -- Typed command argument: /status only accepts known service names
  opaque type ServiceName <: String = String

  object ServiceName {
    private val valid = Set("api", "web", "worker")

    def parse(text: String): Either[String, ServiceName] = {
      val name = text.trim.toLowerCase
      if (valid.contains(name)) Right(name)
      else Left(s"Unknown service '$text'. Valid: ${valid.mkString(", ")}")
    }
  }

  given CommandParser[ServiceName] with {
    def parse(text: String): Either[String, ServiceName] = ServiceName.parse(text)

    override def usageHint: String = "[service name]"
  }

  // -- Typed multi-arg command: /scale splits "api 3" into service + replica count
  given CommandArgCodec[ServiceName] with {
    def parse(text: String): Either[String, ServiceName] = ServiceName.parse(text)

    def show(value: ServiceName): String = value
  }

  case class ScaleArgs(service: ServiceName, replicas: Int) derives CommandParser

  // -- Form for the /deploy modal
  case class DeployForm(service: String, version: String, dryRun: Boolean) derives FormDef
}
