package example.docs

import cats.effect.IO
import chatops4s.slack.{CommandResponse, FieldCodec, FormDef, FormId, InitialValues, SlackGateway, SlackSetup}
import chatops4s.slack.api.blocks.{BlockOption, PlainTextObject}

private object FormsPage {

  // start_form_definition
  case class DeployForm(service: String, version: String, dryRun: Boolean) derives FormDef
  // end_form_definition

  def formOperations(slack: SlackGateway[IO] & SlackSetup[IO], channel: String): IO[Unit] =
    for {
      // start_form_open
      deployForm <- slack.registerForm[DeployForm, String]("deploy-form") { submission =>
                      val form = submission.values
                      slack.send(channel, s"Deploying ${form.service} ${form.version}").void
                    }
      _          <- slack.registerCommand[String]("deploy-form", "Open deployment form") { cmd =>
                      slack
                        .openForm(cmd.triggerId, deployForm, "Deploy Service")
                        .as(CommandResponse.Silent)
                    }
      // end_form_open
    } yield ()

  // start_initial_values
  def withInitialValues(
      slack: SlackGateway[IO],
      triggerId: chatops4s.slack.api.TriggerId,
      formId: FormId[DeployForm, String],
  ): IO[Unit] = {
    val initial = InitialValues
      .of[DeployForm]
      .set(_.service, "api-gateway")
      .set(_.version, "1.0.0")
      .set(_.dryRun, true)
    slack.openForm(triggerId, formId, "Deploy Service", initialValues = initial)
  }
  // end_initial_values

  // start_form_metadata
  def withMetadata(
      slack: SlackGateway[IO] & SlackSetup[IO],
      channel: String,
  ): IO[Unit] = {
    for {
      deployForm <- slack.registerForm[DeployForm, String]("deploy-form") { submission =>
                      val meta = submission.metadata // your metadata string
                      val form = submission.values
                      slack.send(channel, s"[$meta] Deploying ${form.service}").void
                    }
      _          <- slack.registerCommand[String]("deploy-form", "Open deployment form") { cmd =>
                      slack
                        .openForm(cmd.triggerId, deployForm, "Deploy", s"${cmd.channelId}:requested")
                        .as(CommandResponse.Silent)
                    }
    } yield ()
  }
  // end_form_metadata

  // start_static_select
  enum Environment { case Staging, Production }

  given FieldCodec[Environment] = FieldCodec.staticSelect(
    List(
      BlockOption(PlainTextObject("Staging"), "staging")       -> Environment.Staging,
      BlockOption(PlainTextObject("Production"), "production") -> Environment.Production,
    ),
  )

  case class DeployFormWithEnv(service: String, environment: Environment) derives FormDef
  // end_static_select
}
