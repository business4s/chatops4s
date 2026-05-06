package example.docs

import cats.effect.IO
import chatops4s.slack.{ButtonId, SlackGateway, SlackSetup}

private object ButtonsPage {

  // start_register_button
  def registerButton(slack: SlackGateway[IO] & SlackSetup[IO]): IO[ButtonId[String]] =
    slack.registerButton[String]("approve") { click =>
      slack.update(click.messageId, s"Approved by ${click.userId.mention}").void
    }
  // end_register_button

  // start_render_buttons
  def renderButtons(
      slack: SlackGateway[IO],
      channel: String,
      approveBtn: ButtonId[String],
      rejectBtn: ButtonId[String],
  ): IO[Unit] =
    slack
      .send(
        channel,
        "Deploy v1.2.3 to production?",
        Seq(
          approveBtn.render("Approve", "v1.2.3"),
          rejectBtn.render("Reject", "v1.2.3"),
        ),
      )
      .void
  // end_render_buttons

  // start_constrained_type
  opaque type Environment <: String = String
  object Environment {
    val Production: Environment = "production"
    val Staging: Environment    = "staging"
  }
  // end_constrained_type

  // start_constrained_usage
  def constrainedButtons(slack: SlackGateway[IO] & SlackSetup[IO], channel: String): IO[Unit] =
    for {
      deployBtn <- slack.registerButton[Environment]("deploy-env") { click =>
                     // click.value is guaranteed to be a valid Environment
                     slack.update(click.messageId, s"Deploying to ${click.value}...").void
                   }
      _         <- slack.send(
                     channel,
                     "Where to deploy?",
                     Seq(
                       deployBtn.render("Production", Environment.Production),
                       deployBtn.render("Staging", Environment.Staging),
                     ),
                   )
    } yield ()
  // end_constrained_usage

  // start_remove_buttons
  def removeButtons(slack: SlackGateway[IO] & SlackSetup[IO]): IO[ButtonId[String]] =
    slack.registerButton[String]("approve-and-remove") { click =>
      // update replaces the message, removing buttons
      slack.update(click.messageId, s"Approved by ${click.userId.mention}").void
    }
  // end_remove_buttons
}
