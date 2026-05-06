package example

import cats.effect.{IO, IOApp}
import chatops4s.slack.{ButtonClick, CommandResponse, FormDef, FormId, FormSubmission, InitialValues, MessageId, MetadataCodec, SlackGateway, Url}
import chatops4s.slack.api.{ChannelId, ConversationId, Email, SlackAppToken, SlackBotToken, Timestamp, UserId}
import chatops4s.slack.api.blocks.{RichTextBlock, RichTextSection, RichTextText}
import sttp.client4.httpclient.fs2.HttpClientFs2Backend

import java.time.{Instant, LocalDate, LocalTime}

object AllInputs extends IOApp.Simple {

  private val channel  = sys.env.getOrElse("SLACK_CHANNEL", "#testing-slack-app")
  private val token    = SlackBotToken.unsafe(sys.env.getOrElse("SLACK_BOT_TOKEN", "xoxb-your-token"))
  private val appToken = SlackAppToken.unsafe(sys.env.getOrElse("SLACK_APP_TOKEN", "xapp-your-app-token"))

  override def run: IO[Unit] =
    HttpClientFs2Backend.resource[IO]().use { backend =>
      for {
        slack        <- SlackGateway.create(backend)
        form         <- slack.registerForm[AllInputsForm, Option[MessageId]]("all-inputs-form")(onSubmit(slack))
        openBtn      <- slack.registerButton[String]("open-all-inputs") { click =>
                          slack.openForm(click.triggerId, form, "All Inputs", Some(click.messageId))
                        }
        prefilledBtn <- slack.registerButton[String]("open-all-inputs-prefilled")(openPrefilled(slack, form, _))
        _            <- slack.registerCommand[String]("all-inputs", "Open all-inputs form") { cmd =>
                          slack.openForm(cmd.triggerId, form, "All Inputs", None).as(CommandResponse.Silent)
                        }
        _            <- slack.validateSetup("AllInputs", "/tmp/slack-manifest.yml")
        fiber        <- slack.start(token, Some(appToken)).start
        _            <- slack.send(
                          channel,
                          "Try all available form inputs:",
                          Seq(openBtn.render("Open Form"), prefilledBtn.render("Open Prefilled")),
                        )
        _            <- fiber.joinWithNever
      } yield ()
    }

  private def openPrefilled(slack: SlackGateway[IO], form: FormId[AllInputsForm, Option[MessageId]], click: ButtonClick[String]): IO[Unit] = {
    for {
      initialValues <- prefilled(slack, click)
      _             <- slack.openForm(click.triggerId, form, "All Inputs (Prefilled)", Some(click.messageId), initialValues = initialValues)
    } yield ()
  }

  private def onSubmit(slack: SlackGateway[IO])(submission: FormSubmission[AllInputsForm, Option[MessageId]]): IO[Unit] = {
    val f       = submission.values
    val lines   = List(
      s"*Text:* ${f.text.getOrElse("_empty_")}",
      s"*Integer:* ${f.integer.map(_.toString).getOrElse("_empty_")}",
      s"*Decimal:* ${f.decimal.map(_.toString).getOrElse("_empty_")}",
      s"*Checkbox:* ${f.checkbox}",
      s"*Email:* ${f.email.map(_.value).getOrElse("_empty_")}",
      s"*URL:* ${f.url.map(_.value).getOrElse("_empty_")}",
      s"*Date:* ${f.date.map(_.toString).getOrElse("_empty_")}",
      s"*Time:* ${f.time.map(_.toString).getOrElse("_empty_")}",
      s"*Datetime:* ${f.datetime.map(_.toString).getOrElse("_empty_")}",
      s"*User:* ${f.user.map(_.mention).getOrElse("_empty_")}",
      s"*Users:* ${f.users.filter(_.nonEmpty).map(_.map(_.mention).mkString(", ")).getOrElse("_empty_")}",
      s"*Channel:* ${f.channel.map(_.mention).getOrElse("_empty_")}",
      s"*Channels:* ${f.channels.filter(_.nonEmpty).map(_.map(_.mention).mkString(", ")).getOrElse("_empty_")}",
      s"*Conversation:* ${f.conversation.map(_.value).getOrElse("_empty_")}",
      s"*Conversations:* ${f.conversations.filter(_.nonEmpty).map(_.map(_.value).mkString(", ")).getOrElse("_empty_")}",
      s"*Rich text:* ${f.richText.map(_.elements.mkString(", ")).getOrElse("_empty_")}",
    )
    val message = s"${submission.userId.mention} submitted:\n${lines.mkString("\n")}"
    submission.metadata match {
      case Some(parentMsg) => slack.reply(parentMsg, message).void
      case None            => slack.send(channel, message).void
    }
  }

  case class AllInputsForm(
      text: Option[String],
      integer: Option[Int],
      decimal: Option[Double],
      checkbox: Boolean,
      email: Option[Email],
      url: Option[Url],
      date: Option[LocalDate],
      time: Option[LocalTime],
      datetime: Option[Instant],
      user: Option[UserId],
      users: Option[List[UserId]],
      channel: Option[ChannelId],
      channels: Option[List[ChannelId]],
      conversation: Option[ConversationId],
      conversations: Option[List[ConversationId]],
      richText: Option[RichTextBlock],
  ) derives FormDef

  private given MetadataCodec[Option[MessageId]] with {
    def encode(value: Option[MessageId]): String =
      value.fold("")(msg => s"${msg.channel.value}:${msg.ts.value}")
    def decode(raw: String): Option[MessageId]   =
      if (raw.isEmpty) None
      else {
        val Array(ch, ts) = raw.split(":", 2): @unchecked
        Some(MessageId(ChannelId(ch), Timestamp(ts)))
      }
  }

  private def prefilled(slack: SlackGateway[IO], click: ButtonClick[String]): IO[InitialValues[AllInputsForm]] =
    for {
      userInfo <- slack.getUserInfo(click.userId)
    } yield InitialValues
      .of[AllInputsForm]
      .set(_.text, Some("Hello world"))
      .set(_.integer, Some(42))
      .set(_.decimal, Some(3.14))
      .set(_.checkbox, true)
      .set(_.email, userInfo.profile.flatMap(_.email))
      .set(_.url, Some(Url("https://example.com")))
      .set(_.date, Some(LocalDate.of(2025, 6, 15)))
      .set(_.time, Some(LocalTime.of(14, 30)))
      .set(_.datetime, Some(Instant.ofEpochSecond(1700000000L)))
      .set(_.user, Some(click.userId))
      .set(_.users, Some(List(click.userId)))
      .set(_.channel, Some(click.messageId.channel))
      .set(_.channels, Some(List(click.messageId.channel)))
      .set(_.conversation, Some(ConversationId(click.messageId.channel.value)))
      .set(_.conversations, Some(List(ConversationId(click.messageId.channel.value))))
      .set(
        _.richText,
        Some(
          RichTextBlock(elements =
            List(
              RichTextSection(elements = List(RichTextText("Hello, this is prefilled rich text!"))),
            ),
          ),
        ),
      )
}
