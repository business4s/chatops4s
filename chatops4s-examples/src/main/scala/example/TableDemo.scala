package example

import cats.effect.{ExitCode, IO, IOApp}
import chatops4s.slack.api.{SlackApi, SlackBotToken, chat}
import chatops4s.slack.api.blocks.{
  HeaderBlock,
  PlainTextObject,
  RichTextBlock,
  RichTextEmoji,
  RichTextLink,
  RichTextSection,
  RichTextStyle,
  RichTextText,
  TableBlock,
  TableCellAlign,
  TableColumnSetting,
}
import sttp.client4.httpclient.fs2.HttpClientFs2Backend

// Posts a sample TableBlock to Slack so the rendered output can be eyeballed.
// Requires SLACK_BOT_TOKEN and SLACK_CHANNEL (channel id or name like "#testing-slack-app").
object TableDemo extends IOApp {

  private val channel = sys.env.getOrElse("SLACK_CHANNEL", "#testing-slack-app")
  private val token   = SlackBotToken.unsafe(sys.env.getOrElse("SLACK_BOT_TOKEN", "xoxb-your-token"))

  override def run(args: List[String]): IO[ExitCode] =
    HttpClientFs2Backend.resource[IO]().use { backend =>
      val api = new SlackApi[IO](backend, token)

      val table = TableBlock(
        block_id = Some("deployments_table"),
        column_settings = Some(
          List(
            TableColumnSetting(),
            TableColumnSetting(align = Some(TableCellAlign.Center)),
            TableColumnSetting(align = Some(TableCellAlign.Right), is_wrapped = Some(true)),
            TableColumnSetting(align = Some(TableCellAlign.Center)),
          ),
        ),
        rows = List(
          List("Service", "Version", "Notes", "Status"),
          List(
            "api",
            "v1.2.3",
            RichTextBlock(elements =
              List(
                RichTextSection(elements =
                  List(
                    RichTextText("Hotfix for "),
                    RichTextText("auth", style = Some(RichTextStyle(bold = Some(true)))),
                    RichTextText(" — see "),
                    RichTextLink(url = "https://example.com/incident-42", text = Some("INC-42")),
                  ),
                ),
              ),
            ),
            RichTextBlock(elements = List(RichTextSection(elements = List(RichTextEmoji(name = "white_check_mark"), RichTextText(" healthy"))))),
          ),
          List(
            "web",
            "v4.0.0",
            RichTextBlock(elements =
              List(
                RichTextSection(elements = List(RichTextText("Routine bump", style = Some(RichTextStyle(italic = Some(true)))))),
              ),
            ),
            RichTextBlock(elements = List(RichTextSection(elements = List(RichTextEmoji(name = "large_yellow_circle"), RichTextText(" deploying"))))),
          ),
          List(
            "worker",
            "v0.9.1",
            RichTextBlock(elements =
              List(
                RichTextSection(elements =
                  List(
                    RichTextText("Rolled back, see "),
                    RichTextLink(url = "https://example.com/rollback", text = Some("post-mortem")),
                  ),
                ),
              ),
            ),
            RichTextBlock(elements = List(RichTextSection(elements = List(RichTextEmoji(name = "x"), RichTextText(" rolled back"))))),
          ),
        ),
      )

      val blocks = List(
        HeaderBlock(text = PlainTextObject("Deployment Status")),
        table,
      )

      val req = chat.PostMessageRequest(
        channel = channel,
        text = "Deployment Status (table preview)",
        blocks = Some(blocks),
      )

      api.chat
        .postMessage(req)
        .flatMap { resp =>
          val r = resp.okOrThrow
          IO.println(s"Posted table to ${r.channel.value} ts=${r.ts.value}")
        }
        .as(ExitCode.Success)
    }
}
