package chatops4s.slack.api

import io.circe.Codec
import org.scalatest.{Canceled, Outcome}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.*
import sttp.client4.circe.*
import sttp.shared.Identity

// Required bot scopes: chat:write, chat:write.public, reactions:write, channels:join
// Required app-level scope: connections:write
// The bot must be a member of SLACK_CHANNEL (the setup step joins automatically).
class SlackApiE2ETest extends AnyFreeSpec with Matchers {

  // CI passes these as empty strings when secrets are unavailable (e.g. fork PRs) — treat blank as absent
  private val botToken = sys.env.get("SLACK_BOT_TOKEN").filter(_.nonEmpty).map(SlackBotToken.unsafe)
  private val appToken = sys.env.get("SLACK_APP_TOKEN").filter(_.nonEmpty).map(SlackAppToken.unsafe)
  private val channel  = sys.env.get("SLACK_CHANNEL").filter(_.nonEmpty)

  override def withFixture(test: NoArgTest): Outcome =
    if (botToken.isEmpty || channel.isEmpty)
      Canceled("SLACK_BOT_TOKEN and SLACK_CHANNEL required")
    else
      super.withFixture(test)

  private lazy val backend = DefaultSyncBackend()
  private lazy val api     = new SlackApi[Identity](backend, botToken.get)

  "SlackApi E2E" - {

    // Populated by postMessage, used by subsequent tests
    var channelId: Option[ChannelId] = None
    var messageTs: Option[Timestamp] = None

    "apps.connections.open should return a websocket URL" in {
      assume(appToken.isDefined, "SLACK_APP_TOKEN required")
      val resp   = SlackAppApi[Identity](backend, appToken.get).apps.connections.open()
      val result = resp.okOrThrow
      info(s"connections.open url: ${result.url}")
      result.url.should(startWith("wss://"))
    }

    "chat.postMessage should post a message" in {
      val resp   = api.chat.postMessage(
        chat.PostMessageRequest(
          channel = channel.get,
          text = "chatops4s-slack-client E2E test message",
        ),
      )
      val result = resp.okOrThrow
      info(s"postMessage response: channel=${result.channel}, ts=${result.ts}")
      channelId = Some(result.channel)
      messageTs = Some(result.ts)
    }

    "setup: join channel" in {
      // reactions require channel membership; chat:write.public posts without joining
      // conversations.join needs a channel ID, so this runs after postMessage resolves it
      assume(channelId.isDefined, "postMessage must run first")
      val resp = basicRequest
        .post(uri"https://slack.com/api/conversations.join")
        .header("Authorization", s"Bearer ${botToken.get.value}")
        .contentType("application/json")
        .body(s"""{"channel":"${channelId.get.value}"}""")
        .response(asJsonAlways[OkErrorResponse])
        .send(backend)
        .body
      info(s"conversations.join response: $resp")
      resp match {
        case Right(r)  =>
          withClue(s"Slack error: ${r.error.getOrElse("none")}") {
            r.ok.shouldBe(true)
          }
        case Left(err) => fail(s"Failed to parse response: $err")
      }
    }

    "chat.update should update the message" in {
      assume(messageTs.isDefined, "postMessage must run first")
      val resp   = api.chat.update(
        chat.UpdateRequest(
          channel = channelId.get,
          ts = messageTs.get,
          text = Some("chatops4s-slack-client E2E test message (updated)"),
        ),
      )
      val result = resp.okOrThrow
      info(s"update response: channel=${result.channel}, ts=${result.ts}")
    }

    "reactions.add should add a reaction" in {
      assume(messageTs.isDefined, "postMessage must run first")
      // okOrThrow is sufficient — AddResponse is empty
      api.reactions
        .add(
          reactions.AddRequest(
            channel = channelId.get,
            timestamp = messageTs.get,
            name = "white_check_mark",
          ),
        )
        .okOrThrow
      info("reactions.add succeeded")
    }

    "reactions.remove should remove the reaction" in {
      assume(messageTs.isDefined, "postMessage must run first")
      api.reactions
        .remove(
          reactions.RemoveRequest(
            channel = channelId.get,
            timestamp = messageTs.get,
            name = "white_check_mark",
          ),
        )
        .okOrThrow
      info("reactions.remove succeeded")
    }

    "chat.postEphemeral should send an ephemeral message" in {
      assume(messageTs.isDefined, "postMessage must run first")
      val userId = sys.env.get("SLACK_TEST_USER_ID")
      assume(userId.isDefined, "SLACK_TEST_USER_ID required for ephemeral test")
      val result = api.chat
        .postEphemeral(
          chat.PostEphemeralRequest(
            channel = channelId.get.value,
            user = UserId(userId.get),
            text = "chatops4s-slack-client E2E ephemeral test",
          ),
        )
        .okOrThrow
      info(s"postEphemeral response: message_ts=${result.message_ts}")
    }

    "chat.delete should delete the message" in {
      assume(messageTs.isDefined, "postMessage must run first")
      val result = api.chat
        .delete(
          chat.DeleteRequest(
            channel = channelId.get,
            ts = messageTs.get,
          ),
        )
        .okOrThrow
      info(s"delete response: channel=${result.channel}, ts=${result.ts}")
    }
  }

  private case class OkErrorResponse(ok: Boolean, error: Option[String] = None) derives Codec.AsObject
}
