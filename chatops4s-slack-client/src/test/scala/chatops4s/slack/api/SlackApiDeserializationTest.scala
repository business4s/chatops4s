package chatops4s.slack.api

import chatops4s.slack.api.manifest.*
import io.circe.Decoder
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util.Using

class SlackApiDeserializationTest extends AnyFreeSpec with Matchers {

  private def loadFixture(name: String): Option[String] =
    Option(getClass.getResourceAsStream(s"/responses/$name")).map { is =>
      Using.resource(Source.fromInputStream(is))(_.mkString)
    }

  private def parseOk[T: Decoder](name: String)(checks: T => Unit): Unit = {
    val json   = loadFixture(name)
    assume(json.isDefined, s"Fixture $name not found — run ResponseCollector first")
    val result = decode[SlackResponse[T]](json.get)
    result match {
      case Right(SlackResponse.Ok(value)) => checks(value)
      case Right(err: SlackResponse.Err)  => fail(s"Expected Ok but got $err")
      case Left(err)                      => fail(s"Failed to decode: $err")
    }
  }

  private def parseOkInline[T: Decoder](json: String)(checks: T => Unit): Unit = {
    val result = decode[SlackResponse[T]](json)
    result match {
      case Right(SlackResponse.Ok(value)) => checks(value)
      case Right(err: SlackResponse.Err)  => fail(s"Expected Ok but got $err")
      case Left(err)                      => fail(s"Failed to decode: $err")
    }
  }

  "Deserialization of real Slack responses" - {

    "chat.postMessage" in {
      parseOk[chat.PostMessageResponse]("chat.postMessage.json") { r =>
        r.channel.value should not be empty
        r.ts.value should not be empty
      }
    }

    "chat.update" in {
      parseOk[chat.UpdateResponse]("chat.update.json") { r =>
        r.channel.value should not be empty
        r.ts.value should not be empty
      }
    }

    "chat.delete" in {
      parseOk[chat.DeleteResponse]("chat.delete.json") { r =>
        r.channel.value should not be empty
        r.ts.value should not be empty
      }
    }

    "chat.postEphemeral" in {
      parseOk[chat.PostEphemeralResponse]("chat.postEphemeral.json") { r =>
        r.message_ts.value should not be empty
      }
    }

    "reactions.add" in {
      parseOk[reactions.AddResponse]("reactions.add.json") { _ => /* AddResponse is empty */ }
    }

    "reactions.remove" in {
      parseOk[reactions.RemoveResponse]("reactions.remove.json") { _ => /* RemoveResponse is empty */ }
    }

    "views.open" in {
      parseOk[views.OpenResponse]("views.open.json") { r =>
        r.view shouldBe defined
        r.view.get.hcursor.get[String]("id").toOption shouldBe Some("VMHU10V25")
      }
    }

    "apps.connections.open" in {
      parseOk[apps.ConnectionsOpenResponse]("apps.connections.open.json") { r =>
        r.url should startWith("wss://")
      }
    }

    "error response parses as Err" in {
      val json   = loadFixture("error.json")
      assume(json.isDefined, "Fixture error.json not found — run ResponseCollector first")
      val result = decode[SlackResponse[chat.DeleteResponse]](json.get)
      result match {
        case Right(SlackResponse.Err(error, _, _)) =>
          error should not be empty
        case Right(SlackResponse.Ok(_))            =>
          fail("Expected Err but got Ok")
        case Left(err)                             =>
          fail(s"Failed to decode: $err")
      }
    }

    "okOrThrow works on success" in {
      val json     = loadFixture("chat.postMessage.json")
      assume(json.isDefined, "Fixture chat.postMessage.json not found — run ResponseCollector first")
      val response = decode[SlackResponse[chat.PostMessageResponse]](json.get).toTry.get
      val value    = response.okOrThrow
      value.channel.value should not be empty
      value.ts.value should not be empty
    }

    "okOrThrow throws SlackApiError on error" in {
      val json     = loadFixture("error.json")
      assume(json.isDefined, "Fixture error.json not found — run ResponseCollector first")
      val response = decode[SlackResponse[chat.DeleteResponse]](json.get).toTry.get
      val ex       = intercept[SlackApiError] {
        response.okOrThrow
      }
      ex.error should not be empty
    }

    "invalid_blocks error prefers response_metadata.messages over errors" in {
      val json     =
        """{
          |  "ok": false,
          |  "error": "invalid_blocks",
          |  "errors": ["must define either `text` or `fields` [json-pointer:/blocks/0/type]"],
          |  "response_metadata": {
          |    "messages": ["[ERROR] must define either `text` or `fields` [json-pointer:/blocks/0/type]"]
          |  }
          |}""".stripMargin
      val response = decode[SlackResponse[chat.PostMessageResponse]](json).toTry.get
      val ex       = intercept[SlackApiError] {
        response.okOrThrow
      }
      ex.error shouldBe "invalid_blocks"
      ex.details shouldBe List("[ERROR] must define either `text` or `fields` [json-pointer:/blocks/0/type]")
      ex.response shouldBe defined
      ex.getMessage should include("invalid_blocks")
      ex.getMessage should include("[json-pointer:/blocks/0/type]")
    }

    "invalid_blocks error falls back to errors when response_metadata.messages missing or empty" in {
      val missing  =
        """{
          |  "ok": false,
          |  "error": "invalid_blocks",
          |  "errors": ["must define either `text` or `fields` [json-pointer:/blocks/0/type]"]
          |}""".stripMargin
      val response = decode[SlackResponse[chat.PostMessageResponse]](missing).toTry.get
      val ex       = intercept[SlackApiError] {
        response.okOrThrow
      }
      ex.details shouldBe List("must define either `text` or `fields` [json-pointer:/blocks/0/type]")

      val empty         =
        """{
          |  "ok": false,
          |  "error": "invalid_blocks",
          |  "errors": ["must define either `text` or `fields` [json-pointer:/blocks/0/type]"],
          |  "response_metadata": {"messages": []}
          |}""".stripMargin
      val responseEmpty = decode[SlackResponse[chat.PostMessageResponse]](empty).toTry.get
      val exEmpty       = intercept[SlackApiError] {
        responseEmpty.okOrThrow
      }
      exEmpty.details shouldBe List("must define either `text` or `fields` [json-pointer:/blocks/0/type]")
    }
  }

  // Examples based on Slack API documentation
  "Deserialization of Slack doc examples" - {

    // https://docs.slack.dev/reference/methods/chat.postMessage
    "chat.postMessage" in {
      parseOkInline[chat.PostMessageResponse](
        """{
          |  "ok": true,
          |  "channel": "C123456",
          |  "ts": "1503435956.000247",
          |  "message": {
          |    "text": "Here's a message for you",
          |    "username": "ecto1",
          |    "bot_id": "B19LU7CSY",
          |    "attachments": [],
          |    "type": "message",
          |    "subtype": "bot_message",
          |    "ts": "1503435956.000247"
          |  }
          |}""".stripMargin,
      ) { r =>
        r.channel shouldBe ChannelId("C123456")
        r.ts shouldBe Timestamp("1503435956.000247")
        r.message shouldBe defined
      }
    }

    // https://docs.slack.dev/reference/methods/chat.update
    "chat.update" in {
      parseOkInline[chat.UpdateResponse](
        """{
          |  "ok": true,
          |  "channel": "C123456",
          |  "ts": "1401383885.000061",
          |  "text": "Updated text you carefully authored",
          |  "message": {
          |    "text": "Updated text you carefully authored",
          |    "user": "U34567"
          |  }
          |}""".stripMargin,
      ) { r =>
        r.channel shouldBe ChannelId("C123456")
        r.ts shouldBe Timestamp("1401383885.000061")
        r.text shouldBe Some("Updated text you carefully authored")
        r.message shouldBe defined
      }
    }

    // https://docs.slack.dev/reference/methods/chat.delete
    "chat.delete" in {
      parseOkInline[chat.DeleteResponse](
        """{
          |  "ok": true,
          |  "channel": "C123456",
          |  "ts": "1401383885.000061"
          |}""".stripMargin,
      ) { r =>
        r.channel shouldBe ChannelId("C123456")
        r.ts shouldBe Timestamp("1401383885.000061")
      }
    }

    // https://docs.slack.dev/reference/methods/chat.postEphemeral
    "chat.postEphemeral" in {
      parseOkInline[chat.PostEphemeralResponse](
        """{
          |  "ok": true,
          |  "message_ts": "1502210682.580145"
          |}""".stripMargin,
      ) { r =>
        r.message_ts shouldBe Timestamp("1502210682.580145")
      }
    }

    // https://docs.slack.dev/reference/methods/reactions.add
    "reactions.add" in {
      parseOkInline[reactions.AddResponse]("""{"ok": true}""") { _ => /* empty response */ }
    }

    // https://docs.slack.dev/reference/methods/reactions.remove
    "reactions.remove" in {
      parseOkInline[reactions.RemoveResponse]("""{"ok": true}""") { _ => /* empty response */ }
    }

    // https://docs.slack.dev/reference/methods/views.open
    "views.open" in {
      parseOkInline[views.OpenResponse](
        """{
          |  "ok": true,
          |  "view": {
          |    "id": "VMHU10V25",
          |    "team_id": "T8N4K1JN",
          |    "type": "modal",
          |    "bot_id": "BA0G7HC02",
          |    "app_id": "A21SDS90",
          |    "title": {
          |      "type": "plain_text",
          |      "text": "Quite a plain modal"
          |    },
          |    "submit": {
          |      "type": "plain_text",
          |      "text": "Create"
          |    },
          |    "close": {
          |      "type": "plain_text",
          |      "text": "Cancel"
          |    },
          |    "blocks": [
          |      {
          |        "type": "input",
          |        "block_id": "a_block_id",
          |        "label": {
          |          "type": "plain_text",
          |          "text": "A simple label",
          |          "emoji": true
          |        },
          |        "optional": false,
          |        "dispatch_action": false,
          |        "element": {
          |          "type": "plain_text_input",
          |          "action_id": "an_action_id",
          |          "dispatch_action_config": {
          |            "trigger_actions_on": ["on_enter_pressed"]
          |          }
          |        }
          |      }
          |    ],
          |    "private_metadata": "",
          |    "callback_id": "view_identifier_12",
          |    "state": {
          |      "values": {}
          |    },
          |    "hash": "156772938.1827394",
          |    "clear_on_close": false,
          |    "notify_on_close": false,
          |    "root_view_id": "VMHU10V25",
          |    "external_id": "",
          |    "app_installed_team_id": "T8N4K1JN"
          |  }
          |}""".stripMargin,
      ) { r =>
        r.view shouldBe defined
        r.view.get.hcursor.get[String]("id").toOption shouldBe Some("VMHU10V25")
        r.view.get.hcursor.get[String]("type").toOption shouldBe Some("modal")
      }
    }

    // https://docs.slack.dev/reference/methods/apps.connections.open
    "apps.connections.open" in {
      parseOkInline[apps.ConnectionsOpenResponse](
        """{
          |  "ok": true,
          |  "url": "wss://wss-primary.slack.com/link/?ticket=example-ticket&app_id=A123456"
          |}""".stripMargin,
      ) { r =>
        r.url should startWith("wss://")
      }
    }

    // https://docs.slack.dev/reference/methods/conversations.history
    "conversations.history" in {
      parseOkInline[conversations.HistoryResponse](
        """{
          |  "ok": true,
          |  "messages": [
          |    {
          |      "type": "message",
          |      "user": "U123ABC456",
          |      "text": "I find you punny and would like to smell your daisy",
          |      "ts": "1512085950.000216"
          |    },
          |    {
          |      "type": "message",
          |      "user": "U222BBB222",
          |      "text": "What, you want to see my caused a stir?",
          |      "ts": "1512104434.000490"
          |    }
          |  ],
          |  "has_more": true,
          |  "response_metadata": {
          |    "messages": ["something"]
          |  }
          |}""".stripMargin,
      ) { r =>
        r.messages.size shouldBe 2
        r.messages.head.text shouldBe Some("I find you punny and would like to smell your daisy")
        r.messages.head.ts shouldBe Some(Timestamp("1512085950.000216"))
        r.has_more shouldBe Some(true)
      }
    }

    "conversations.history with metadata" in {
      parseOkInline[conversations.HistoryResponse](
        """{
          |  "ok": true,
          |  "messages": [
          |    {
          |      "type": "message",
          |      "user": "U123",
          |      "text": "Hello",
          |      "ts": "1512085950.000216",
          |      "metadata": {
          |        "event_type": "chatops4s_idempotency",
          |        "event_payload": {"key": "deploy-123"}
          |      }
          |    }
          |  ]
          |}""".stripMargin,
      ) { r =>
        r.messages.size shouldBe 1
        r.messages.head.metadata shouldBe defined
        val meta = r.messages.head.metadata.get
        meta.hcursor.downField("event_type").as[String] shouldBe Right("chatops4s_idempotency")
        meta.hcursor.downField("event_payload").downField("key").as[String] shouldBe Right("deploy-123")
      }
    }

    // https://docs.slack.dev/reference/methods/conversations.replies
    "conversations.replies" in {
      parseOkInline[conversations.RepliesResponse](
        """{
          |  "ok": true,
          |  "messages": [
          |    {
          |      "type": "message",
          |      "user": "U123ABC456",
          |      "text": "Original message",
          |      "thread_ts": "1512085950.000216",
          |      "reply_count": 1,
          |      "ts": "1512085950.000216"
          |    },
          |    {
          |      "type": "message",
          |      "user": "U222BBB222",
          |      "text": "A reply",
          |      "thread_ts": "1512085950.000216",
          |      "ts": "1512085999.000300"
          |    }
          |  ],
          |  "has_more": false
          |}""".stripMargin,
      ) { r =>
        r.messages.size shouldBe 2
        r.messages(1).text shouldBe Some("A reply")
        r.messages(1).thread_ts shouldBe Some(Timestamp("1512085950.000216"))
        r.has_more shouldBe Some(false)
      }
    }
  }

  "Manifest serde" - {

    // Based on the Java SDK's AppManifestTest fixture
    "decode comprehensive manifest JSON" in {
      val json =
        """{
          |  "_metadata": {
          |    "major_version": 1,
          |    "minor_version": 1
          |  },
          |  "display_information": {
          |    "name": "manifest-sandbox",
          |    "description": "A test app",
          |    "long_description": "A longer description of the test app",
          |    "background_color": "#2c2d30"
          |  },
          |  "features": {
          |    "app_home": {
          |      "home_tab_enabled": true,
          |      "messages_tab_enabled": false,
          |      "messages_tab_read_only_enabled": false
          |    },
          |    "bot_user": {
          |      "display_name": "manifest-sandbox",
          |      "always_online": true
          |    },
          |    "shortcuts": [
          |      {
          |        "name": "message one",
          |        "type": "message",
          |        "callback_id": "m",
          |        "description": "message"
          |      },
          |      {
          |        "name": "global one",
          |        "type": "global",
          |        "callback_id": "g",
          |        "description": "global"
          |      }
          |    ],
          |    "slash_commands": [
          |      {
          |        "command": "/hey",
          |        "url": "https://www.example.com/",
          |        "description": "What's up?",
          |        "usage_hint": "What's up?",
          |        "should_escape": true
          |      }
          |    ],
          |    "unfurl_domains": [
          |      "example.com"
          |    ]
          |  },
          |  "oauth_config": {
          |    "redirect_urls": [
          |      "https://www.example.com/foo"
          |    ],
          |    "scopes": {
          |      "user": [
          |        "search:read",
          |        "channels:read",
          |        "groups:read",
          |        "mpim:read"
          |      ],
          |      "bot": [
          |        "commands",
          |        "incoming-webhook",
          |        "app_mentions:read",
          |        "links:read"
          |      ]
          |    },
          |    "token_management_enabled": false
          |  },
          |  "settings": {
          |    "allowed_ip_address_ranges": [
          |      "123.123.123.123/32"
          |    ],
          |    "event_subscriptions": {
          |      "request_url": "https://www.example.com/slack/events",
          |      "user_events": [
          |        "member_joined_channel"
          |      ],
          |      "bot_events": [
          |        "app_mention",
          |        "link_shared"
          |      ]
          |    },
          |    "interactivity": {
          |      "is_enabled": true,
          |      "request_url": "https://www.example.com/",
          |      "message_menu_options_url": "https://www.example.com/"
          |    },
          |    "org_deploy_enabled": true,
          |    "socket_mode_enabled": false,
          |    "token_rotation_enabled": true,
          |    "siws_links": {
          |      "initiate_uri": "https://www.example.com/siws"
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.display_information.name shouldBe "manifest-sandbox"
          m.display_information.description shouldBe Some("A test app")
          m.display_information.background_color shouldBe Some("#2c2d30")
          m._metadata.major_version shouldBe Some(1)
          m.features.app_home.get.home_tab_enabled shouldBe Some(true)
          m.features.bot_user.get.display_name shouldBe "manifest-sandbox"
          m.features.bot_user.get.always_online shouldBe Some(true)
          m.features.shortcuts.get should have size 2
          m.features.shortcuts.get.head.name shouldBe "message one"
          m.features.slash_commands.get should have size 1
          m.features.slash_commands.get.head.command shouldBe "/hey"
          m.features.slash_commands.get.head.should_escape shouldBe Some(true)
          m.features.unfurl_domains.get shouldBe List("example.com")
          m.oauth_config.scopes.get.bot.get should contain("commands")
          m.oauth_config.scopes.get.user.get should contain("search:read")
          m.oauth_config.redirect_urls.get shouldBe List("https://www.example.com/foo")
          m.oauth_config.token_management_enabled shouldBe Some(false)
          m.settings.allowed_ip_address_ranges.get shouldBe List("123.123.123.123/32")
          m.settings.event_subscriptions.get.bot_events.get shouldBe List("app_mention", "link_shared")
          m.settings.event_subscriptions.get.user_events.get shouldBe List("member_joined_channel")
          m.settings.interactivity.get.is_enabled shouldBe Some(true)
          m.settings.org_deploy_enabled shouldBe Some(true)
          m.settings.socket_mode_enabled shouldBe Some(false)
          m.settings.token_rotation_enabled shouldBe Some(true)
          m.settings.siws_links.get.initiate_uri shouldBe Some("https://www.example.com/siws")
      }
    }

    "decode manifest with minimal populated sections" in {
      val json =
        """{
          |  "_metadata": {
          |    "major_version": 2,
          |    "minor_version": 0
          |  },
          |  "display_information": {
          |    "name": "my-app"
          |  },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {}
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.display_information.name shouldBe "my-app"
          m._metadata.major_version shouldBe Some(2)
          m.features shouldBe Features()
          m.oauth_config shouldBe OauthConfig()
          m.settings shouldBe ManifestSettings()
      }
    }

    "tolerate unknown fields from API" in {
      val json =
        """{
          |  "_metadata": {
          |    "major_version": 1,
          |    "minor_version": 1
          |  },
          |  "display_information": {
          |    "name": "test-app"
          |  },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {
          |    "hermes_app_type": "remote",
          |    "socket_mode_enabled": true
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.display_information.name shouldBe "test-app"
          m.settings.socket_mode_enabled shouldBe Some(true)
      }
    }

    "decode functions with nested parameter properties and choices" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "fn-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "functions": {
          |    "greeting_fn": {
          |      "title": "Greeting",
          |      "description": "Send a greeting",
          |      "input_parameters": {
          |        "properties": {
          |          "recipient": {
          |            "type": "slack#/types/user_id",
          |            "title": "Recipient",
          |            "description": "Who to greet",
          |            "is_required": true
          |          },
          |          "style": {
          |            "type": "string",
          |            "title": "Style",
          |            "choices": [
          |              { "value": "formal", "title": "Formal" },
          |              { "value": "casual", "title": "Casual", "description": "Laid-back tone" }
          |            ]
          |          }
          |        },
          |        "required": ["recipient"]
          |      },
          |      "output_parameters": {
          |        "properties": {
          |          "message": {
          |            "type": "string",
          |            "title": "Message"
          |          }
          |        }
          |      }
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.functions shouldBe defined
          val fn        = m.functions.get("greeting_fn")
          fn.title shouldBe "Greeting"
          fn.input_parameters.properties should have size 2
          fn.input_parameters.required shouldBe Some(List("recipient"))
          val recipient = fn.input_parameters.properties("recipient")
          recipient.`type` shouldBe "slack#/types/user_id"
          recipient.is_required shouldBe Some(true)
          val style     = fn.input_parameters.properties("style")
          style.choices shouldBe defined
          style.choices.get should have size 2
          style.choices.get.head.title shouldBe "Formal"
          fn.output_parameters.properties should have size 1
      }
    }

    "decode workflows with steps and triggers" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "wf-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "workflows": {
          |    "my_workflow": {
          |      "title": "My Workflow",
          |      "description": "A sample workflow",
          |      "input_parameters": {
          |        "properties": {
          |          "channel": { "type": "slack#/types/channel_id" }
          |        }
          |      },
          |      "steps": [
          |        {
          |          "id": "0",
          |          "function_id": "greeting_fn",
          |          "inputs": { "recipient": "{{inputs.channel}}" },
          |          "type": "custom"
          |        }
          |      ],
          |      "suggested_triggers": [
          |        {
          |          "name": "on message",
          |          "description": "Triggered by message",
          |          "type": "message_posted",
          |          "inputs": { "channel_id": "C12345" }
          |        }
          |      ]
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.workflows shouldBe defined
          val wf = m.workflows.get("my_workflow")
          wf.title shouldBe "My Workflow"
          wf.steps should have size 1
          wf.steps.head.function_id shouldBe "greeting_fn"
          wf.steps.head.`type` shouldBe Some("custom")
          wf.suggested_triggers shouldBe defined
          wf.suggested_triggers.get.head.`type` shouldBe "message_posted"
          wf.input_parameters shouldBe defined
      }
    }

    "decode datastores with attributes" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "ds-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "datastores": {
          |    "tasks": {
          |      "primary_key": "task_id",
          |      "attributes": {
          |        "task_id": { "type": "string" },
          |        "title": { "type": "string" },
          |        "tags": { "type": "array", "items": { "type": "string" } }
          |      },
          |      "time_to_live_attribute": "expires_at"
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.datastores shouldBe defined
          val ds = m.datastores.get("tasks")
          ds.primary_key shouldBe "task_id"
          ds.attributes should have size 3
          ds.attributes("task_id").`type` shouldBe "string"
          ds.time_to_live_attribute shouldBe Some("expires_at")
      }
    }

    "decode external_auth_providers with identity_config" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "auth-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "external_auth_providers": [
          |    {
          |      "provider_type": "custom",
          |      "options": {
          |        "client_id": "abc123",
          |        "provider_name": "My IdP",
          |        "authorization_url": "https://idp.example.com/auth",
          |        "token_url": "https://idp.example.com/token",
          |        "scope": ["openid", "profile"],
          |        "use_pkce": true,
          |        "identity_config": {
          |          "url": "https://idp.example.com/userinfo",
          |          "account_identifier": "$.sub",
          |          "http_method_type": "GET"
          |        },
          |        "token_url_config": {
          |          "use_basic_auth_scheme": true
          |        }
          |      }
          |    }
          |  ]
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.external_auth_providers shouldBe defined
          val p = m.external_auth_providers.get.head
          p.provider_type shouldBe "custom"
          p.options.client_id shouldBe "abc123"
          p.options.scope shouldBe Some(List("openid", "profile"))
          p.options.use_pkce shouldBe Some(true)
          p.options.identity_config shouldBe defined
          p.options.identity_config.get.url shouldBe "https://idp.example.com/userinfo"
          p.options.identity_config.get.account_identifier shouldBe "$.sub"
          p.options.token_url_config shouldBe defined
          p.options.token_url_config.get.use_basic_auth_scheme shouldBe Some(true)
      }
    }

    "decode features.assistant_view with suggested_prompts" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "assistant-app" },
          |  "oauth_config": {},
          |  "settings": {},
          |  "features": {
          |    "assistant_view": {
          |      "assistant_description": "A helpful bot",
          |      "suggested_prompts": [
          |        { "title": "Help", "message": "How can I help?" },
          |        { "title": "Status", "message": "What is the status?" }
          |      ]
          |    },
          |    "rich_previews": {
          |      "is_active": true,
          |      "entity_types": ["link", "file"]
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.features.assistant_view shouldBe defined
          m.features.assistant_view.get.assistant_description shouldBe Some("A helpful bot")
          m.features.assistant_view.get.suggested_prompts shouldBe defined
          m.features.assistant_view.get.suggested_prompts.get should have size 2
          m.features.assistant_view.get.suggested_prompts.get.head.title shouldBe "Help"
          m.features.rich_previews shouldBe defined
          m.features.rich_previews.get.is_active shouldBe Some(true)
          m.features.rich_previews.get.entity_types shouldBe Some(List("link", "file"))
      }
    }

    "decode compliance and app_directory" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "compliant-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "compliance": {
          |    "fedramp_authorization": "moderate"
          |  },
          |  "app_directory": {
          |    "app_directory_categories": ["productivity", "developer-tools"],
          |    "use_direct_install": true,
          |    "direct_install_url": "https://example.com/install",
          |    "privacy_policy_url": "https://example.com/privacy",
          |    "support_email": "support@example.com",
          |    "supported_languages": ["en", "fr"],
          |    "pricing": "free"
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.compliance shouldBe defined
          m.compliance.get.fedramp_authorization shouldBe Some("moderate")
          m.app_directory shouldBe defined
          m.app_directory.get.app_directory_categories shouldBe Some(List("productivity", "developer-tools"))
          m.app_directory.get.use_direct_install shouldBe Some(true)
          m.app_directory.get.privacy_policy_url shouldBe Some("https://example.com/privacy")
          m.app_directory.get.support_email shouldBe Some("support@example.com")
          m.app_directory.get.supported_languages shouldBe Some(List("en", "fr"))
          m.app_directory.get.pricing shouldBe Some("free")
      }
    }

    "decode settings.is_hosted and event_subscriptions.metadata_subscriptions" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "hosted-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {
          |    "is_hosted": true,
          |    "event_subscriptions": {
          |      "bot_events": ["app_mention"],
          |      "metadata_subscriptions": [
          |        { "app_id": "A111", "event_type": "my_event" }
          |      ]
          |    }
          |  }
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.settings.is_hosted shouldBe Some(true)
          m.settings.event_subscriptions shouldBe defined
          val es = m.settings.event_subscriptions.get
          es.metadata_subscriptions shouldBe defined
          es.metadata_subscriptions.get should have size 1
          es.metadata_subscriptions.get.head.app_id shouldBe "A111"
          es.metadata_subscriptions.get.head.event_type shouldBe "my_event"
      }
    }

    "decode custom types and metadata_events" in {
      val json =
        """{
          |  "_metadata": { "major_version": 1, "minor_version": 1 },
          |  "display_information": { "name": "types-app" },
          |  "features": {},
          |  "oauth_config": {},
          |  "settings": {},
          |  "types": {
          |    "my_object": {
          |      "type": "object",
          |      "title": "My Object",
          |      "properties": {
          |        "name": { "type": "string", "description": "Object name" }
          |      }
          |    }
          |  },
          |  "metadata_events": [
          |    {
          |      "type": "my_event_type",
          |      "title": "My Event",
          |      "properties": {
          |        "key": { "type": "string" }
          |      },
          |      "required": ["key"]
          |    }
          |  ]
          |}""".stripMargin

      val result = decode[SlackAppManifest](json)
      result match {
        case Left(err) => fail(s"Failed to decode: $err")
        case Right(m)  =>
          m.types shouldBe defined
          val ct = m.types.get("my_object")
          ct.`type` shouldBe "object"
          ct.title shouldBe Some("My Object")
          ct.properties shouldBe defined
          ct.properties.get("name").`type` shouldBe "string"

          m.metadata_events shouldBe defined
          m.metadata_events.get should have size 1
          m.metadata_events.get.head.`type` shouldBe "my_event_type"
          m.metadata_events.get.head.required shouldBe Some(List("key"))
      }
    }

    "round-trip encode then decode" in {
      val original = SlackAppManifest(
        display_information = DisplayInformation(
          name = "round-trip-app",
          description = Some("test"),
        ),
        features = Features(
          bot_user = Some(BotUser(display_name = "round-trip-app", always_online = Some(true))),
          slash_commands = Some(
            List(
              SlashCommand(command = "/deploy", description = "Deploy", usage_hint = Some("[env]")),
            ),
          ),
        ),
        oauth_config = OauthConfig(
          scopes = Some(OauthScopes(bot = Some(List("chat:write", "commands")))),
        ),
        settings = ManifestSettings(
          socket_mode_enabled = Some(true),
          interactivity = Some(Interactivity(is_enabled = Some(true))),
          event_subscriptions = Some(EventSubscriptions(bot_events = Some(List("app_mention")))),
        ),
        outgoing_domains = Some(List("example.com")),
      )

      val json    = original.asJson.deepDropNullValues
      val decoded = json.as[SlackAppManifest]

      decoded match {
        case Left(err) => fail(s"Round-trip decode failed: $err")
        case Right(m)  =>
          m.display_information shouldBe original.display_information
          m.features.bot_user shouldBe original.features.bot_user
          m.features.slash_commands shouldBe original.features.slash_commands
          m.oauth_config shouldBe original.oauth_config
          m.settings.socket_mode_enabled shouldBe original.settings.socket_mode_enabled
          m.settings.interactivity shouldBe original.settings.interactivity
          m.settings.event_subscriptions shouldBe original.settings.event_subscriptions
          m.outgoing_domains shouldBe original.outgoing_domains
      }
    }

    "round-trip with new manifest sections" in {
      val original = SlackAppManifest(
        display_information = DisplayInformation(name = "full-app"),
        functions = Some(
          Map(
            "greet" -> ManifestFunction(
              title = "Greet",
              description = "Send greeting",
              input_parameters = ParameterSet(
                properties = Map("name" -> ParameterProperty(`type` = "string", title = Some("Name"))),
                required = Some(List("name")),
              ),
              output_parameters = ParameterSet(
                properties = Map("msg" -> ParameterProperty(`type` = "string")),
              ),
            ),
          ),
        ),
        datastores = Some(
          Map(
            "items" -> Datastore(
              primary_key = "id",
              attributes = Map("id" -> DatastoreAttribute(`type` = "string")),
            ),
          ),
        ),
        compliance = Some(Compliance(fedramp_authorization = Some("moderate"))),
        app_directory = Some(AppDirectory(pricing = Some("free"))),
        features = Features(
          assistant_view = Some(
            AssistantView(
              assistant_description = Some("Helper"),
              suggested_prompts = Some(List(SuggestedPrompt("Hi", "Hello"))),
            ),
          ),
        ),
      )

      val json    = original.asJson.deepDropNullValues
      val decoded = json.as[SlackAppManifest]

      decoded match {
        case Left(err) => fail(s"Round-trip decode failed: $err")
        case Right(m)  =>
          m.functions shouldBe defined
          m.functions.get("greet").title shouldBe "Greet"
          m.functions.get("greet").input_parameters.required shouldBe Some(List("name"))
          m.datastores shouldBe defined
          m.datastores.get("items").primary_key shouldBe "id"
          m.compliance.get.fedramp_authorization shouldBe Some("moderate")
          m.app_directory.get.pricing shouldBe Some("free")
          m.features.assistant_view.get.assistant_description shouldBe Some("Helper")
          m.features.assistant_view.get.suggested_prompts.get should have size 1
      }
    }

    "export response with functions" in {
      parseOkInline[apps.manifest.ExportResponse](
        """{
          |  "ok": true,
          |  "manifest": {
          |    "_metadata": { "major_version": 1, "minor_version": 1 },
          |    "display_information": { "name": "fn-export-app" },
          |    "features": {},
          |    "oauth_config": {},
          |    "functions": {
          |      "my_fn": {
          |        "title": "My Function",
          |        "description": "Does things",
          |        "input_parameters": {
          |          "properties": {
          |            "channel": { "type": "slack#/types/channel_id" }
          |          },
          |          "required": ["channel"]
          |        },
          |        "output_parameters": {
          |          "properties": {
          |            "result": { "type": "string" }
          |          }
          |        }
          |      }
          |    },
          |    "settings": {
          |      "function_runtime": "remote"
          |    }
          |  }
          |}""".stripMargin,
      ) { r =>
        r.manifest shouldBe defined
        val m = r.manifest.get
        m.functions shouldBe defined
        m.functions.get("my_fn").title shouldBe "My Function"
        m.functions.get("my_fn").input_parameters.required shouldBe Some(List("channel"))
        m.settings.function_runtime shouldBe Some("remote")
      }
    }
  }

  "Config API response serde" - {

    "apps.manifest.create" in {
      parseOkInline[apps.manifest.CreateResponse](
        """{
          |  "ok": true,
          |  "app_id": "A12345",
          |  "credentials": {
          |    "client_id": "12345.6789",
          |    "client_secret": "secret123",
          |    "verification_token": "vtoken",
          |    "signing_secret": "ssecret"
          |  },
          |  "oauth_authorize_url": "https://slack.com/oauth/v2/authorize?client_id=12345.6789&scope=commands"
          |}""".stripMargin,
      ) { r =>
        r.app_id shouldBe "A12345"
        r.credentials shouldBe defined
        r.credentials.get.client_id shouldBe "12345.6789"
        r.credentials.get.client_secret shouldBe "secret123"
        r.credentials.get.verification_token shouldBe "vtoken"
        r.credentials.get.signing_secret shouldBe "ssecret"
        r.oauth_authorize_url shouldBe defined
      }
    }

    "apps.manifest.update" in {
      parseOkInline[apps.manifest.UpdateResponse](
        """{
          |  "ok": true,
          |  "app_id": "A12345",
          |  "permissions_updated": true
          |}""".stripMargin,
      ) { r =>
        r.app_id shouldBe Some("A12345")
        r.permissions_updated shouldBe Some(true)
      }
    }

    "apps.manifest.validate with no errors" in {
      parseOkInline[apps.manifest.ValidateResponse](
        """{
          |  "ok": true
          |}""".stripMargin,
      ) { r =>
        r.errors shouldBe None
      }
    }

    "apps.manifest.validate with errors" in {
      parseOkInline[apps.manifest.ValidateResponse](
        """{
          |  "ok": true,
          |  "errors": [
          |    {
          |      "code": "invalid_manifest",
          |      "message": "display_information.name is required",
          |      "pointer": "/display_information/name"
          |    }
          |  ]
          |}""".stripMargin,
      ) { r =>
        r.errors shouldBe defined
        r.errors.get should have size 1
        r.errors.get.head.message shouldBe "display_information.name is required"
        r.errors.get.head.code shouldBe Some("invalid_manifest")
        r.errors.get.head.pointer shouldBe Some("/display_information/name")
      }
    }

    "apps.manifest.export" in {
      parseOkInline[apps.manifest.ExportResponse](
        """{
          |  "ok": true,
          |  "manifest": {
          |    "_metadata": {
          |      "major_version": 1,
          |      "minor_version": 1
          |    },
          |    "display_information": {
          |      "name": "exported-app"
          |    },
          |    "features": {
          |      "bot_user": {
          |        "display_name": "exported-app",
          |        "always_online": true
          |      }
          |    },
          |    "oauth_config": {
          |      "scopes": {
          |        "bot": ["chat:write"]
          |      }
          |    },
          |    "settings": {
          |      "socket_mode_enabled": true
          |    }
          |  }
          |}""".stripMargin,
      ) { r =>
        r.manifest shouldBe defined
        r.manifest.get.display_information.name shouldBe "exported-app"
        r.manifest.get.features.bot_user.get.always_online shouldBe Some(true)
        r.manifest.get.oauth_config.scopes.get.bot.get shouldBe List("chat:write")
      }
    }

    "apps.manifest.delete" in {
      parseOkInline[apps.manifest.DeleteResponse](
        """{"ok": true}""",
      ) { _ => /* DeleteResponse is empty */ }
    }

    "oauth.v2.access" in {
      parseOkInline[oauth.AccessResponse](
        """{
          |  "ok": true,
          |  "access_token": "xoxb-17653672481-19874698323-pdFZKVeTuE8sk7oOcBrzbqgy",
          |  "token_type": "bot",
          |  "scope": "chat:write,chat:write.public",
          |  "bot_user_id": "U0KRQLJ9H",
          |  "app_id": "A0KRD7HC3",
          |  "team": {
          |    "name": "Slack Softball Team",
          |    "id": "T9TK3CUKW"
          |  }
          |}""".stripMargin,
      ) { r =>
        r.access_token shouldBe "xoxb-17653672481-19874698323-pdFZKVeTuE8sk7oOcBrzbqgy"
        r.token_type shouldBe Some("bot")
        r.scope shouldBe Some("chat:write,chat:write.public")
        r.bot_user_id shouldBe Some("U0KRQLJ9H")
        r.app_id shouldBe Some("A0KRD7HC3")
        r.team shouldBe defined
        r.team.get.name shouldBe Some("Slack Softball Team")
        r.team.get.id shouldBe Some("T9TK3CUKW")
      }
    }

    "oauth.v2.access with minimal response" in {
      parseOkInline[oauth.AccessResponse](
        """{
          |  "ok": true,
          |  "access_token": "xoxp-user-token"
          |}""".stripMargin,
      ) { r =>
        r.access_token shouldBe "xoxp-user-token"
        r.token_type shouldBe None
        r.team shouldBe None
      }
    }

    "tooling.tokens.rotate" in {
      parseOkInline[tooling.tokens.RotateResponse](
        """{
          |  "ok": true,
          |  "token": "xoxe.xoxp-new-token",
          |  "refresh_token": "xoxe-new-refresh",
          |  "team_id": "T12345",
          |  "user_id": "U12345",
          |  "iat": 1700000000,
          |  "exp": 1700043200
          |}""".stripMargin,
      ) { r =>
        r.token shouldBe SlackConfigToken.unsafe("xoxe.xoxp-new-token")
        r.refresh_token shouldBe SlackRefreshToken.unsafe("xoxe-new-refresh")
        r.team_id shouldBe Some("T12345")
        r.user_id shouldBe Some("U12345")
        r.iat shouldBe Some(1700000000)
        r.exp shouldBe Some(1700043200)
      }
    }
  }
}
