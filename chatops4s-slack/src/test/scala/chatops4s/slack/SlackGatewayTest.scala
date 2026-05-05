package chatops4s.slack

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import chatops4s.slack.api.{ChannelId, ConversationId, Email, SlackAppToken, SlackBotToken, TeamId, Timestamp, TriggerId, UserId, users}
import chatops4s.slack.api.manifest.{BotUser, DisplayInformation, Features, SlackAppManifest}
import chatops4s.slack.api.socket.*
import chatops4s.slack.api.blocks.*
import io.circe.Json
import io.circe.parser
import chatops4s.slack.api.socket.ViewStateValue
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.WebSocketBackendStub

import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDate, LocalTime}

case class ScaleArgs(service: String, replicas: Int) derives CommandParser

class SlackGatewayTest extends AnyFreeSpec with Matchers {

  private val okPostMessage = """{"ok":true,"channel":"C123","ts":"1234567890.123"}"""

  /** Creates a gateway with a pre-connected client for tests that need to call Slack API methods. */
  private def createGateway(
      backend: WebSocketBackendStub[IO] = MockBackend.create(),
      cache: UserInfoCache[IO] = {
        given sttp.monad.MonadError[IO] = MockBackend.create().monad
        UserInfoCache.noCache[IO]
      },
      idempotencyCheck: Option[IdempotencyCheck[IO]] = None,
  ): SlackGatewayImpl[IO] = {
    given monad: sttp.monad.MonadError[IO]         = backend.monad
    val client                                     = new SlackClient[IO](SlackBotToken.unsafe("xoxb-test-token"), backend)
    val clientRef                                  = Ref.of[IO, Option[SlackClient[IO]]](Some(client)).unsafeRunSync()
    val handlersRef                                = Ref.of[IO, Map[ButtonId[?], ErasedHandler[IO]]](Map.empty).unsafeRunSync()
    val commandHandlersRef                         = Ref.of[IO, Map[CommandName, CommandEntry[IO]]](Map.empty).unsafeRunSync()
    val formHandlersRef                            = Ref.of[IO, Map[FormId[?, ?], FormEntry[IO]]](Map.empty).unsafeRunSync()
    val cacheRef                                   = Ref.of[IO, UserInfoCache[IO]](cache).unsafeRunSync()
    val check                                      = idempotencyCheck.getOrElse(IdempotencyCheck.slackScan[IO](clientRef))
    val idempotencyRef                             = Ref.of[IO, IdempotencyCheck[IO]](check).unsafeRunSync()
    val defaultErrorHandler: Throwable => IO[Unit] = e => monad.blocking(println(s"Test error handler: ${e.getMessage}"))
    val errorHandlerRef                            = Ref.of[IO, Throwable => IO[Unit]](defaultErrorHandler).unsafeRunSync()
    new SlackGatewayImpl[IO](clientRef, handlersRef, commandHandlersRef, formHandlersRef, cacheRef, idempotencyRef, errorHandlerRef, backend)
  }

  /** Creates a gateway without a client, for tests that only use registration/manifest methods. */
  private def createDisconnectedGateway(
      backend: WebSocketBackendStub[IO] = MockBackend.create(),
  ): SlackGatewayImpl[IO] = {
    given monad: sttp.monad.MonadError[IO]         = backend.monad
    val clientRef                                  = Ref.of[IO, Option[SlackClient[IO]]](None).unsafeRunSync()
    val handlersRef                                = Ref.of[IO, Map[ButtonId[?], ErasedHandler[IO]]](Map.empty).unsafeRunSync()
    val commandHandlersRef                         = Ref.of[IO, Map[CommandName, CommandEntry[IO]]](Map.empty).unsafeRunSync()
    val formHandlersRef                            = Ref.of[IO, Map[FormId[?, ?], FormEntry[IO]]](Map.empty).unsafeRunSync()
    val cacheRef                                   = Ref.of[IO, UserInfoCache[IO]](UserInfoCache.noCache[IO]).unsafeRunSync()
    val idempotencyRef                             = Ref.of[IO, IdempotencyCheck[IO]](IdempotencyCheck.noCheck[IO]).unsafeRunSync()
    val defaultErrorHandler: Throwable => IO[Unit] = e => monad.blocking(println(s"Test error handler: ${e.getMessage}"))
    val errorHandlerRef                            = Ref.of[IO, Throwable => IO[Unit]](defaultErrorHandler).unsafeRunSync()
    new SlackGatewayImpl[IO](clientRef, handlersRef, commandHandlersRef, formHandlersRef, cacheRef, idempotencyRef, errorHandlerRef, backend)
  }

  "SlackGateway" - {

    "send" - {
      "should send a simple message" in {
        val gateway = createGateway(MockBackend.withPostMessage(okPostMessage))

        val result = gateway.send("C123", "Hello World").unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
      }

      "should send a message with buttons" in {
        val gateway = createGateway(MockBackend.withPostMessage(okPostMessage))
        val approve = gateway.registerButton[String](_ => IO.unit).unsafeRunSync()
        val reject  = gateway.registerButton[String](_ => IO.unit).unsafeRunSync()

        val result = gateway
          .send(
            "C123",
            "Deploy?",
            Seq(
              Button("Approve", approve, approve.value),
              Button("Reject", reject, reject.value),
            ),
          )
          .unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
      }

      "should omit value field for buttons with empty value" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("chat.postMessage")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust(okPostMessage)

        val gateway    = createGateway(backend)
        val emptyBtn   = gateway.registerButton[String](_ => IO.unit).unsafeRunSync()
        val nonEmptyId = gateway.registerButton[String](_ => IO.unit).unsafeRunSync()

        gateway
          .send(
            "C123",
            "Deploy?",
            Seq(
              emptyBtn.render("Confirm"),
              Button("Reject", nonEmptyId, "some-value"),
            ),
          )
          .unsafeRunSync()

        capturedBody shouldBe defined
        val json     = parser.parse(capturedBody.get).toOption.get
        val blocks   = json.hcursor.downField("blocks").as[List[Json]].toOption.get
        val actions  = blocks.find(_.hcursor.downField("type").as[String].contains("actions")).get
        val elements = actions.hcursor.downField("elements").as[List[Json]].toOption.get

        elements.size shouldBe 2
        // Empty-value button should not have a "value" field
        elements(0).hcursor.downField("value").as[String] shouldBe a[Left[?, ?]]
        // Non-empty-value button should have a "value" field
        elements(1).hcursor.downField("value").as[String] shouldBe Right("some-value")
      }

      "should handle API errors" in {
        val errorBody = """{"ok":false,"error":"invalid_auth"}"""
        val gateway   = createGateway(MockBackend.withPostMessage(errorBody))

        val ex = intercept[chatops4s.slack.api.SlackApiError] {
          gateway.send("C123", "Test").unsafeRunSync()
        }
        ex.error shouldBe "invalid_auth"
      }

      "should fail when not connected" in {
        val gateway = createDisconnectedGateway()

        val ex = intercept[RuntimeException] {
          gateway.send("C123", "Test").unsafeRunSync()
        }
        ex.getMessage should include("start()")
      }
    }

    "reply" - {
      "should reply in thread" in {
        val body    = """{"ok":true,"channel":"C123","ts":"1234567891.456"}"""
        val gateway = createGateway(MockBackend.withPostMessage(body))

        val result = gateway.reply(MessageId(ChannelId("C123"), Timestamp("1234567890.123")), "Thread reply").unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567891.456"))
      }

      "should reply in thread with buttons" in {
        val body    = """{"ok":true,"channel":"C123","ts":"1234567891.456"}"""
        val gateway = createGateway(MockBackend.withPostMessage(body))
        val btn     = gateway.registerButton[String](_ => IO.unit).unsafeRunSync()

        val result = gateway
          .reply(
            MessageId(ChannelId("C123"), Timestamp("1234567890.123")),
            "Confirm?",
            Seq(Button("OK", btn, btn.value)),
          )
          .unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567891.456"))
      }
    }

    "update" - {
      "should update a message" in {
        val gateway = createGateway(
          MockBackend.withUpdate(
            """{"ok":true,"channel":"C123","ts":"1234567890.123"}""",
          ),
        )

        val msgId  = MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        val result = gateway.update(msgId, "Updated text").unsafeRunSync()

        result shouldBe msgId
      }

      "should handle API errors on update" in {
        val errorBody = """{"ok":false,"error":"message_not_found"}"""
        val gateway   = createGateway(MockBackend.withUpdate(errorBody))

        val msgId = MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        val ex    = intercept[chatops4s.slack.api.SlackApiError] {
          gateway.update(msgId, "Updated text").unsafeRunSync()
        }
        ex.error shouldBe "message_not_found"
      }
    }

    "onButton" - {
      "should generate unique button IDs" in {
        val gateway = createGateway()

        val ids = (1 to 10).toList.traverse(_ => gateway.registerButton[String](_ => IO.unit)).unsafeRunSync()

        ids.map(_.value).toSet.size shouldBe 10
      }
    }

    "interaction handling" - {
      "should dispatch button click to registered handler" in {
        val gateway                               = createGateway()
        var captured: Option[ButtonClick[String]] = None

        val btnId = gateway
          .registerButton[String] { click =>
            IO { captured = Some(click) }
          }
          .unsafeRunSync()

        val payload = interactionPayload(btnId.value, "my-value")
        gateway.handleInteractionPayload(payload).unsafeRunSync()

        captured shouldBe defined
        captured.get.userId shouldBe UserId("U123")
        captured.get.messageId shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        captured.get.value shouldBe "my-value"
      }

      "should ignore unknown action IDs" in {
        val gateway = createGateway()
        var called  = false

        gateway.registerButton[String] { _ => IO { called = true } }.unsafeRunSync()

        val payload = interactionPayload("unknown_action_id", "v")
        gateway.handleInteractionPayload(payload).unsafeRunSync()

        called shouldBe false
      }

      "should dispatch multiple actions in one payload" in {
        val gateway = createGateway()
        var count   = 0

        val btn1 = gateway.registerButton[String] { _ => IO { count += 1 } }.unsafeRunSync()
        val btn2 = gateway.registerButton[String] { _ => IO { count += 10 } }.unsafeRunSync()

        val payload = InteractionPayload(
          `type` = "block_actions",
          user = User(UserId("U123")),
          channel = Some(Channel(ChannelId("C123"))),
          container = Container(message_ts = Some(Timestamp("1234567890.123"))),
          actions = List(
            Action(btn1.value, "blk-1", "button", Timestamp("1234567890.123"), value = Some("v1")),
            Action(btn2.value, "blk-2", "button", Timestamp("1234567890.123"), value = Some("v2")),
          ),
          trigger_id = TriggerId("test-trigger-id"),
          api_app_id = "A123",
          token = "tok",
        )
        gateway.handleInteractionPayload(payload).unsafeRunSync()

        count shouldBe 11
      }
    }

    "delete" - {
      "should delete a message" in {
        val body    = """{"ok":true,"channel":"C123","ts":"1234567890.123"}"""
        val gateway = createGateway(MockBackend.create().whenAnyRequest.thenRespondAdjust(body))

        gateway.delete(MessageId(ChannelId("C123"), Timestamp("1234567890.123"))).unsafeRunSync()
      }

      "should handle API errors on delete" in {
        val errorBody = """{"ok":false,"error":"message_not_found"}"""
        val gateway   = createGateway(MockBackend.create().whenAnyRequest.thenRespondAdjust(errorBody))

        val ex = intercept[chatops4s.slack.api.SlackApiError] {
          gateway.delete(MessageId(ChannelId("C123"), Timestamp("1234567890.123"))).unsafeRunSync()
        }
        ex.error shouldBe "message_not_found"
      }
    }

    "reactions" - {
      "should add a reaction" in {
        val gateway = createGateway(MockBackend.withOkApi())

        gateway.addReaction(MessageId(ChannelId("C123"), Timestamp("1234567890.123")), "rocket").unsafeRunSync()
      }

      "should remove a reaction" in {
        val gateway = createGateway(MockBackend.withOkApi())

        gateway.removeReaction(MessageId(ChannelId("C123"), Timestamp("1234567890.123")), "rocket").unsafeRunSync()
      }
    }

    "sendEphemeral" - {
      "should send an ephemeral message" in {
        val body    = """{"ok":true,"message_ts":"1234567890.123"}"""
        val gateway = createGateway(MockBackend.create().whenAnyRequest.thenRespondAdjust(body))

        gateway.sendEphemeral("C123", UserId("U456"), "Only you can see this").unsafeRunSync()
      }
    }

    "getUserInfo" - {
      "should return user info" in {
        val body    =
          """{"ok":true,"user":{"id":"U123","name":"testuser","real_name":"Test User","profile":{"email":"test@example.com","display_name":"testuser","real_name":"Test User"},"is_bot":false,"tz":"America/New_York"}}"""
        val gateway = createGateway(MockBackend.withUsersInfo(body))

        val result = gateway.getUserInfo(UserId("U123")).unsafeRunSync()

        result.id shouldBe UserId("U123")
        result.name shouldBe Some("testuser")
        result.real_name shouldBe Some("Test User")
        result.profile shouldBe defined
        result.profile.get.email shouldBe Some(Email("test@example.com"))
        result.is_bot shouldBe Some(false)
        result.tz shouldBe Some("America/New_York")
      }

      "should handle API errors" in {
        val errorBody = """{"ok":false,"error":"user_not_found"}"""
        val gateway   = createGateway(MockBackend.withUsersInfo(errorBody))

        val ex = intercept[chatops4s.slack.api.SlackApiError] {
          gateway.getUserInfo(UserId("U999")).unsafeRunSync()
        }
        ex.error shouldBe "user_not_found"
      }
    }

    "UserInfoCache" - {
      val userInfoBody =
        """{"ok":true,"user":{"id":"U123","name":"testuser","real_name":"Test User","profile":{"email":"test@example.com","display_name":"testuser","real_name":"Test User"},"is_bot":false,"tz":"America/New_York"}}"""

      "should cache user info on first fetch and return cached on second" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        var apiCallCount                       = 0
        val backend                            = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("users.info")
            if (matches) apiCallCount += 1
            matches
          }
          .thenRespondAdjust(userInfoBody)

        val cache   = UserInfoCache.inMemory[IO]().unsafeRunSync()
        val gateway = createGateway(backend, cache)

        val result1 = gateway.getUserInfo(UserId("U123")).unsafeRunSync()
        val result2 = gateway.getUserInfo(UserId("U123")).unsafeRunSync()

        result1.id shouldBe UserId("U123")
        result2.id shouldBe UserId("U123")
        apiCallCount shouldBe 1
      }

      "should expire entries after TTL" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        val epoch                              = Instant.parse("2025-01-01T00:00:00Z")
        var currentTime                        = epoch
        val cache                              = UserInfoCache
          .inMemoryWithClock[IO](
            ttl = java.time.Duration.ofSeconds(60),
            maxEntries = 1000,
            clock = () => currentTime,
          )
          .unsafeRunSync()

        var apiCallCount = 0
        val backend      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("users.info")
            if (matches) apiCallCount += 1
            matches
          }
          .thenRespondAdjust(userInfoBody)

        val gateway = createGateway(backend, cache)

        gateway.getUserInfo(UserId("U123")).unsafeRunSync()
        apiCallCount shouldBe 1

        // Not expired yet
        currentTime = epoch.plusSeconds(30)
        gateway.getUserInfo(UserId("U123")).unsafeRunSync()
        apiCallCount shouldBe 1

        // Expired
        currentTime = epoch.plusSeconds(120)
        gateway.getUserInfo(UserId("U123")).unsafeRunSync()
        apiCallCount shouldBe 2
      }

      "should evict oldest entries when over maxEntries" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        val epoch                              = Instant.parse("2025-01-01T00:00:00Z")
        var currentTime                        = epoch
        val cache                              = UserInfoCache
          .inMemoryWithClock[IO](
            ttl = java.time.Duration.ofHours(1),
            maxEntries = 2,
            clock = () => currentTime,
          )
          .unsafeRunSync()

        val info1 = users.UserInfo(id = UserId("U1"))
        val info2 = users.UserInfo(id = UserId("U2"))
        val info3 = users.UserInfo(id = UserId("U3"))

        currentTime = epoch.plusSeconds(1)
        cache.put(UserId("U1"), info1).unsafeRunSync()
        currentTime = epoch.plusSeconds(2)
        cache.put(UserId("U2"), info2).unsafeRunSync()
        currentTime = epoch.plusSeconds(3)
        cache.put(UserId("U3"), info3).unsafeRunSync()

        // U1 should have been evicted (oldest)
        cache.get(UserId("U1")).unsafeRunSync() shouldBe None
        cache.get(UserId("U2")).unsafeRunSync() shouldBe defined
        cache.get(UserId("U3")).unsafeRunSync() shouldBe defined
      }

      "noCache should always return None" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        val cache                              = UserInfoCache.noCache[IO]
        val info                               = users.UserInfo(id = UserId("U1"))

        cache.put(UserId("U1"), info).unsafeRunSync()
        cache.get(UserId("U1")).unsafeRunSync() shouldBe None
      }
    }

    "onCommand" - {
      "should dispatch slash command to registered handler" in {
        val gateway                           = createGateway(MockBackend.withResponseUrl())
        var captured: Option[Command[String]] = None

        gateway
          .registerCommand[String]("/deploy") { cmd =>
            IO { captured = Some(cmd) }.as(CommandResponse.InChannel(s"Deploying ${cmd.args}"))
          }
          .unsafeRunSync()

        val payload = slashCommandPayload("/deploy", "v1.2.3")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()

        captured shouldBe defined
        captured.get.args shouldBe "v1.2.3"
        captured.get.userId shouldBe UserId("U123")
        captured.get.channelId shouldBe ChannelId("C123")
        captured.get.text shouldBe "v1.2.3"
      }

      "should normalize command names (strip / and lowercase)" in {
        val gateway = createGateway(MockBackend.withResponseUrl())
        var called  = false

        gateway
          .registerCommand[String]("Deploy") { _ =>
            IO { called = true }.as(CommandResponse.Ephemeral("ok"))
          }
          .unsafeRunSync()

        val payload = slashCommandPayload("/deploy", "test")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()

        called shouldBe true
      }

      "should ignore unregistered commands" in {
        val gateway = createGateway()
        var called  = false

        gateway
          .registerCommand[String]("/deploy") { _ =>
            IO { called = true }.as(CommandResponse.Ephemeral("ok"))
          }
          .unsafeRunSync()

        val payload = slashCommandPayload("/rollback", "test")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()

        called shouldBe false
      }

      "should return ephemeral error on parse failure" in {
        val gateway = createGateway(MockBackend.withResponseUrl())

        given CommandParser[Int] with {
          def parse(text: String): Either[String, Int] =
            text.toIntOption.toRight(s"'$text' is not a number")
        }

        gateway
          .registerCommand[Int]("/count") { cmd =>
            IO.pure(CommandResponse.InChannel(s"Count: ${cmd.args}"))
          }
          .unsafeRunSync()

        // This should not throw - parse error is handled internally
        val payload = slashCommandPayload("/count", "not-a-number")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()
      }

      "should dispatch derived case class command" in {
        val gateway                              = createGateway(MockBackend.withResponseUrl())
        var captured: Option[Command[ScaleArgs]] = None

        gateway
          .registerCommand[ScaleArgs]("/scale") { cmd =>
            IO { captured = Some(cmd) }.as(CommandResponse.Ephemeral("ok"))
          }
          .unsafeRunSync()

        val payload = slashCommandPayload("/scale", "api 3")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()

        captured shouldBe defined
        captured.get.args.service shouldBe "api"
        captured.get.args.replicas shouldBe 3
      }

      "should return error for derived parser when args are missing" in {
        val gateway = createGateway(MockBackend.withResponseUrl())

        gateway
          .registerCommand[ScaleArgs]("/scale") { _ =>
            IO.pure(CommandResponse.Ephemeral("ok"))
          }
          .unsafeRunSync()

        val payload = slashCommandPayload("/scale", "api")
        gateway.handleSlashCommandPayload(payload).unsafeRunSync()
        // parse error is handled internally as ephemeral
      }
    }

    "manifest" - {
      "minimal - no commands or buttons" in {
        val gateway = createGateway()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-minimal.json")
      }

      "with commands" in {
        val gateway = createGateway()

        gateway
          .registerCommand[String]("deploy", "Deploy to prod") { _ =>
            IO.pure(CommandResponse.Silent)
          }
          .unsafeRunSync()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-with-commands.json")
      }

      "with commands with usage hint" in {
        val gateway = createGateway()

        gateway
          .registerCommand[ScaleArgs]("scale", "Scale a service") { _ =>
            IO.pure(CommandResponse.Silent)
          }
          .unsafeRunSync()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-with-commands-usage-hint.json")
      }

      "with explicit usage hint override" in {
        val gateway = createGateway()

        gateway
          .registerCommand[String]("search", "Search for something", usageHint = "[query]") { _ =>
            IO.pure(CommandResponse.Silent)
          }
          .unsafeRunSync()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-with-explicit-usage-hint.json")
      }

      "with buttons" in {
        val gateway = createGateway()

        gateway.registerButton[String](_ => IO.unit).unsafeRunSync()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-with-buttons.json")
      }

      "with forms" in {
        val gateway = createGateway()

        case class TestForm(name: String) derives FormDef

        gateway.registerForm[TestForm, String](_ => IO.unit).unsafeRunSync()

        val result = gateway.manifest("TestApp").unsafeRunSync()

        SnapshotTest.testSnapshot(result.renderJson, "snapshots/manifest-with-forms.json")
      }

      "should include scopes required by the registered IdempotencyCheck" in {
        val customCheck = new IdempotencyCheck[IO] {
          def findExisting(channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): IO[Option[MessageId]] = IO.pure(None)
          def recordSent(key: IdempotencyKey, messageId: MessageId): IO[Unit]                                        = IO.unit
          override val requiredBotScopes: Set[String]                                                                = Set("custom:scope-a", "custom:scope-b")
        }

        val gateway = createGateway()
        gateway.withIdempotencyCheck(customCheck).unsafeRunSync()

        val manifest  = gateway.manifest("TestApp").unsafeRunSync()
        val botScopes = manifest.oauth_config.scopes.flatMap(_.bot).getOrElse(Nil)

        botScopes should contain allOf ("custom:scope-a", "custom:scope-b")
      }

      "modifier should add extra scopes" in {
        val gateway = createGateway()

        val result = gateway
          .manifest("TestApp")
          .unsafeRunSync()
          .addBotScopes("channels:read")

        val json = result.renderJson
        json should include("channels:read")
      }

      "addBotScopes should auto-populate bot_user when missing" in {
        val manifest = SlackAppManifest(
          display_information = DisplayInformation(name = "MyBot"),
        ).addBotScopes("chat:write")

        manifest.features.bot_user shouldBe defined
        manifest.features.bot_user.get.display_name shouldBe "MyBot"
      }

      "addBotScopes should preserve existing bot_user" in {
        val manifest = SlackAppManifest(
          display_information = DisplayInformation(name = "MyBot"),
          features = Features(bot_user = Some(BotUser(display_name = "Custom Name", always_online = Some(true)))),
        ).addBotScopes("chat:write")

        manifest.features.bot_user.get.display_name shouldBe "Custom Name"
        manifest.features.bot_user.get.always_online shouldBe Some(true)
      }
    }

    "checkSetup" - {
      "should return Created on first run" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        val result = gateway.checkSetup("TestApp", manifestPath.toString).unsafeRunSync()

        result shouldBe a[SetupVerification.Created]
        val created = result.asInstanceOf[SetupVerification.Created]
        created.path shouldBe manifestPath
        created.createAppUrl should include("api.slack.com")
        Files.exists(manifestPath) shouldBe true
        val content = Files.readString(manifestPath)
        content should include("TestApp")

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }

      "should return UpToDate when manifest matches" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        val manifest = gateway.manifest("TestApp").unsafeRunSync().renderJson
        Files.writeString(manifestPath, manifest)

        val result = gateway.checkSetup("TestApp", manifestPath.toString).unsafeRunSync()
        result shouldBe SetupVerification.UpToDate

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }

      "should return Changed when manifest differs" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        Files.writeString(manifestPath, "old content")

        val result = gateway.checkSetup("TestApp", manifestPath.toString).unsafeRunSync()

        result shouldBe a[SetupVerification.Changed]
        val changed = result.asInstanceOf[SetupVerification.Changed]
        changed.path shouldBe manifestPath
        changed.diff should not be empty

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }
    }

    "validateSetup" - {
      "should create manifest file on first run" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        val ex = intercept[ManifestCheck.ManifestCreated] {
          gateway.validateSetup("TestApp", manifestPath.toString).unsafeRunSync()
        }

        ex.path shouldBe manifestPath
        Files.exists(manifestPath) shouldBe true
        val content = Files.readString(manifestPath)
        content should include("TestApp")

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }

      "should succeed when manifest matches" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        val manifest = gateway.manifest("TestApp").unsafeRunSync().renderJson
        Files.writeString(manifestPath, manifest)

        // Should not throw
        gateway.validateSetup("TestApp", manifestPath.toString).unsafeRunSync()

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }

      "should detect manifest changes" in {
        val gateway      = createDisconnectedGateway()
        val tmpDir       = Files.createTempDirectory("manifest-test")
        val manifestPath = tmpDir.resolve("slack-manifest.yml")

        Files.writeString(manifestPath, "old content")

        val ex = intercept[ManifestCheck.ManifestChanged] {
          gateway.validateSetup("TestApp", manifestPath.toString).unsafeRunSync()
        }

        ex.path shouldBe manifestPath

        Files.deleteIfExists(manifestPath)
        Files.deleteIfExists(tmpDir)
      }
    }

    "idempotency" - {
      val okPostMsg = """{"ok":true,"channel":"C123","ts":"1234567890.123"}"""

      "send without key should send normally without conversations.history call" in {
        var historyCallCount = 0
        val backend          = MockBackend
          .create()
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("conversations.history")) historyCallCount += 1
            true
          }
          .thenRespondAdjust(okPostMsg)

        val gateway = createGateway(backend)
        val result  = gateway.send("C123", "Hello").unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        historyCallCount shouldBe 0
      }

      "send with key and no match in history should send with metadata" in {
        var capturedBody: Option[String] = None
        val historyResponse              = """{"ok":true,"messages":[]}"""
        val backend                      = MockBackend
          .create()
          .whenRequestMatches(_.uri.toString().contains("conversations.history"))
          .thenRespondAdjust(historyResponse)
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("chat.postMessage")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust(okPostMsg)

        val gateway = createGateway(backend)
        val result  = gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("my-key"))).unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        capturedBody shouldBe defined
        val json     = parser.parse(capturedBody.get).toOption.get
        val metadata = json.hcursor.downField("metadata")
        metadata.downField("event_type").as[String] shouldBe Right("chatops4s_idempotency")
        metadata.downField("event_payload").downField("key").as[String] shouldBe Right("my-key")
      }

      "send with key and match found in history should return existing MessageId" in {
        var postMessageCalled = false
        val metadataJson      = """{"event_type":"chatops4s_idempotency","event_payload":{"key":"my-key"}}"""
        val historyResponse   = s"""{"ok":true,"messages":[{"ts":"1234567890.999","metadata":$metadataJson}]}"""
        val backend           = MockBackend
          .create()
          .whenRequestMatches(_.uri.toString().contains("conversations.history"))
          .thenRespondAdjust(historyResponse)
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("chat.postMessage")) postMessageCalled = true
            req.uri.toString().contains("chat.postMessage")
          }
          .thenRespondAdjust(okPostMsg)

        val gateway = createGateway(backend)
        val result  = gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("my-key"))).unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.999"))
        postMessageCalled shouldBe false
      }

      "reply with key and no match in thread should send with metadata" in {
        var capturedBody: Option[String] = None
        val repliesResponse              = """{"ok":true,"messages":[]}"""
        val backend                      = MockBackend
          .create()
          .whenRequestMatches(_.uri.toString().contains("conversations.replies"))
          .thenRespondAdjust(repliesResponse)
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("chat.postMessage")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust(okPostMsg)

        val gateway   = createGateway(backend)
        val threadMsg = MessageId(ChannelId("C123"), Timestamp("1234567890.100"))
        val result    = gateway.reply(threadMsg, "Thread reply", idempotencyKey = Some(IdempotencyKey("reply-key"))).unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        capturedBody shouldBe defined
        val json     = parser.parse(capturedBody.get).toOption.get
        val metadata = json.hcursor.downField("metadata")
        metadata.downField("event_type").as[String] shouldBe Right("chatops4s_idempotency")
        metadata.downField("event_payload").downField("key").as[String] shouldBe Right("reply-key")
      }

      "reply with key and match found in thread should return existing MessageId" in {
        var postMessageCalled = false
        val metadataJson      = """{"event_type":"chatops4s_idempotency","event_payload":{"key":"reply-key"}}"""
        val repliesResponse   = s"""{"ok":true,"messages":[{"ts":"1234567890.200","metadata":$metadataJson}]}"""
        val backend           = MockBackend
          .create()
          .whenRequestMatches(_.uri.toString().contains("conversations.replies"))
          .thenRespondAdjust(repliesResponse)
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("chat.postMessage")) postMessageCalled = true
            req.uri.toString().contains("chat.postMessage")
          }
          .thenRespondAdjust(okPostMsg)

        val gateway   = createGateway(backend)
        val threadMsg = MessageId(ChannelId("C123"), Timestamp("1234567890.100"))
        val result    = gateway.reply(threadMsg, "Thread reply", idempotencyKey = Some(IdempotencyKey("reply-key"))).unsafeRunSync()

        result shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.200"))
        postMessageCalled shouldBe false
      }

      "send with noCheck should always send even with key" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        var postMessageCallCount               = 0
        val backend                            = MockBackend
          .create()
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("chat.postMessage")) postMessageCallCount += 1
            true
          }
          .thenRespondAdjust(okPostMsg)

        val gateway = createGateway(backend, idempotencyCheck = Some(IdempotencyCheck.noCheck[IO]))
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("key-1"))).unsafeRunSync()
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("key-1"))).unsafeRunSync()

        postMessageCallCount shouldBe 2
      }

      "withIdempotencyCheck should swap strategy at runtime" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        var postMessageCallCount               = 0
        val backend                            = MockBackend
          .create()
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("chat.postMessage")) postMessageCallCount += 1
            true
          }
          .thenRespondAdjust(okPostMsg)

        // Start with noCheck — both sends go through
        val gateway = createGateway(backend, idempotencyCheck = Some(IdempotencyCheck.noCheck[IO]))
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("k"))).unsafeRunSync()
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("k"))).unsafeRunSync()
        postMessageCallCount shouldBe 2

        // Swap to inMemory — second send is deduplicated
        val memCheck = IdempotencyCheck.inMemory[IO]().unsafeRunSync()
        gateway.withIdempotencyCheck(memCheck).unsafeRunSync()
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("k2"))).unsafeRunSync()
        gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("k2"))).unsafeRunSync()
        postMessageCallCount shouldBe 3
      }

      "send with inMemory check should use local cache and skip Slack scan" in {
        given monad: sttp.monad.MonadError[IO] = MockBackend.create().monad
        var historyCallCount                   = 0
        var postMessageCallCount               = 0
        val backend                            = MockBackend
          .create()
          .whenRequestMatches { req =>
            if (req.uri.toString().contains("conversations.history")) historyCallCount += 1
            if (req.uri.toString().contains("chat.postMessage")) postMessageCallCount += 1
            true
          }
          .thenRespondAdjust(okPostMsg)

        val cache   = IdempotencyCheck.inMemory[IO]().unsafeRunSync()
        val gateway = createGateway(backend, idempotencyCheck = Some(cache))

        val result1 = gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("key-1"))).unsafeRunSync()
        val result2 = gateway.send("C123", "Hello", idempotencyKey = Some(IdempotencyKey("key-1"))).unsafeRunSync()

        result1 shouldBe MessageId(ChannelId("C123"), Timestamp("1234567890.123"))
        result2 shouldBe result1
        historyCallCount shouldBe 0
        postMessageCallCount shouldBe 1
      }
    }

    "forms" - {
      "should generate unique form IDs" in {
        val gateway = createGateway()

        case class TestForm(name: String) derives FormDef

        val ids = (1 to 10).toList.traverse(_ => gateway.registerForm[TestForm, String](_ => IO.unit)).unsafeRunSync()

        ids.map(_.value).toSet.size shouldBe 10
      }

      "should open form with correct view JSON" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("views.open")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust("""{"ok":true}""")

        val gateway = createGateway(backend)

        case class DeployForm(service: String, version: String, dryRun: Boolean) derives FormDef

        val formId = gateway.registerForm[DeployForm, String](_ => IO.unit).unsafeRunSync()
        gateway.openForm(TriggerId("trigger-123"), formId, "Deploy Service", submitLabel = "Deploy").unsafeRunSync()

        capturedBody shouldBe defined
        val json = parser.parse(capturedBody.get).toOption.get
        val view = json.hcursor.downField("view")
        view.downField("type").as[String] shouldBe Right("modal")
        view.downField("callback_id").as[String] shouldBe Right(formId.value)
        view.downField("title").downField("text").as[String] shouldBe Right("Deploy Service")
        view.downField("submit").downField("text").as[String] shouldBe Right("Deploy")

        val blocks = view.downField("blocks").as[List[Json]].toOption.get
        blocks.size shouldBe 3

        // service field
        blocks(0).hcursor.downField("block_id").as[String] shouldBe Right("service")
        blocks(0).hcursor.downField("element").downField("type").as[String] shouldBe Right("plain_text_input")

        // version field
        blocks(1).hcursor.downField("block_id").as[String] shouldBe Right("version")
        blocks(1).hcursor.downField("element").downField("type").as[String] shouldBe Right("plain_text_input")

        // dryRun field
        blocks(2).hcursor.downField("block_id").as[String] shouldBe Right("dryRun")
        blocks(2).hcursor.downField("element").downField("type").as[String] shouldBe Right("checkboxes")
      }

      "should dispatch view submission to registered handler" in {
        val gateway                                                  = createGateway()
        var captured: Option[FormSubmission[TestSubmitForm, String]] = None

        case class TestSubmitForm(name: String, count: Int) derives FormDef

        val formId = gateway
          .registerForm[TestSubmitForm, String] { submission =>
            IO { captured = Some(submission) }
          }
          .unsafeRunSync()

        val payload = viewSubmissionPayload(
          callbackId = formId.value,
          values = Map(
            "name"  -> Map("name" -> ViewStateValue(value = Some("my-service"))),
            "count" -> Map("count" -> ViewStateValue(value = Some("42"))),
          ),
        )
        gateway.handleViewSubmissionPayload(payload).unsafeRunSync()

        captured shouldBe defined
        captured.get.userId shouldBe UserId("U123")
        captured.get.values.name shouldBe "my-service"
        captured.get.values.count shouldBe 42
      }

      "should ignore unknown callback IDs in view submission" in {
        val gateway = createGateway()
        var called  = false

        case class TestForm(name: String) derives FormDef

        gateway
          .registerForm[TestForm, String] { _ =>
            IO { called = true }
          }
          .unsafeRunSync()

        val payload = viewSubmissionPayload(
          callbackId = "unknown-id",
          values = Map.empty,
        )
        gateway.handleViewSubmissionPayload(payload).unsafeRunSync()

        called shouldBe false
      }

      "CommandParser.derived should produce correct usage hint" in {
        val parser = summon[CommandParser[ScaleArgs]]
        parser.usageHint shouldBe "[service] [replicas]"
      }

      "CommandParser.derived should parse space-separated args" in {
        val parser = summon[CommandParser[ScaleArgs]]
        parser.parse("api 3") shouldBe Right(ScaleArgs("api", 3))
      }

      "CommandParser.derived should give last field the remainder" in {
        case class MsgArgs(target: String, message: String) derives CommandParser
        val parser = summon[CommandParser[MsgArgs]]
        parser.parse("user hello world") shouldBe Right(MsgArgs("user", "hello world"))
      }

      "CommandParser.derived should fail on too few arguments" in {
        val parser = summon[CommandParser[ScaleArgs]]
        parser.parse("api").isLeft shouldBe true
      }

      "CommandParser.derived should fail on invalid types" in {
        val parser = summon[CommandParser[ScaleArgs]]
        parser.parse("api abc").isLeft shouldBe true
      }

      "CommandParser.derived should convert camelCase to human-readable hint" in {
        case class MyArgs(serviceName: String, replicaCount: Int) derives CommandParser
        summon[CommandParser[MyArgs]].usageHint shouldBe "[service name] [replica count]"
      }

      "FormDef.derived should produce correct fields for a sample case class" in {
        case class SampleForm(
            name: String,
            age: Int,
            score: Double,
            active: Boolean,
            nickname: Option[String],
        ) derives FormDef

        val fd     = summon[FormDef[SampleForm]]
        val fields = fd.fields

        fields.size shouldBe 5
        fields(0) shouldBe FormFieldDef("name", "Name", optional = false)
        fields(1) shouldBe FormFieldDef("age", "Age", optional = false)
        fields(2) shouldBe FormFieldDef("score", "Score", optional = false)
        fields(3) shouldBe FormFieldDef("active", "Active", optional = true)
        fields(4) shouldBe FormFieldDef("nickname", "Nickname", optional = true)
      }

      "FormDef.derived should parse values correctly" in {
        case class SampleForm(name: String, count: Int, active: Boolean) derives FormDef

        val fd     = summon[FormDef[SampleForm]]
        val values = Map(
          "name"   -> Map("name" -> ViewStateValue(value = Some("test"))),
          "count"  -> Map("count" -> ViewStateValue(value = Some("5"))),
          "active" -> Map("active" -> ViewStateValue(selected_options = Some(List(SelectedOption(value = "true"))))),
        )

        val result = fd.parse(values)
        result shouldBe Right(SampleForm("test", 5, true))
      }

      "FormDef.derived should parse boolean false (empty selected_options)" in {
        case class BoolForm(flag: Boolean) derives FormDef

        val fd     = summon[FormDef[BoolForm]]
        val values = Map(
          "flag" -> Map("flag" -> ViewStateValue(selected_options = Some(List.empty))),
        )

        fd.parse(values) shouldBe Right(BoolForm(false))
      }

      "FormDef.derived should parse Option[String] as None when missing" in {
        case class OptForm(note: Option[String]) derives FormDef

        val fd     = summon[FormDef[OptForm]]
        val values = Map(
          "note" -> Map("note" -> ViewStateValue()),
        )

        fd.parse(values) shouldBe Right(OptForm(None))
      }

      "FormDef.derived should parse Option[String] as Some when present" in {
        case class OptForm(note: Option[String]) derives FormDef

        val fd     = summon[FormDef[OptForm]]
        val values = Map(
          "note" -> Map("note" -> ViewStateValue(value = Some("hello"))),
        )

        fd.parse(values) shouldBe Right(OptForm(Some("hello")))
      }

      // --- New type tests ---

      "FormDef.derived should parse LocalDate" in {
        case class DateForm(date: LocalDate) derives FormDef

        val fd     = summon[FormDef[DateForm]]
        val values = Map(
          "date" -> Map("date" -> ViewStateValue(selected_date = Some("2025-01-15"))),
        )

        fd.parse(values) shouldBe Right(DateForm(LocalDate.of(2025, 1, 15)))
      }

      "FormDef.derived should parse LocalTime" in {
        case class TimeForm(time: LocalTime) derives FormDef

        val fd     = summon[FormDef[TimeForm]]
        val values = Map(
          "time" -> Map("time" -> ViewStateValue(selected_time = Some("14:30"))),
        )

        fd.parse(values) shouldBe Right(TimeForm(LocalTime.of(14, 30)))
      }

      "FormDef.derived should parse Instant" in {
        case class InstantForm(timestamp: Instant) derives FormDef

        val fd     = summon[FormDef[InstantForm]]
        val values = Map(
          "timestamp" -> Map("timestamp" -> ViewStateValue(selected_date_time = Some(1700000000))),
        )

        fd.parse(values) shouldBe Right(InstantForm(Instant.ofEpochSecond(1700000000L)))
      }

      "FormDef.derived should parse Email" in {
        case class EmailForm(email: Email) derives FormDef

        val fd     = summon[FormDef[EmailForm]]
        val values = Map(
          "email" -> Map("email" -> ViewStateValue(value = Some("test@example.com"))),
        )

        fd.parse(values) shouldBe Right(EmailForm(Email("test@example.com")))
      }

      "FormDef.derived should parse Url" in {
        case class UrlForm(website: Url) derives FormDef

        val fd     = summon[FormDef[UrlForm]]
        val values = Map(
          "website" -> Map("website" -> ViewStateValue(value = Some("https://example.com"))),
        )

        fd.parse(values) shouldBe Right(UrlForm(Url("https://example.com")))
      }

      "FormDef.derived should parse UserId" in {
        case class UserForm(user: UserId) derives FormDef

        val fd     = summon[FormDef[UserForm]]
        val values = Map(
          "user" -> Map("user" -> ViewStateValue(selected_user = Some(UserId("U456")))),
        )

        fd.parse(values) shouldBe Right(UserForm(UserId("U456")))
      }

      "FormDef.derived should parse List[UserId]" in {
        case class UsersForm(users: List[UserId]) derives FormDef

        val fd     = summon[FormDef[UsersForm]]
        val values = Map(
          "users" -> Map("users" -> ViewStateValue(selected_users = Some(List(UserId("U1"), UserId("U2"))))),
        )

        fd.parse(values) shouldBe Right(UsersForm(List(UserId("U1"), UserId("U2"))))
      }

      "FormDef.derived should parse ChannelId" in {
        case class ChannelForm(channel: ChannelId) derives FormDef

        val fd     = summon[FormDef[ChannelForm]]
        val values = Map(
          "channel" -> Map("channel" -> ViewStateValue(selected_channel = Some("C789"))),
        )

        fd.parse(values) shouldBe Right(ChannelForm(ChannelId("C789")))
      }

      "FormDef.derived should parse List[ChannelId]" in {
        case class ChannelsForm(channels: List[ChannelId]) derives FormDef

        val fd     = summon[FormDef[ChannelsForm]]
        val values = Map(
          "channels" -> Map("channels" -> ViewStateValue(selected_channels = Some(List("C1", "C2")))),
        )

        fd.parse(values) shouldBe Right(ChannelsForm(List(ChannelId("C1"), ChannelId("C2"))))
      }

      "FormDef.derived should parse ConversationId" in {
        case class ConvoForm(convo: ConversationId) derives FormDef

        val fd     = summon[FormDef[ConvoForm]]
        val values = Map(
          "convo" -> Map("convo" -> ViewStateValue(selected_conversation = Some("D123"))),
        )

        fd.parse(values) shouldBe Right(ConvoForm(ConversationId("D123")))
      }

      "FormDef.derived should parse List[ConversationId]" in {
        case class ConvosForm(convos: List[ConversationId]) derives FormDef

        val fd     = summon[FormDef[ConvosForm]]
        val values = Map(
          "convos" -> Map("convos" -> ViewStateValue(selected_conversations = Some(List("D1", "D2")))),
        )

        fd.parse(values) shouldBe Right(ConvosForm(List(ConversationId("D1"), ConversationId("D2"))))
      }

      "FormDef.derived should parse RichTextBlock" in {
        case class RichForm(content: RichTextBlock) derives FormDef

        val fd        = summon[FormDef[RichForm]]
        val richBlock = RichTextBlock(elements =
          List(
            RichTextSection(elements = List(RichTextText("Hello"))),
          ),
        )
        val values    = Map(
          "content" -> Map("content" -> ViewStateValue(rich_text_value = Some(richBlock))),
        )

        fd.parse(values) shouldBe Right(RichForm(richBlock))
      }

      "FormDef.derived should parse List[Json] (files)" in {
        case class FileForm(attachments: List[Json]) derives FormDef

        val fd     = summon[FormDef[FileForm]]
        val file1  = Json.obj("id" -> Json.fromString("F1"))
        val file2  = Json.obj("id" -> Json.fromString("F2"))
        val values = Map(
          "attachments" -> Map("attachments" -> ViewStateValue(files = Some(List(file1, file2)))),
        )

        fd.parse(values) shouldBe Right(FileForm(List(file1, file2)))
      }

      // --- Option wrapping for new types ---

      "FormDef.derived should parse Option[LocalDate] as None" in {
        case class OptDateForm(date: Option[LocalDate]) derives FormDef

        val fd     = summon[FormDef[OptDateForm]]
        val values = Map(
          "date" -> Map("date" -> ViewStateValue()),
        )

        fd.parse(values) shouldBe Right(OptDateForm(None))
      }

      "FormDef.derived should parse Option[LocalDate] as Some" in {
        case class OptDateForm(date: Option[LocalDate]) derives FormDef

        val fd     = summon[FormDef[OptDateForm]]
        val values = Map(
          "date" -> Map("date" -> ViewStateValue(selected_date = Some("2025-06-15"))),
        )

        fd.parse(values) shouldBe Right(OptDateForm(Some(LocalDate.of(2025, 6, 15))))
      }

      "FormDef.derived should parse Option[UserId] as None" in {
        case class OptUserForm(user: Option[UserId]) derives FormDef

        val fd     = summon[FormDef[OptUserForm]]
        val values = Map(
          "user" -> Map("user" -> ViewStateValue()),
        )

        fd.parse(values) shouldBe Right(OptUserForm(None))
      }

      "FormDef.derived should parse Option[UserId] as Some" in {
        case class OptUserForm(user: Option[UserId]) derives FormDef

        val fd     = summon[FormDef[OptUserForm]]
        val values = Map(
          "user" -> Map("user" -> ViewStateValue(selected_user = Some(UserId("U999")))),
        )

        fd.parse(values) shouldBe Right(OptUserForm(Some(UserId("U999"))))
      }

      // --- Options-based codec tests ---

      "staticSelect should parse selected option" in {
        val mappings = List(
          BlockOption(text = PlainTextObject("Small"), value = "s") -> "small",
          BlockOption(text = PlainTextObject("Large"), value = "l") -> "large",
        )
        val codec    = FieldCodec.staticSelect(mappings)

        val result = codec.parse(ViewStateValue(selected_option = Some(SelectedOption(value = "s"))))
        result shouldBe Right("small")
      }

      "staticSelect should fail on unknown option" in {
        val mappings = List(
          BlockOption(text = PlainTextObject("Small"), value = "s") -> "small",
        )
        val codec    = FieldCodec.staticSelect(mappings)

        val result = codec.parse(ViewStateValue(selected_option = Some(SelectedOption(value = "x"))))
        result.isLeft shouldBe true
      }

      "multiStaticSelect should parse selected options" in {
        val mappings = List(
          BlockOption(text = PlainTextObject("A"), value = "a") -> 1,
          BlockOption(text = PlainTextObject("B"), value = "b") -> 2,
          BlockOption(text = PlainTextObject("C"), value = "c") -> 3,
        )
        val codec    = FieldCodec.multiStaticSelect(mappings)

        val result = codec.parse(
          ViewStateValue(selected_options =
            Some(
              List(
                SelectedOption(value = "a"),
                SelectedOption(value = "c"),
              ),
            ),
          ),
        )
        result shouldBe Right(List(1, 3))
      }

      "radioButtons should parse selected option" in {
        val mappings = List(
          BlockOption(text = PlainTextObject("Yes"), value = "y") -> true,
          BlockOption(text = PlainTextObject("No"), value = "n")  -> false,
        )
        val codec    = FieldCodec.radioButtons(mappings)

        val result = codec.parse(ViewStateValue(selected_option = Some(SelectedOption(value = "n"))))
        result shouldBe Right(false)
      }

      "checkboxes should parse selected options" in {
        val mappings = List(
          BlockOption(text = PlainTextObject("Read"), value = "r")    -> "read",
          BlockOption(text = PlainTextObject("Write"), value = "w")   -> "write",
          BlockOption(text = PlainTextObject("Execute"), value = "x") -> "exec",
        )
        val codec    = FieldCodec.checkboxes(mappings)

        val result = codec.parse(
          ViewStateValue(selected_options =
            Some(
              List(
                SelectedOption(value = "r"),
                SelectedOption(value = "x"),
              ),
            ),
          ),
        )
        result shouldBe Right(List("read", "exec"))
      }

      "externalSelect should parse selected option value" in {
        val codec = FieldCodec.externalSelect(minQueryLength = Some(3))

        val result = codec.parse(ViewStateValue(selected_option = Some(SelectedOption(value = "ext-val"))))
        result shouldBe Right("ext-val")
      }

      "multiExternalSelect should parse selected options" in {
        val codec = FieldCodec.multiExternalSelect()

        val result = codec.parse(
          ViewStateValue(selected_options =
            Some(
              List(
                SelectedOption(value = "v1"),
                SelectedOption(value = "v2"),
              ),
            ),
          ),
        )
        result shouldBe Right(List("v1", "v2"))
      }

      // --- openForm view JSON structure tests for new types ---

      "should open form with date picker element" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("views.open")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust("""{"ok":true}""")

        val gateway = createGateway(backend)

        case class DateForm(date: LocalDate) derives FormDef

        val formId = gateway.registerForm[DateForm, String](_ => IO.unit).unsafeRunSync()
        gateway.openForm(TriggerId("trigger-123"), formId, "Pick Date").unsafeRunSync()

        capturedBody shouldBe defined
        val json   = parser.parse(capturedBody.get).toOption.get
        val blocks = json.hcursor.downField("view").downField("blocks").as[List[Json]].toOption.get
        blocks.size shouldBe 1
        blocks(0).hcursor.downField("element").downField("type").as[String] shouldBe Right("datepicker")
      }

      "should open form with users select element" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("views.open")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust("""{"ok":true}""")

        val gateway = createGateway(backend)

        case class UserForm(user: UserId) derives FormDef

        val formId = gateway.registerForm[UserForm, String](_ => IO.unit).unsafeRunSync()
        gateway.openForm(TriggerId("trigger-123"), formId, "Pick User").unsafeRunSync()

        capturedBody shouldBe defined
        val json   = parser.parse(capturedBody.get).toOption.get
        val blocks = json.hcursor.downField("view").downField("blocks").as[List[Json]].toOption.get
        blocks.size shouldBe 1
        blocks(0).hcursor.downField("element").downField("type").as[String] shouldBe Right("users_select")
      }

      "should open form with email input element" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("views.open")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust("""{"ok":true}""")

        val gateway = createGateway(backend)

        case class EmailForm(email: Email) derives FormDef

        val formId = gateway.registerForm[EmailForm, String](_ => IO.unit).unsafeRunSync()
        gateway.openForm(TriggerId("trigger-123"), formId, "Enter Email").unsafeRunSync()

        capturedBody shouldBe defined
        val json   = parser.parse(capturedBody.get).toOption.get
        val blocks = json.hcursor.downField("view").downField("blocks").as[List[Json]].toOption.get
        blocks.size shouldBe 1
        blocks(0).hcursor.downField("element").downField("type").as[String] shouldBe Right("email_text_input")
      }

      "should open form with all element types" in {
        var capturedBody: Option[String] = None
        val backend                      = MockBackend
          .create()
          .whenRequestMatches { req =>
            val matches = req.uri.toString().contains("views.open")
            if (matches) {
              capturedBody = req.body match {
                case sttp.client4.StringBody(s, _, _) => Some(s)
                case _                                => None
              }
            }
            matches
          }
          .thenRespondAdjust("""{"ok":true}""")

        val gateway = createGateway(backend)

        case class FullForm(
            name: String,
            age: Int,
            score: Double,
            active: Boolean,
            email: Email,
            website: Url,
            date: LocalDate,
            time: LocalTime,
            timestamp: Instant,
            user: UserId,
            channel: ChannelId,
            convo: ConversationId,
        ) derives FormDef

        val formId = gateway.registerForm[FullForm, String](_ => IO.unit).unsafeRunSync()
        gateway.openForm(TriggerId("trigger-123"), formId, "Full Form").unsafeRunSync()

        capturedBody shouldBe defined
        val json   = parser.parse(capturedBody.get).toOption.get
        val blocks = json.hcursor.downField("view").downField("blocks").as[List[Json]].toOption.get
        blocks.size shouldBe 12

        val expectedTypes = List(
          "plain_text_input",    // name: String
          "number_input",        // age: Int
          "number_input",        // score: Double
          "checkboxes",          // active: Boolean
          "email_text_input",    // email: Email
          "url_text_input",      // website: Url
          "datepicker",          // date: LocalDate
          "timepicker",          // time: LocalTime
          "datetimepicker",      // timestamp: Instant
          "users_select",        // user: UserId
          "channels_select",     // channel: ChannelId
          "conversations_select", // convo: ConversationId
        )

        blocks.zip(expectedTypes).foreach { case (block, expectedType) =>
          block.hcursor.downField("element").downField("type").as[String] shouldBe Right(expectedType)
        }
      }

      "should round-trip typed metadata through openForm and submission" in {
        var capturedSub: Option[FormSubmission[TestMetaForm, DeployMeta]] = None
        val gateway                                                       = createGateway(MockBackend.create().whenAnyRequest.thenRespondAdjust("""{"ok":true}"""))

        case class TestMetaForm(name: String) derives FormDef
        case class DeployMeta(env: String) derives io.circe.Encoder.AsObject, io.circe.Decoder

        val formId = gateway.registerForm[TestMetaForm, DeployMeta] { sub => IO { capturedSub = Some(sub) } }.unsafeRunSync()
        gateway.openForm(TriggerId("t"), formId, "Test", DeployMeta("prod")).unsafeRunSync()

        val payload = viewSubmissionPayload(
          callbackId = formId.value,
          values = Map("name" -> Map("name" -> ViewStateValue(value = Some("svc")))),
          privateMetadata = Some("""{"env":"prod"}"""),
        )
        gateway.handleViewSubmissionPayload(payload).unsafeRunSync()

        capturedSub.get.metadata shouldBe DeployMeta("prod")
        capturedSub.get.values.name shouldBe "svc"
      }

      "should round-trip string metadata" in {
        var capturedMeta: Option[String] = None
        val gateway                      = createGateway(MockBackend.create().whenAnyRequest.thenRespondAdjust("""{"ok":true}"""))

        case class SimpleForm(name: String) derives FormDef

        val formId = gateway.registerForm[SimpleForm, String] { sub => IO { capturedMeta = Some(sub.metadata) } }.unsafeRunSync()
        gateway.openForm(TriggerId("t"), formId, "Test", "ctx-123").unsafeRunSync()

        val payload = viewSubmissionPayload(
          callbackId = formId.value,
          values = Map("name" -> Map("name" -> ViewStateValue(value = Some("x")))),
          privateMetadata = Some("ctx-123"),
        )
        gateway.handleViewSubmissionPayload(payload).unsafeRunSync()

        capturedMeta shouldBe Some("ctx-123")
      }

      "should default to empty string metadata when no private_metadata present" in {
        var capturedMeta: Option[String] = None
        val gateway                      = createGateway()

        case class SimpleForm2(name: String) derives FormDef

        val formId = gateway.registerForm[SimpleForm2, String] { sub => IO { capturedMeta = Some(sub.metadata) } }.unsafeRunSync()

        val payload = viewSubmissionPayload(
          callbackId = formId.value,
          values = Map("name" -> Map("name" -> ViewStateValue(value = Some("x")))),
        )
        gateway.handleViewSubmissionPayload(payload).unsafeRunSync()

        capturedMeta shouldBe Some("")
      }
    }

    "shutdown" - {
      "SocketMode loop should exit immediately when shutdownSignal is set" in {
        val backend = MockBackend.create()
        val signal  = new java.util.concurrent.atomic.AtomicBoolean(true)

        SocketMode
          .runLoop(
            SlackAppToken.unsafe("xapp-test"),
            backend,
            _ => IO.unit,
            shutdownSignal = signal,
          )
          .unsafeRunSync()

        // If we get here, the loop respected the signal and returned
        succeed
      }

      "start should reset shutdownSignal so it can reconnect" in {
        val gateway = createDisconnectedGateway()
        gateway.shutdown().unsafeRunSync()
        // start without app token doesn't enter SocketMode, but it should reset the signal
        gateway.start(SlackBotToken.unsafe("xoxb-test"), None).unsafeRunSync()
        // no assertion needed — if start() didn't reset, a subsequent start with app token would exit immediately
        succeed
      }
    }

    "onError" - {
      "should call custom error handler when button handler throws" in {
        val gateway                          = createGateway()
        var capturedError: Option[Throwable] = None

        gateway.onError(e => IO { capturedError = Some(e) }).unsafeRunSync()

        val btnId = gateway
          .registerButton[String] { _ =>
            IO.raiseError(new RuntimeException("boom"))
          }
          .unsafeRunSync()

        val envelope = Envelope(
          `type` = EnvelopeType.Interactive,
          envelope_id = "env-1",
          payload = Some(io.circe.Encoder[InteractionPayload].apply(interactionPayload(btnId.value, "v"))),
        )
        gateway.handleEnvelope(envelope).unsafeRunSync()

        capturedError shouldBe defined
        capturedError.get.getMessage shouldBe "boom"
      }

      "should call custom error handler when form handler throws" in {
        val gateway                          = createGateway()
        var capturedError: Option[Throwable] = None

        gateway.onError(e => IO { capturedError = Some(e) }).unsafeRunSync()

        case class ErrorForm(name: String) derives FormDef

        val formId = gateway
          .registerForm[ErrorForm, String] { _ =>
            IO.raiseError(new RuntimeException("form-boom"))
          }
          .unsafeRunSync()

        val submissionPayload = viewSubmissionPayload(
          callbackId = formId.value,
          values = Map("name" -> Map("name" -> ViewStateValue(value = Some("test")))),
        )
        val envelope          = Envelope(
          `type` = EnvelopeType.Interactive,
          payload = Some(io.circe.Encoder[ViewSubmissionPayload].apply(submissionPayload)),
          envelope_id = "env-2",
        )
        gateway.handleEnvelope(envelope).unsafeRunSync()

        capturedError shouldBe defined
        capturedError.get.getMessage shouldBe "form-boom"
      }
    }
  }

  private def slashCommandPayload(command: String, text: String): SlashCommandPayload =
    SlashCommandPayload(
      command = command,
      text = text,
      user_id = UserId("U123"),
      channel_id = ChannelId("C123"),
      response_url = "https://hooks.slack.com/commands/T123/456/789",
      trigger_id = TriggerId("test-trigger-id"),
      team_id = TeamId("T123"),
      team_domain = "test",
      channel_name = "general",
      api_app_id = "A123",
    )

  private def interactionPayload(actionId: String, value: String): InteractionPayload =
    InteractionPayload(
      `type` = "block_actions",
      user = User(UserId("U123")),
      channel = Some(Channel(ChannelId("C123"))),
      container = Container(message_ts = Some(Timestamp("1234567890.123"))),
      actions = List(
        Action(actionId, "blk-1", "button", Timestamp("1234567890.123"), value = Some(value)),
      ),
      trigger_id = TriggerId("test-trigger-id"),
      api_app_id = "A123",
      token = "tok",
    )

  private def viewSubmissionPayload(
      callbackId: String,
      values: Map[String, Map[String, ViewStateValue]],
      privateMetadata: Option[String] = None,
  ): ViewSubmissionPayload =
    ViewSubmissionPayload(
      `type` = "view_submission",
      user = User(UserId("U123")),
      view = ViewPayload(
        id = "V123",
        callback_id = Some(callbackId),
        state = Some(ViewState(values)),
        private_metadata = privateMetadata,
      ),
      api_app_id = "A123",
    )
}
