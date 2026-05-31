package example.docs

import cats.effect.IO
import chatops4s.slack.{ButtonId, IdempotencyCheck, IdempotencyKey, MessageId, SlackGateway, SlackSetup, UserInfoCache}
import chatops4s.slack.api.UserId
import sttp.client4.WebSocketBackend

import java.time.Duration
import scala.annotation.nowarn

@nowarn("msg=unused")
private object BasicOps {

  def messaging(slack: SlackGateway[IO], channel: String): IO[Unit] =
    for {
      // start_send
      msgId <- slack.send(channel, "Deployment started")
      // end_send
      // start_reply
      _     <- slack.reply(msgId, "Step 1 complete")
      // end_reply
      // start_update
      _     <- slack.update(msgId, "Deployment finished")
      // end_update
      // start_delete
      _     <- slack.delete(msgId)
      // end_delete
    } yield ()

  def reactions(slack: SlackGateway[IO], msgId: MessageId): IO[Unit] =
    for {
      // start_reactions
      _ <- slack.addReaction(msgId, "hourglass_flowing_sand")
      _ <- slack.removeReaction(msgId, "hourglass_flowing_sand")
      _ <- slack.addReaction(msgId, "white_check_mark")
      // end_reactions
    } yield ()

  def ephemeral(slack: SlackGateway[IO], channel: String, userId: UserId): IO[Unit] =
    // start_ephemeral
    slack.sendEphemeral(channel, userId, "Only you can see this")
  // end_ephemeral

  def buttonsOnMessages(
      slack: SlackGateway[IO],
      channel: String,
      approveBtn: ButtonId[String],
      rejectBtn: ButtonId[String],
  ): IO[MessageId] =
    // start_buttons_on_messages
    slack.send(
      channel,
      "Approve deployment?",
      Seq(
        approveBtn.render("Approve", "v1.2.3"),
        rejectBtn.render("Reject", "v1.2.3"),
      ),
    )
  // end_buttons_on_messages

  def userInfo(slack: SlackGateway[IO], userId: UserId): IO[Unit] =
    for {
      // start_user_info
      info <- slack.getUserInfo(userId)
      // info.profile.flatMap(_.email)      — user's email
      // info.profile.flatMap(_.real_name)  — display name
      // end_user_info
      _    <- IO.unit
    } yield ()

  def customCache(slack: SlackGateway[IO] & SlackSetup[IO], backend: WebSocketBackend[IO]): IO[Unit] = {
    given sttp.monad.MonadError[IO] = backend.monad
    for {
      // start_custom_cache
      customCache <- UserInfoCache.inMemory[IO](ttl = Duration.ofMinutes(30), maxEntries = 5000)
      _           <- slack.withUserInfoCache(customCache)
      // end_custom_cache
    } yield ()
  }

  def idempotentSend(slack: SlackGateway[IO], channel: String): IO[Unit] =
    for {
      // start_idempotent_send
      msgId <- slack.send(channel, "Deployment started", idempotencyKey = Some(IdempotencyKey("deploy-v1.2.3")))
      // end_idempotent_send
      _     <- IO.unit
    } yield ()

  def idempotentReply(slack: SlackGateway[IO], msgId: MessageId): IO[Unit] =
    for {
      // start_idempotent_reply
      _ <- slack.reply(msgId, "Step 1 complete", idempotencyKey = Some(IdempotencyKey("step-1")))
      // end_idempotent_reply
    } yield ()

  def errorHandler(slack: SlackGateway[IO] & SlackSetup[IO]): IO[Unit] =
    // start_on_error
    slack.onError { error =>
      IO.println(s"Handler error: ${error.getMessage}")
    }
  // end_on_error

  def customIdempotencyCheck(slack: SlackGateway[IO] & SlackSetup[IO], backend: WebSocketBackend[IO]): IO[Unit] = {
    given sttp.monad.MonadError[IO] = backend.monad
    for {
      // start_custom_idempotency
      check <- IdempotencyCheck.inMemory[IO](ttl = Duration.ofMinutes(30), maxEntries = 5000)
      _     <- slack.withIdempotencyCheck(check)
      // end_custom_idempotency
    } yield ()
  }
}
