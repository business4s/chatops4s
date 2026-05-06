package chatops4s.slack

import chatops4s.slack.api.{TriggerId, UserId, users}
import sttp.client4.WebSocketBackend
import sttp.monad.syntax.*

trait SlackGateway[F[_]] {
  def send(channel: String, text: String, buttons: Seq[Button] = Seq.empty, idempotencyKey: Option[IdempotencyKey] = None): F[MessageId]
  def reply(to: MessageId, text: String, buttons: Seq[Button] = Seq.empty, idempotencyKey: Option[IdempotencyKey] = None): F[MessageId]
  def update(messageId: MessageId, text: String, buttons: Seq[Button] = Seq.empty): F[MessageId]
  def delete(messageId: MessageId): F[Unit]
  def addReaction(messageId: MessageId, emoji: String): F[Unit]
  def removeReaction(messageId: MessageId, emoji: String): F[Unit]
  def sendEphemeral(channel: String, userId: UserId, text: String): F[Unit]
  def openForm[T, M](
      triggerId: TriggerId,
      formId: FormId[T, M],
      title: String,
      metadata: M = "",
      submitLabel: String = "Submit",
      initialValues: InitialValues[T] = InitialValues.of[T],
  ): F[Unit]

  def getUserInfo(userId: UserId): F[users.UserInfo]
}

object SlackGateway {

  def create[F[_]](
      backend: WebSocketBackend[F],
  ): F[SlackGateway[F] & SlackSetup[F]] = {
    given monad: sttp.monad.MonadError[F]         = backend.monad
    val defaultErrorHandler: Throwable => F[Unit] = e =>
      monad.blocking(org.slf4j.LoggerFactory.getLogger("chatops4s.slack.SlackGateway").error("Handler error", e))
    for {
      clientRef          <- Ref.of[F, Option[SlackClient[F]]](None)
      handlersRef        <- Ref.of[F, Map[ButtonId[?], ErasedHandler[F]]](Map.empty)
      commandHandlersRef <- Ref.of[F, Map[CommandName, CommandEntry[F]]](Map.empty)
      formHandlersRef    <- Ref.of[F, Map[FormId[?, ?], FormEntry[F]]](Map.empty)
      defaultCache       <- UserInfoCache.inMemory[F]()
      cacheRef           <- Ref.of[F, UserInfoCache[F]](defaultCache)
      defaultCheck       <- IdempotencyCheck.slackScan[F](clientRef)
      idempotencyRef     <- Ref.of[F, IdempotencyCheck[F]](defaultCheck)
      errorHandlerRef    <- Ref.of[F, Throwable => F[Unit]](defaultErrorHandler)
    } yield {
      val gateway =
        new SlackGatewayImpl[F](clientRef, handlersRef, commandHandlersRef, formHandlersRef, cacheRef, idempotencyRef, errorHandlerRef, backend)
      gateway: SlackGateway[F] & SlackSetup[F]
    }
  }
}
