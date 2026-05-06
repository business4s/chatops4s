package chatops4s.slack

import chatops4s.slack.api.{ChannelId, NonEmptyString, ResponseType, SlackAppToken, SlackBotToken, TriggerId, UserId, users}
import chatops4s.slack.api.socket.*
import chatops4s.slack.api.blocks.*
import chatops4s.slack.api.manifest.SlackAppManifest
import io.circe.{Decoder, Json}
import sttp.client4.WebSocketBackend
import sttp.monad.MonadError
import sttp.monad.syntax.*
import chatops4s.slack.monadSyntax.*

import java.nio.file.Paths

private[slack] type ErasedHandler[F[_]]        = ButtonClick[String] => F[Unit]
private[slack] type ErasedCommandHandler[F[_]] = SlashCommandPayload => F[CommandResponse]

private[slack] opaque type CommandName = String
private[slack] object CommandName {
  def apply(raw: String): CommandName           = raw.stripPrefix("/").toLowerCase
  extension (cn: CommandName) def value: String = cn
}

private[slack] case class CommandEntry[F[_]](
    handler: ErasedCommandHandler[F],
    description: String,
    usageHint: String,
)

private[slack] case class FormEntry[F[_]](
    formDef: FormDef[Any],
    handler: FormSubmission[Any, Any] => F[Unit],
    encodeMetadata: Any => String,
    decodeMetadata: String => Any,
)

private[slack] class SlackGatewayImpl[F[_]](
    clientRef: Ref[F, Option[SlackClient[F]]],
    handlersRef: Ref[F, Map[ButtonId[?], ErasedHandler[F]]],
    commandHandlersRef: Ref[F, Map[CommandName, CommandEntry[F]]],
    formHandlersRef: Ref[F, Map[FormId[?, ?], FormEntry[F]]],
    cacheRef: Ref[F, UserInfoCache[F]],
    idempotencyRef: Ref[F, IdempotencyCheck[F]],
    errorHandlerRef: Ref[F, Throwable => F[Unit]],
    backend: WebSocketBackend[F],
) extends SlackGateway[F]
    with SlackSetup[F] {

  private val logger                                                    = org.slf4j.LoggerFactory.getLogger("chatops4s.slack.SlackGateway")
  private given monad: MonadError[F]                                    = backend.monad
  private val shutdownSignal: java.util.concurrent.atomic.AtomicBoolean = new java.util.concurrent.atomic.AtomicBoolean(false)

  // TODO: Handlers accumulate indefinitely by design. Buttons/commands/forms are registered once
  // at startup and reused. If dynamic registration is needed in the future, add TTL or unregister methods.
  override def registerButton[T <: String](name: String)(handler: ButtonClick[T] => F[Unit]): F[ButtonId[T]] = {
    if (name.trim.isEmpty)
      monad.error(new IllegalArgumentException("Button name must be non-empty"))
    else {
      val id     = ButtonId[T](name)
      val erased = handler.asInstanceOf[ErasedHandler[F]]
      handlersRef.get.flatMap { existing =>
        if (existing.contains(id))
          monad.error(new IllegalArgumentException(s"Button '$name' is already registered"))
        else
          handlersRef.update(_ + (id -> erased)).as(id)
      }
    }
  }

  override def registerCommand[T: {CommandParser as parser}](name: String, description: String = "", usageHint: String = "")(
      handler: Command[T] => F[CommandResponse],
  ): F[Unit] = {
    val normalized                      = CommandName(name)
    val resolvedHint                    = if (usageHint.nonEmpty) usageHint else parser.usageHint
    val erased: ErasedCommandHandler[F] = { payload =>
      parser.parse(payload.text) match {
        case Left(error) =>
          monad.unit(CommandResponse.Ephemeral(s"Invalid command arguments: $error"))
        case Right(args) =>
          val cmd = Command(
            payload = payload,
            args = args,
          )
          handler(cmd)
      }
    }
    commandHandlersRef.update(_ + (normalized -> CommandEntry(erased, description, resolvedHint)))
  }

  override def registerForm[T: {FormDef as fd}, M: {MetadataCodec as mc}](
      name: String,
  )(handler: FormSubmission[T, M] => F[Unit]): F[FormId[T, M]] = {
    if (name.trim.isEmpty)
      monad.error(new IllegalArgumentException("Form name must be non-empty"))
    else {
      val id    = FormId[T, M](name)
      val entry = FormEntry[F](
        formDef = fd.asInstanceOf[FormDef[Any]],
        handler = handler.asInstanceOf[FormSubmission[Any, Any] => F[Unit]],
        encodeMetadata = (v: Any) => mc.encode(v.asInstanceOf[M]),
        decodeMetadata = (raw: String) => mc.decode(raw),
      )
      formHandlersRef.get.flatMap { existing =>
        if (existing.contains(id))
          monad.error(new IllegalArgumentException(s"Form '$name' is already registered"))
        else
          formHandlersRef.update(_ + (id -> entry)).as(id)
      }
    }
  }

  override def manifest(appName: String): F[SlackAppManifest] = {
    for {
      handlers    <- handlersRef.get
      commands    <- commandHandlersRef.get
      forms       <- formHandlersRef.get
      idempotency <- idempotencyRef.get
    } yield {
      val commandDefs = commands.map((name, entry) => name.value -> (entry.description, entry.usageHint))
      SlackManifest.generate(
        appName,
        commandDefs,
        hasInteractivity = handlers.nonEmpty || forms.nonEmpty,
        extraBotScopes = idempotency.requiredBotScopes,
      )
    }
  }

  override def checkSetup(appName: String, manifestPath: String, modifier: SlackAppManifest => SlackAppManifest = identity): F[SetupVerification] = {
    for {
      m       <- manifest(appName)
      rendered = modifier(m).renderJson
      result  <- monad.eval(ManifestCheck.check(rendered, Paths.get(manifestPath)))
    } yield result
  }

  override def validateSetup(appName: String, manifestPath: String, modifier: SlackAppManifest => SlackAppManifest = identity): F[Unit] = {
    checkSetup(appName, manifestPath, modifier).flatMap {
      case SetupVerification.UpToDate   => monad.unit(())
      case v: SetupVerification.Created => monad.error(new ManifestCheck.ManifestCreated(v.path, v.message))
      case v: SetupVerification.Changed => monad.error(new ManifestCheck.ManifestChanged(v.path, v.message))
    }
  }

  override def withUserInfoCache(cache: UserInfoCache[F]): F[Unit] =
    cacheRef.update(_ => cache)

  override def onError(handler: Throwable => F[Unit]): F[Unit] =
    errorHandlerRef.update(_ => handler)

  override def listHandlers(): F[HandlerSummary] =
    for {
      buttons  <- handlersRef.get
      commands <- commandHandlersRef.get
      forms    <- formHandlersRef.get
    } yield HandlerSummary(
      buttons = buttons.keys.map(_.value).toSet,
      commands = commands.keys.map(_.value).toSet,
      forms = forms.keys.map(_.value).toSet,
    )

  override def shutdown(): F[Unit] =
    monad.eval(shutdownSignal.set(true))

  override def withIdempotencyCheck(check: IdempotencyCheck[F]): F[Unit] =
    idempotencyRef.update(_ => check)

  override def start(botToken: SlackBotToken, appToken: Option[SlackAppToken]): F[Unit] = {
    val client = new SlackClient[F](botToken, backend)
    for {
      _              <- monad.eval(shutdownSignal.set(false))
      _              <- clientRef.update(_ => Some(client))
      handlers       <- handlersRef.get
      commands       <- commandHandlersRef.get
      forms          <- formHandlersRef.get
      hasInteractions = handlers.nonEmpty || commands.nonEmpty || forms.nonEmpty
      _              <- if (hasInteractions && appToken.isEmpty)
                          monad.error(
                            new RuntimeException(
                              "App token required: buttons, commands, or forms are registered. " +
                                "Provide a SlackAppToken (xapp-...) with connections:write scope.",
                            ),
                          )
                        else if (appToken.isDefined)
                          SocketMode.runLoop(appToken.get, backend, handleEnvelope, shutdownSignal = shutdownSignal)
                        else
                          monad.unit(())
    } yield ()
  }

  override def send(channel: String, text: String, buttons: Seq[Button], idempotencyKey: Option[IdempotencyKey]): F[MessageId] =
    idempotencyKey match {
      case None      => withClient(_.postMessage(channel, text, buildBlocks(text, buttons), threadTs = None))
      case Some(key) =>
        for {
          check    <- idempotencyRef.get
          existing <- check.findExisting(channel, threadTs = None, key)
          result   <- existing match {
                        case Some(found) => monad.unit(found)
                        case None        =>
                          val metadata = Some(IdempotencyCheck.buildMetadataJson(key))
                          for {
                            msgId <- withClient(_.postMessage(channel, text, buildBlocks(text, buttons), threadTs = None, metadata = metadata))
                            _     <- check.recordSent(key, msgId)
                          } yield msgId
                      }
        } yield result
    }

  override def reply(to: MessageId, text: String, buttons: Seq[Button], idempotencyKey: Option[IdempotencyKey]): F[MessageId] =
    idempotencyKey match {
      case None      => withClient(_.postMessage(to.channel.value, text, buildBlocks(text, buttons), threadTs = Some(to.ts)))
      case Some(key) =>
        for {
          check    <- idempotencyRef.get
          existing <- check.findExisting(to.channel.value, threadTs = Some(to.ts), key)
          result   <- existing match {
                        case Some(found) => monad.unit(found)
                        case None        =>
                          val metadata = Some(IdempotencyCheck.buildMetadataJson(key))
                          for {
                            msgId <- withClient(
                                       _.postMessage(to.channel.value, text, buildBlocks(text, buttons), threadTs = Some(to.ts), metadata = metadata),
                                     )
                            _     <- check.recordSent(key, msgId)
                          } yield msgId
                      }
        } yield result
    }

  override def update(messageId: MessageId, text: String, buttons: Seq[Button]): F[MessageId] =
    withClient(_.updateMessage(messageId, text, buildBlocks(text, buttons)))

  override def delete(messageId: MessageId): F[Unit] =
    withClient(_.deleteMessage(messageId))

  override def addReaction(messageId: MessageId, emoji: String): F[Unit] =
    withClient(_.addReaction(messageId, emoji))

  override def removeReaction(messageId: MessageId, emoji: String): F[Unit] =
    withClient(_.removeReaction(messageId, emoji))

  override def sendEphemeral(channel: String, userId: UserId, text: String): F[Unit] =
    withClient(_.postEphemeral(channel, userId, text))

  override def getUserInfo(userId: UserId): F[users.UserInfo] =
    cacheRef.get.flatMap { cache =>
      cache.get(userId).flatMap {
        case Some(info) => monad.unit(info)
        case None       =>
          withClient { client =>
            client.getUserInfo(userId).flatMap { info =>
              cache.put(userId, info).map(_ => info)
            }
          }
      }
    }

  override def openForm[T, M](
      triggerId: TriggerId,
      formId: FormId[T, M],
      title: String,
      metadata: M,
      submitLabel: String = "Submit",
      initialValues: InitialValues[T] = InitialValues.of[T],
  ): F[Unit] = {
    formHandlersRef.get.flatMap { forms =>
      forms.get(formId) match {
        case None        => monad.error(new RuntimeException(s"Form not found: ${formId.value}"))
        case Some(entry) =>
          withClient { client =>
            val viewBlocks = entry.formDef.buildBlocks(initialValues.toMap)
            val encoded    = entry.encodeMetadata(metadata)
            val view       = View(
              `type` = ViewType.Modal,
              callback_id = Some(formId.value),
              title = PlainTextObject(text = title),
              submit = Some(PlainTextObject(text = submitLabel)),
              blocks = viewBlocks,
              private_metadata = if (encoded.nonEmpty) Some(encoded) else None,
            )
            client.openView(triggerId, view)
          }
      }
    }
  }

  private def withClient[A](f: SlackClient[F] => F[A]): F[A] =
    clientRef.get.flatMap {
      case Some(c) => f(c)
      case None    => monad.error(new RuntimeException("Not connected. Call start() first."))
    }

  private[slack] def handleEnvelope(envelope: Envelope): F[Unit] = {
    val dispatch = envelope.`type` match {
      case EnvelopeType.Interactive   =>
        envelope.payload.traverse_ { json =>
          val payloadType = json.hcursor.downField("type").as[String].getOrElse("")
          if (payloadType == "view_submission") dispatchPayload[ViewSubmissionPayload](json, handleViewSubmissionPayload)
          else dispatchPayload[InteractionPayload](json, handleInteractionPayload)
        }
      case EnvelopeType.SlashCommands => envelope.payload.traverse_(dispatchPayload[SlashCommandPayload](_, handleSlashCommandPayload))
      case _                          => monad.unit(())
    }
    dispatch.handleError { case e =>
      errorHandlerRef.get.flatMap(handler => handler(e))
    }
  }

  private def dispatchPayload[A: Decoder](json: Json, handler: A => F[Unit]): F[Unit] =
    json.as[A] match {
      case Right(a)  => handler(a)
      case Left(err) => monad.blocking(logger.warn(s"Failed to decode payload: ${err.getMessage}"))
    }

  private[slack] def handleInteractionPayload(payload: InteractionPayload): F[Unit] = {
    handlersRef.get.flatMap { handlers =>
      payload.actions.traverse_ { action =>
        val click = ButtonClick[String](
          payload = payload,
          action = action,
          value = action.value.getOrElse(""),
        )

        handlers.get(ButtonId[String](action.action_id)).traverse_(handler => handler(click))
      }
    }
  }

  private[slack] def handleSlashCommandPayload(payload: SlashCommandPayload): F[Unit] = {
    val normalized = CommandName(payload.command)
    commandHandlersRef.get.flatMap { commands =>
      commands.get(normalized).traverse_ { entry =>
        entry.handler(payload).flatMap {
          case CommandResponse.Silent       => monad.unit(())
          case CommandResponse.Ephemeral(t) =>
            withClient(_.respondToCommand(payload.response_url, t, ResponseType.Ephemeral))
          case CommandResponse.InChannel(t) =>
            withClient(_.respondToCommand(payload.response_url, t, ResponseType.InChannel))
        }
      }
    }
  }

  private[slack] def handleViewSubmissionPayload(payload: ViewSubmissionPayload): F[Unit] = {
    val callbackId = FormId[Any, Any](payload.view.callback_id.getOrElse(""))
    formHandlersRef.get.flatMap { forms =>
      forms.get(callbackId).traverse_ { entry =>
        val values = payload.view.state.map(_.values).getOrElse(Map.empty)
        entry.formDef.parse(values) match {
          case Left(error)   =>
            monad.error(
              new RuntimeException(
                s"Form parse error for form '${callbackId.value}': $error. This usually indicates a mismatch between the FormDef fields and the submitted form state.",
              ),
            )
          case Right(parsed) =>
            val rawMeta     = payload.view.private_metadata.getOrElse("")
            val decodedMeta = entry.decodeMetadata(rawMeta)
            val submission  = FormSubmission(payload = payload, values = parsed, metadata = decodedMeta)
            entry.handler(submission)
        }
      }
    }
  }

  private def buildBlocks(text: String, buttons: Seq[Button]): Option[List[Block]] =
    if (buttons.nonEmpty) {
      Some(
        List(
          SectionBlock(
            text = Some(MarkdownTextObject(text = text)),
          ),
          ActionsBlock(
            elements = buttons.map(buttonToElement).toList,
          ),
        ),
      )
    } else None

  private def buttonToElement(button: Button): BlockElement =
    ButtonElement(
      text = PlainTextObject(text = button.label),
      action_id = button.actionId,
      value = NonEmptyString(button.value),
    )

}
