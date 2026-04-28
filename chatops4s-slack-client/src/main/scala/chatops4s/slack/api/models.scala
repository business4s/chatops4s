package chatops4s.slack.api

import chatops4s.slack.api.manifest.SlackAppManifest
import io.circe.{Codec, Decoder, Encoder, Json}

private type Block    = chatops4s.slack.api.blocks.Block
private type View     = chatops4s.slack.api.blocks.View
private type ViewType = chatops4s.slack.api.blocks.ViewType

private def maskToken(value: String): String =
  if (value.length <= 8) "***"
  else s"${value.take(4)}***${value.takeRight(4)}"

opaque type SlackBotToken = String
object SlackBotToken {
  def apply(value: String): Either[String, SlackBotToken] =
    if value.startsWith("xoxb-") then Right(value)
    else Left(s"SlackBotToken must start with 'xoxb-', got: ${maskToken(value)}")
  def unsafe(value: String): SlackBotToken                = value
  extension (x: SlackBotToken) def value: String          = x
}

opaque type SlackAppToken = String
object SlackAppToken {
  def apply(value: String): Either[String, SlackAppToken] =
    if value.startsWith("xapp-") then Right(value)
    else Left(s"SlackAppToken must start with 'xapp-', got: ${maskToken(value)}")
  def unsafe(value: String): SlackAppToken                = value
  extension (x: SlackAppToken) def value: String          = x
}

opaque type SlackConfigToken = String
object SlackConfigToken {
  def apply(value: String): Either[String, SlackConfigToken] =
    if value.startsWith("xoxe.xoxp-") then Right(value)
    else Left(s"SlackConfigToken must start with 'xoxe.xoxp-', got: ${maskToken(value)}")
  def unsafe(value: String): SlackConfigToken                = value
  extension (x: SlackConfigToken) def value: String          = x
  given Encoder[SlackConfigToken]                            = Encoder[String]
  given Decoder[SlackConfigToken]                            = Decoder[String].emap(apply)
}

opaque type SlackRefreshToken = String
object SlackRefreshToken {
  def apply(value: String): Either[String, SlackRefreshToken] =
    if value.startsWith("xoxe-") then Right(value)
    else Left(s"SlackRefreshToken must start with 'xoxe-', got: ${maskToken(value)}")
  def unsafe(value: String): SlackRefreshToken                = value
  extension (x: SlackRefreshToken) def value: String          = x
  given Encoder[SlackRefreshToken]                            = Encoder[String]
  given Decoder[SlackRefreshToken]                            = Decoder[String].emap(apply)
}

opaque type NonEmptyString <: String = String
object NonEmptyString {
  def apply(value: String): Option[NonEmptyString] = if (value.isEmpty) None else Some(value)
  extension (x: NonEmptyString) def value: String  = x
  given Encoder[NonEmptyString]                    = Encoder[String]
  given Decoder[NonEmptyString]                    = Decoder[String].emap(s => if (s.isEmpty) Left("expected non-empty string") else Right(s))
}

opaque type ChannelId = String
object ChannelId {
  def apply(value: String): ChannelId          = value
  extension (x: ChannelId) def value: String   = x
  extension (x: ChannelId) def mention: String = s"<#$x>"
  given Encoder[ChannelId]                     = Encoder[String]
  given Decoder[ChannelId]                     = Decoder[String]
}

opaque type UserId = String
object UserId {
  def apply(value: String): UserId          = value
  extension (x: UserId) def value: String   = x
  extension (x: UserId) def mention: String = s"<@$x>"
  given Encoder[UserId]                     = Encoder[String]
  given Decoder[UserId]                     = Decoder[String]
}

opaque type TeamId = String
object TeamId {
  def apply(value: String): TeamId        = value
  extension (x: TeamId) def value: String = x
  given Encoder[TeamId]                   = Encoder[String]
  given Decoder[TeamId]                   = Decoder[String]
}

opaque type ConversationId = String
object ConversationId {
  def apply(value: String): ConversationId        = value
  extension (x: ConversationId) def value: String = x
  given Encoder[ConversationId]                   = Encoder[String]
  given Decoder[ConversationId]                   = Decoder[String]
}

opaque type Timestamp = String
object Timestamp {
  def apply(value: String): Timestamp        = value
  extension (x: Timestamp) def value: String = x
  given Encoder[Timestamp]                   = Encoder[String]
  given Decoder[Timestamp]                   = Decoder[String]
}

opaque type Email = String
object Email {
  def apply(value: String): Email        = value
  extension (x: Email) def value: String = x
  given Encoder[Email]                   = Encoder[String]
  given Decoder[Email]                   = Decoder[String]
}

opaque type TriggerId = String
object TriggerId {
  def apply(value: String): TriggerId        = value
  extension (x: TriggerId) def value: String = x
  given Encoder[TriggerId]                   = Encoder[String]
  given Decoder[TriggerId]                   = Decoder[String]
}

enum ParseMode   {
  case Full, Raw
}
object ParseMode {
  private val mapping      = Map("full" -> Full, "none" -> Raw)
  private val reverse      = mapping.map(_.swap)
  given Encoder[ParseMode] = Encoder[String].contramap(reverse)
  given Decoder[ParseMode] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown parse mode: $s"))
}

enum ResponseType   {
  case InChannel, Ephemeral
}
object ResponseType {
  private val mapping         = Map("in_channel" -> InChannel, "ephemeral" -> Ephemeral)
  private val reverse         = mapping.map(_.swap)
  given Encoder[ResponseType] = Encoder[String].contramap(reverse)
  given Decoder[ResponseType] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown response type: $s"))
}

// https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/Message.java
case class Message(
    `type`: Option[String] = None,
    subtype: Option[String] = None,
    team: Option[TeamId] = None,
    channel: Option[ChannelId] = None,
    user: Option[UserId] = None,
    username: Option[String] = None,
    text: Option[String] = None,
    blocks: Option[List[Block]] = None,
    attachments: Option[List[Json]] = None,
    ts: Option[Timestamp] = None,
    thread_ts: Option[Timestamp] = None,
    app_id: Option[String] = None,
    bot_id: Option[String] = None,
    bot_profile: Option[Json] = None,
    display_as_bot: Option[Boolean] = None,
    icons: Option[Json] = None,
    file: Option[Json] = None,
    files: Option[List[Json]] = None,
    upload: Option[Boolean] = None,
    parent_user_id: Option[UserId] = None,
    client_msg_id: Option[String] = None,
    edited: Option[Json] = None,
    unfurl_links: Option[Boolean] = None,
    unfurl_media: Option[Boolean] = None,
    is_thread_broadcast: Option[Boolean] = None,
    is_locked: Option[Boolean] = None,
    reply_count: Option[Int] = None,
    reply_users: Option[List[UserId]] = None,
    reply_users_count: Option[Int] = None,
    latest_reply: Option[Timestamp] = None,
    subscribed: Option[Boolean] = None,
    hidden: Option[Boolean] = None,
    is_starred: Option[Boolean] = None,
    pinned_to: Option[List[String]] = None,
    reactions: Option[List[Json]] = None,
    metadata: Option[Json] = None,
    room: Option[Json] = None,
) derives Codec.AsObject

sealed trait SlackResponse[+T] {
  def okOrThrow: T
}

object SlackResponse {
  case class Ok[+T](value: T)                                     extends SlackResponse[T]       {
    def okOrThrow: T = value
  }
  case class Err(error: String, details: List[String], raw: Json) extends SlackResponse[Nothing] {
    def okOrThrow: Nothing = throw SlackApiError(error, details, Some(raw))
  }

  given [T: Decoder]: Decoder[SlackResponse[T]] = Decoder.instance { cursor =>
    cursor.get[Boolean]("ok").flatMap {
      case true  => cursor.as[T].map(Ok(_))
      case false =>
        cursor.getOrElse[String]("error")("unknown_error").map { error =>
          // Slack typically returns the same content under response_metadata.messages (with [ERROR]/[WARN]
          // prefixes) and the top-level `errors` array. Prefer messages; fall back to errors if absent.
          val details = cursor
            .downField("response_metadata")
            .downField("messages")
            .as[List[String]]
            .orElse(cursor.downField("errors").as[List[String]])
            .getOrElse(Nil)
          Err(error, details, cursor.value)
        }
    }
  }
}

object chat {

  case class PostMessageRequest(
      channel: String,
      text: String,
      blocks: Option[List[Block]] = None,
      attachments: Option[List[Json]] = None,
      thread_ts: Option[Timestamp] = None,
      unfurl_links: Option[Boolean] = None,
      unfurl_media: Option[Boolean] = None,
      mrkdwn: Option[Boolean] = None,
      metadata: Option[Json] = None,
      reply_broadcast: Option[Boolean] = None,
      parse: Option[ParseMode] = None,
      icon_emoji: Option[String] = None,
      icon_url: Option[String] = None,
      username: Option[String] = None,
  ) derives Codec.AsObject

  case class PostMessageResponse(
      channel: ChannelId,
      ts: Timestamp,
      message: Option[Message] = None,
      response_metadata: Option[ResponseMetadata] = None,
  ) derives Codec.AsObject

  case class UpdateRequest(
      channel: ChannelId,
      ts: Timestamp,
      text: Option[String] = None,
      blocks: Option[List[Block]] = None,
      attachments: Option[List[Json]] = None,
      reply_broadcast: Option[Boolean] = None,
      metadata: Option[Json] = None,
      parse: Option[ParseMode] = None,
      file_ids: Option[List[String]] = None,
  ) derives Codec.AsObject

  case class UpdateResponse(
      channel: ChannelId,
      ts: Timestamp,
      text: Option[String] = None,
      message: Option[Json] = None,
  ) derives Codec.AsObject

  case class DeleteRequest(
      channel: ChannelId,
      ts: Timestamp,
  ) derives Codec.AsObject

  case class DeleteResponse(
      channel: ChannelId,
      ts: Timestamp,
  ) derives Codec.AsObject

  case class PostEphemeralRequest(
      channel: String,
      user: UserId,
      text: String,
      blocks: Option[List[Block]] = None,
      attachments: Option[List[Json]] = None,
      thread_ts: Option[Timestamp] = None,
      parse: Option[ParseMode] = None,
      icon_emoji: Option[String] = None,
      icon_url: Option[String] = None,
      username: Option[String] = None,
  ) derives Codec.AsObject

  case class PostEphemeralResponse(
      message_ts: Timestamp,
  ) derives Codec.AsObject
}

case class ResponseMetadata(
    messages: Option[List[String]] = None,
) derives Codec.AsObject

object reactions {

  case class AddRequest(
      channel: ChannelId,
      timestamp: Timestamp,
      name: String,
  ) derives Codec.AsObject

  case class AddResponse() derives Codec.AsObject

  case class RemoveRequest(
      channel: ChannelId,
      timestamp: Timestamp,
      name: String,
  ) derives Codec.AsObject

  case class RemoveResponse() derives Codec.AsObject
}

object apps {

  case class ConnectionsOpenResponse(
      url: String,
  ) derives Codec.AsObject

  object manifest {
    case class CreateRequest(manifest: SlackAppManifest) derives Codec.AsObject
    case class CreateResponse(
        app_id: String,
        credentials: Option[AppCredentials] = None,
        oauth_authorize_url: Option[String] = None,
    ) derives Codec.AsObject

    case class AppCredentials(
        client_id: String,
        client_secret: String,
        verification_token: String,
        signing_secret: String,
    ) derives Codec.AsObject

    case class UpdateRequest(app_id: String, manifest: SlackAppManifest) derives Codec.AsObject
    case class UpdateResponse(
        app_id: Option[String] = None,
        permissions_updated: Option[Boolean] = None,
    ) derives Codec.AsObject

    case class ValidateRequest(manifest: SlackAppManifest, app_id: Option[String] = None) derives Codec.AsObject
    case class ValidateResponse(
        errors: Option[List[ManifestError]] = None,
    ) derives Codec.AsObject

    case class DeleteRequest(app_id: String) derives Codec.AsObject
    case class DeleteResponse() derives Codec.AsObject

    case class ExportRequest(app_id: String) derives Codec.AsObject
    case class ExportResponse(manifest: Option[SlackAppManifest] = None) derives Codec.AsObject

    case class ManifestError(
        code: Option[String] = None,
        message: String,
        pointer: Option[String] = None,
    ) derives Codec.AsObject
  }
}

object tooling {
  object tokens {
    case class RotateRequest(refresh_token: SlackRefreshToken) derives Codec.AsObject
    case class RotateResponse(
        token: SlackConfigToken,
        refresh_token: SlackRefreshToken,
        team_id: Option[String] = None,
        user_id: Option[String] = None,
        iat: Option[Int] = None,
        exp: Option[Int] = None,
    ) derives Codec.AsObject
  }
}

object views {

  case class OpenRequest(
      trigger_id: TriggerId,
      view: View,
  ) derives Codec.AsObject

  case class OpenResponse(
      view: Option[Json] = None,
  ) derives Codec.AsObject
}

object conversations {

  case class HistoryRequest(
      channel: ChannelId,
      limit: Option[Int] = None,
      include_all_metadata: Option[Boolean] = None,
      oldest: Option[Timestamp] = None,
      latest: Option[Timestamp] = None,
      inclusive: Option[Boolean] = None,
      cursor: Option[String] = None,
  ) derives Codec.AsObject

  case class HistoryResponse(
      messages: List[Message],
      has_more: Option[Boolean] = None,
      response_metadata: Option[ResponseMetadata] = None,
  ) derives Codec.AsObject

  case class RepliesRequest(
      channel: ChannelId,
      ts: Timestamp,
      limit: Option[Int] = None,
      include_all_metadata: Option[Boolean] = None,
      oldest: Option[Timestamp] = None,
      latest: Option[Timestamp] = None,
      inclusive: Option[Boolean] = None,
      cursor: Option[String] = None,
  ) derives Codec.AsObject

  case class RepliesResponse(
      messages: List[Message],
      has_more: Option[Boolean] = None,
      response_metadata: Option[ResponseMetadata] = None,
  ) derives Codec.AsObject
}

object users {

  // https://docs.slack.dev/reference/methods/users.info
  case class InfoRequest(user: UserId) derives Codec.AsObject

  case class UserProfile(
      email: Option[Email] = None,
      display_name: Option[String] = None,
      real_name: Option[String] = None,
  ) derives Codec.AsObject

  case class UserInfo(
      id: UserId,
      name: Option[String] = None,
      real_name: Option[String] = None,
      profile: Option[UserProfile] = None,
      is_bot: Option[Boolean] = None,
      tz: Option[String] = None,
  ) derives Codec.AsObject

  case class InfoResponse(user: UserInfo) derives Codec.AsObject
}

object oauth {

  case class AccessRequest(
      code: String,
      client_id: String,
      client_secret: String,
      redirect_uri: Option[String] = None,
  ) derives Codec.AsObject

  case class AccessResponse(
      access_token: String,
      token_type: Option[String] = None,
      scope: Option[String] = None,
      bot_user_id: Option[String] = None,
      app_id: Option[String] = None,
      team: Option[AccessResponseTeam] = None,
  ) derives Codec.AsObject

  case class AccessResponseTeam(
      name: Option[String] = None,
      id: Option[String] = None,
  ) derives Codec.AsObject
}
