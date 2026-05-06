package chatops4s.slack

import chatops4s.slack.api.{
  ChannelId,
  Message,
  ResponseType,
  SlackApi,
  SlackBotToken,
  Timestamp,
  TriggerId,
  UserId,
  chat,
  conversations,
  reactions,
  users,
  views,
}
import chatops4s.slack.api.socket.CommandResponsePayload
import chatops4s.slack.api.blocks.{Block, View}
import io.circe.Json
import io.circe.syntax.*
import sttp.client4.*
import sttp.monad.syntax.*
import chatops4s.slack.monadSyntax.*

class SlackClient[F[_]](token: SlackBotToken, backend: Backend[F]) {

  private given monad: sttp.monad.MonadError[F] = backend.monad

  private val api = new SlackApi[F](backend, token)

  def postMessage(
      channel: String,
      text: String,
      blocks: Option[List[Block]],
      threadTs: Option[Timestamp],
      metadata: Option[Json] = None,
  ): F[MessageId] = {
    val request = chat.PostMessageRequest(
      channel = channel,
      text = text,
      blocks = blocks,
      thread_ts = threadTs,
      metadata = metadata,
    )

    api.chat.postMessage(request).map { resp =>
      val r = resp.okOrThrow
      MessageId(r.channel, r.ts)
    }
  }

  def respondToCommand(responseUrl: String, text: String, responseType: ResponseType): F[Unit] = {
    val body = CommandResponsePayload(response_type = responseType, text = text)

    val req = basicRequest
      .post(uri"$responseUrl")
      .contentType("application/json")
      .body(body.asJson.deepDropNullValues.noSpaces)

    backend.send(req).void
  }

  def deleteMessage(messageId: MessageId): F[Unit] =
    api.chat
      .delete(chat.DeleteRequest(channel = messageId.channel, ts = messageId.ts))
      .map(_.okOrThrow)
      .void

  def addReaction(messageId: MessageId, emoji: String): F[Unit] =
    api.reactions
      .add(reactions.AddRequest(channel = messageId.channel, timestamp = messageId.ts, name = emoji))
      .map(_.okOrThrow)
      .void

  def removeReaction(messageId: MessageId, emoji: String): F[Unit] =
    api.reactions
      .remove(reactions.RemoveRequest(channel = messageId.channel, timestamp = messageId.ts, name = emoji))
      .map(_.okOrThrow)
      .void

  def postEphemeral(channel: String, userId: UserId, text: String): F[Unit] =
    api.chat
      .postEphemeral(chat.PostEphemeralRequest(channel = channel, user = userId, text = text))
      .map(_.okOrThrow)
      .void

  def openView(triggerId: TriggerId, view: View): F[Unit] =
    api.views
      .open(views.OpenRequest(trigger_id = triggerId, view = view))
      .map(_.okOrThrow)
      .void

  def getUserInfo(userId: UserId): F[users.UserInfo] =
    api.users.info(users.InfoRequest(user = userId)).map(_.okOrThrow.user)

  def updateMessage(messageId: MessageId, text: String, blocks: Option[List[Block]]): F[MessageId] = {
    val request = chat.UpdateRequest(
      channel = messageId.channel,
      ts = messageId.ts,
      text = Some(text),
      blocks = blocks,
    )

    api.chat.update(request).map { resp =>
      val _ = resp.okOrThrow
      messageId
    }
  }

  def fetchRecentMessages(channel: ChannelId, limit: Int): F[List[Message]] = {
    val request = conversations.HistoryRequest(
      channel = channel,
      limit = Some(limit),
      include_all_metadata = Some(true),
    )
    api.conversations.history(request).map(_.okOrThrow.messages)
  }

  def fetchThreadReplies(channel: ChannelId, threadTs: Timestamp, limit: Int): F[List[Message]] = {
    val request = conversations.RepliesRequest(
      channel = channel,
      ts = threadTs,
      limit = Some(limit),
      include_all_metadata = Some(true),
    )
    api.conversations.replies(request).map(_.okOrThrow.messages)
  }

  // BotChannelsOnly uses Tier 3 `users.conversations` -- vastly fewer results than `conversations.list`
  // when the workspace has many channels but the bot is in few. AllChannels paginates the workspace.
  def listChannels(lookup: ChannelLookup, limit: Int = 200): F[ConversationsPage[F]] =
    fetchChannelsPage(lookup, cursor = None, limit)

  private def fetchChannelsPage(lookup: ChannelLookup, cursor: Option[String], limit: Int): F[ConversationsPage[F]] = {
    val types     = Some("public_channel,private_channel")
    val responseF = lookup match {
      case ChannelLookup.BotChannelsOnly =>
        api.users.conversationsList(
          users.ConversationsListRequest(limit = Some(limit), cursor = cursor, types = types, exclude_archived = Some(true)),
        )
      case ChannelLookup.AllChannels     =>
        api.conversations.list(
          conversations.ListRequest(limit = Some(limit), cursor = cursor, types = types, exclude_archived = Some(true)),
        )
    }
    responseF.map(_.okOrThrow).map { resp =>
      val nextCursor = resp.response_metadata.flatMap(_.next_cursor).filter(_.nonEmpty)
      ConversationsPage(resp.channels, nextCursor.map(c => fetchChannelsPage(lookup, Some(c), limit)))
    }
  }
}

case class ConversationsPage[F[_]](
    channels: List[conversations.ConversationInfo],
    next: Option[F[ConversationsPage[F]]],
)
