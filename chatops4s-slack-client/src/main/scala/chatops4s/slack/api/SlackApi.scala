package chatops4s.slack.api

import chatops4s.slack.api.chat.{
  DeleteRequest,
  DeleteResponse,
  PostEphemeralRequest,
  PostEphemeralResponse,
  PostMessageRequest,
  PostMessageResponse,
  UpdateRequest,
  UpdateResponse,
}
import chatops4s.slack.api.conversations.{HistoryRequest, HistoryResponse, ListRequest, ListResponse, RepliesRequest, RepliesResponse}
import chatops4s.slack.api.reactions.{AddRequest, AddResponse, RemoveRequest, RemoveResponse}
import chatops4s.slack.api.users.{
  ConversationsListRequest as UsersConversationsListRequest,
  InfoRequest as UsersInfoRequest,
  InfoResponse as UsersInfoResponse,
}
import chatops4s.slack.api.views.{OpenRequest, OpenResponse}
import io.circe.syntax.*
import sttp.client4.*
import sttp.client4.circe.*
import sttp.monad.syntax.*

class SlackApi[F[_]](backend: Backend[F], token: SlackBotToken) {

  private given sttp.monad.MonadError[F] = backend.monad
  private val baseUrl                    = "https://slack.com/api"

  object chat {

    // https://docs.slack.dev/reference/methods/chat.postMessage
    def postMessage(req: PostMessageRequest): F[SlackResponse[PostMessageResponse]] = post("chat.postMessage", req)

    // https://docs.slack.dev/reference/methods/chat.update
    def update(req: UpdateRequest): F[SlackResponse[UpdateResponse]] = post("chat.update", req)

    // https://docs.slack.dev/reference/methods/chat.delete
    def delete(req: DeleteRequest): F[SlackResponse[DeleteResponse]] = post("chat.delete", req)

    // https://docs.slack.dev/reference/methods/chat.postEphemeral
    def postEphemeral(req: PostEphemeralRequest): F[SlackResponse[PostEphemeralResponse]] = post("chat.postEphemeral", req)
  }

  object reactions {

    // https://docs.slack.dev/reference/methods/reactions.add
    def add(req: AddRequest): F[SlackResponse[AddResponse]] = post("reactions.add", req)

    // https://docs.slack.dev/reference/methods/reactions.remove
    def remove(req: RemoveRequest): F[SlackResponse[RemoveResponse]] = post("reactions.remove", req)
  }

  object views {

    // https://docs.slack.dev/reference/methods/views.open
    def open(req: OpenRequest): F[SlackResponse[OpenResponse]] = post("views.open", req)
  }

  object conversations {

    // The conversations.* family seems to silently drop fields when the body is application/json
    // (booleans like `include_all_metadata`, and even required fields like `channel`/`ts` on `replies`),
    // so we form-encode all of them.

    // https://docs.slack.dev/reference/methods/conversations.history
    def history(req: HistoryRequest): F[SlackResponse[HistoryResponse]] = postForm("conversations.history", req)

    // https://docs.slack.dev/reference/methods/conversations.replies
    def replies(req: RepliesRequest): F[SlackResponse[RepliesResponse]] = postForm("conversations.replies", req)

    // https://docs.slack.dev/reference/methods/conversations.list
    def list(req: ListRequest): F[SlackResponse[ListResponse]] = postForm("conversations.list", req)
  }

  object users {

    // https://docs.slack.dev/reference/methods/users.info
    def info(req: UsersInfoRequest): F[SlackResponse[UsersInfoResponse]] =
      get("users.info", Map("user" -> req.user.value))

    // https://docs.slack.dev/reference/methods/users.conversations
    // Sent form-encoded for the same reason as conversations.list.
    def conversationsList(req: UsersConversationsListRequest): F[SlackResponse[ListResponse]] =
      postForm("users.conversations", req)
  }

  private def get[Res: io.circe.Decoder](method: String, params: Map[String, String]): F[SlackResponse[Res]] =
    backend
      .send(
        basicRequest
          .get(uri"$baseUrl/$method?$params")
          .header("Authorization", s"Bearer ${token.value}")
          .response(asJsonAlways[SlackResponse[Res]]),
      )
      .map(_.body)
      .map {
        case Right(res) => SlackResponse.withMethod(method, res)
        case Left(err)  => throw SlackApiError(method, "deserialization_error", List(err.toString))
      }

  private def post[Req: io.circe.Encoder, Res: io.circe.Decoder](method: String, req: Req): F[SlackResponse[Res]] =
    sendPost(method)(_.contentType("application/json").body(req.asJson.deepDropNullValues.noSpaces))

  // Slack's older list/read endpoints (conversations.list, users.conversations, ...) silently drop
  // boolean params when the body is application/json. Form-encoding is the documented workaround.
  private def postForm[Req: io.circe.Encoder, Res: io.circe.Decoder](method: String, req: Req): F[SlackResponse[Res]] =
    sendPost(method)(_.body(jsonToFormParams(req.asJson.deepDropNullValues)))

  private def sendPost[Res: io.circe.Decoder](
      method: String,
  )(withBody: Request[Either[String, String]] => Request[Either[String, String]]): F[SlackResponse[Res]] =
    backend
      .send(
        withBody(
          basicRequest
            .post(uri"$baseUrl/$method")
            .header("Authorization", s"Bearer ${token.value}"),
        ).response(asJsonAlways[SlackResponse[Res]]),
      )
      .map(_.body)
      .map {
        case Right(res) => SlackResponse.withMethod(method, res)
        case Left(err)  => throw SlackApiError(method, "deserialization_error", List(err.toString))
      }

  // Only handles primitive fields. Throws on nested objects/arrays so a future request DTO that
  // adds one fails loudly here instead of silently posting a JSON-encoded string to Slack.
  // TODO we should do small internal derived typeclass instead
  private def jsonToFormParams(json: io.circe.Json): Map[String, String] =
    json.asObject.fold(Map.empty[String, String]) { obj =>
      obj.toMap.collect {
        case (k, v) if !v.isNull =>
          require(!v.isObject && !v.isArray, s"postForm cannot encode nested field '$k'; use post (JSON) instead")
          k -> v.asString.getOrElse(v.noSpaces)
      }
    }
}
