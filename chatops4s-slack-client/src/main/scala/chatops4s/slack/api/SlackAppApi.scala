package chatops4s.slack.api

import chatops4s.slack.api.apps.ConnectionsOpenResponse
import io.circe.syntax.*
import sttp.client4.*
import sttp.client4.circe.*
import sttp.client4.ws.async.*
import sttp.monad.MonadError
import sttp.monad.syntax.*

class SlackAppApi[F[_]](backend: Backend[F], token: SlackAppToken) {

  private given sttp.monad.MonadError[F] = backend.monad
  private val baseUrl                    = "https://slack.com/api"

  object apps {
    object connections {

      // https://docs.slack.dev/reference/methods/apps.connections.open
      def open(): F[SlackResponse[ConnectionsOpenResponse]] =
        backend
          .send(
            basicRequest
              .post(uri"$baseUrl/apps.connections.open")
              .header("Authorization", s"Bearer ${token.value}")
              .contentType("application/x-www-form-urlencoded")
              .response(asJsonAlways[SlackResponse[ConnectionsOpenResponse]]),
          )
          .map(_.body)
          .map {
            case Right(res) => SlackResponse.withMethod("apps.connections.open", res)
            case Left(err)  => throw SlackApiError("apps.connections.open", "deserialization_error", List(err.toString))
          }
    }
  }
}

object SlackAppApi {

  // https://docs.slack.dev/apis/events-api/using-socket-mode
  def connectToSocket[F[_]](url: String, backend: WebSocketBackend[F])(
      handler: socket.Envelope => F[Unit],
  ): F[socket.DisconnectReason] = {
    given monad: MonadError[F] = backend.monad
    basicRequest
      .get(uri"$url")
      .response(asWebSocketOrFail[F, socket.DisconnectReason] { ws =>
        def loop: F[socket.DisconnectReason] =
          ws.receiveText().flatMap { text =>
            io.circe.parser.decode[socket.Envelope](text) match {
              case Right(envelope) =>
                val ack = ws.sendText(socket.Ack(envelope.envelope_id).asJson.noSpaces)
                ack.flatMap(_ => handler(envelope)).flatMap(_ => loop)
              case Left(_)         =>
                io.circe.parser.decode[socket.Disconnect](text) match {
                  case Right(disconnect) => monad.unit(disconnect.reason)
                  case Left(_)           => loop // Malformed frames skipped to avoid killing the connection
                }
            }
          }
        loop
      })
      .send(backend)
      .map(_.body)
  }
}
