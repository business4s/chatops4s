package chatops4s.slack.api

import sttp.client4.*
import sttp.client4.circe.*
import sttp.monad.MonadError
import sttp.monad.syntax.*

class SlackToolingApi[F[_]](backend: Backend[F], token: SlackRefreshToken) {

  private given MonadError[F] = backend.monad
  private val baseUrl         = "https://slack.com/api"

  object tooling {
    object tokens {

      // https://docs.slack.dev/reference/methods/tooling.tokens.rotate
      def rotate(): F[SlackResponse[chatops4s.slack.api.tooling.tokens.RotateResponse]] =
        postForm("tooling.tokens.rotate", Map("refresh_token" -> token.value))
    }
  }

  private def postForm[Res: io.circe.Decoder](method: String, params: Map[String, String]): F[SlackResponse[Res]] =
    backend
      .send(
        basicRequest
          .post(uri"$baseUrl/$method")
          .header("Authorization", s"Bearer ${token.value}")
          .body(params)
          .response(asJsonAlways[SlackResponse[Res]]),
      )
      .map(_.body)
      .map {
        case Right(res) => SlackResponse.withMethod(method, res)
        case Left(err)  => throw SlackApiError(method, "deserialization_error", List(err.toString))
      }
}
