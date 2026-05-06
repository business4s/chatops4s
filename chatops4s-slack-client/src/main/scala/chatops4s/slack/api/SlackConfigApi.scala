package chatops4s.slack.api

import io.circe.syntax.*
import sttp.client4.*
import sttp.client4.circe.*
import sttp.monad.MonadError
import sttp.monad.syntax.*

class SlackConfigApi[F[_]](backend: Backend[F], token: SlackConfigToken) {

  private given MonadError[F] = backend.monad
  private val baseUrl         = "https://slack.com/api"

  object apps {
    object manifest {

      // https://docs.slack.dev/reference/methods/apps.manifest.create
      def create(req: chatops4s.slack.api.apps.manifest.CreateRequest): F[SlackResponse[chatops4s.slack.api.apps.manifest.CreateResponse]] =
        post("apps.manifest.create", req)

      // https://docs.slack.dev/reference/methods/apps.manifest.update
      def update(req: chatops4s.slack.api.apps.manifest.UpdateRequest): F[SlackResponse[chatops4s.slack.api.apps.manifest.UpdateResponse]] =
        post("apps.manifest.update", req)

      // https://docs.slack.dev/reference/methods/apps.manifest.validate
      def validate(req: chatops4s.slack.api.apps.manifest.ValidateRequest): F[SlackResponse[chatops4s.slack.api.apps.manifest.ValidateResponse]] =
        post("apps.manifest.validate", req)

      // https://docs.slack.dev/reference/methods/apps.manifest.export
      def `export`(req: chatops4s.slack.api.apps.manifest.ExportRequest): F[SlackResponse[chatops4s.slack.api.apps.manifest.ExportResponse]] =
        post("apps.manifest.export", req)

      // https://docs.slack.dev/reference/methods/apps.manifest.delete
      def delete(req: chatops4s.slack.api.apps.manifest.DeleteRequest): F[SlackResponse[chatops4s.slack.api.apps.manifest.DeleteResponse]] =
        post("apps.manifest.delete", req)
    }
  }

  private def post[Req: io.circe.Encoder, Res: io.circe.Decoder](method: String, req: Req): F[SlackResponse[Res]] =
    backend
      .send(
        basicRequest
          .post(uri"$baseUrl/$method")
          .header("Authorization", s"Bearer ${token.value}")
          .contentType("application/json")
          .body(stringifyManifest(req.asJson.deepDropNullValues).noSpaces)
          .response(asJsonAlways[SlackResponse[Res]]),
      )
      .map(_.body)
      .map {
        case Right(res) => SlackResponse.withMethod(method, res)
        case Left(err)  => throw SlackApiError(method, "deserialization_error", List(err.toString))
      }

  /** Slack requires the `manifest` field to be "a JSON app manifest encoded as a string", not a nested JSON object.
    *
    * @see
    *   [[https://docs.slack.dev/reference/methods/apps.manifest.create]]
    */
  private def stringifyManifest(json: io.circe.Json): io.circe.Json =
    json.mapObject { obj =>
      obj("manifest") match {
        case Some(v) if !v.isString => obj.add("manifest", io.circe.Json.fromString(v.noSpaces))
        case _                      => obj
      }
    }
}
