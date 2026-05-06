package chatops4s.slack.api

import io.circe.Json

case class SlackApiError(method: String, error: String, details: List[String] = Nil, response: Option[Json] = None)
    extends RuntimeException(
      s"Slack API error in $method: $error" +
        (if (details.nonEmpty) s". ${details.mkString("; ")}" else ""),
    )
