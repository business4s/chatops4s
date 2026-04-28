package chatops4s.slack.api

import io.circe.Json

case class SlackApiError(error: String, details: List[String] = Nil, response: Option[Json] = None)
    extends RuntimeException(
      s"Slack API error: $error" +
        (if (details.nonEmpty) s". ${details.mkString("; ")}" else "") +
        response.fold("")(r => s". Full response: ${r.noSpaces}"),
    )
