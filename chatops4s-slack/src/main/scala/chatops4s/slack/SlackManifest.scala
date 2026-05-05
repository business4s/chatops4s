package chatops4s.slack

import chatops4s.slack.api.manifest.*

private[slack] object SlackManifest {

  def generate(
      appName: String,
      commands: Map[String, (String, String)],
      hasInteractivity: Boolean,
      extraBotScopes: Set[String] = Set.empty,
  ): SlackAppManifest = {
    val baseScopes = List(
      "chat:write",
      "chat:write.public",
      "users:read",
      "users:read.email",
      "reactions:write",
    )

    val withCommands =
      if (commands.nonEmpty) baseScopes :+ "commands"
      else baseScopes

    val extras    = extraBotScopes.toList.sorted.filterNot(withCommands.contains)
    val botScopes = withCommands ++ extras

    val slashCommands =
      if (commands.nonEmpty)
        Some(commands.toList.sortBy(_._1).map { case (name, (desc, hint)) =>
          SlashCommand(
            command = s"/$name",
            description = desc,
            should_escape = Some(false),
            usage_hint = if (hint.nonEmpty) Some(hint) else None,
          )
        })
      else None

    val interactivity =
      if (hasInteractivity) Some(Interactivity(is_enabled = Some(true)))
      else None

    SlackAppManifest(
      display_information = DisplayInformation(name = appName),
      features = Features(
        bot_user = Some(BotUser(display_name = appName, always_online = Some(true))),
        slash_commands = slashCommands,
      ),
      oauth_config = OauthConfig(
        scopes = Some(OauthScopes(bot = Some(botScopes))),
      ),
      settings = ManifestSettings(
        interactivity = interactivity,
        org_deploy_enabled = Some(false),
        socket_mode_enabled = Some(true),
        token_rotation_enabled = Some(false),
      ),
    )
  }
}
