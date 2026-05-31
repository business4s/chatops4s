package example.docs

import cats.effect.IO
import chatops4s.slack.{ConfigTokenStore, RefreshingSlackConfigApi, SetupVerification, SlackSetup}
import chatops4s.slack.api.{SlackConfigToken, SlackRefreshToken, apps}
import chatops4s.slack.api.manifest.{DisplayInformation, SlackAppManifest}
import sttp.client4.Backend

private object AppManagementPage {

  def validateSetupExample(slack: SlackSetup[IO]): IO[Unit] = {
    // start_validate_setup
    // Fails if manifest is new or changed — recommended for development.
    slack.validateSetup("MyApp", "slack-manifest.yml")
    // end_validate_setup
  }

  def checkSetupExample(slack: SlackSetup[IO]): IO[Unit] = {
    // start_check_setup
    for {
      result <- slack.checkSetup("MyApp", "slack-manifest.yml")
      _      <- result match {
                  case SetupVerification.UpToDate   =>
                    IO.println("Manifest is up to date.")
                  case v: SetupVerification.Created =>
                    IO.println(s"Manifest created at ${v.path}") *>
                      IO.println(s"Create your app: ${v.createAppUrl}")
                  case v: SetupVerification.Changed =>
                    IO.println(s"Manifest changed at ${v.path}") *>
                      IO.println(v.diff)
                }
    } yield ()
    // end_check_setup
  }

  def refreshingConfigApiExample(backend: Backend[IO]): IO[Unit] = {
    given sttp.monad.MonadError[IO] = backend.monad
    val configToken                 = SlackConfigToken.unsafe(sys.env("SLACK_CONFIG_TOKEN"))
    val refreshToken                = SlackRefreshToken.unsafe(sys.env("SLACK_REFRESH_TOKEN"))

    // start_refreshing_config_api
    val initial = ConfigTokenStore.TokenPair(configToken, refreshToken)
    for {
      store      <- ConfigTokenStore.inMemory[IO](initial)
      refreshing <- RefreshingSlackConfigApi.create[IO](backend, store)
      // Use withApi to get a SlackConfigApi with a fresh token:
      _          <- refreshing.withApi { api =>
                      api.apps.manifest.create(
                        apps.manifest.CreateRequest(
                          manifest = SlackAppManifest(
                            display_information = DisplayInformation(name = "MyApp"),
                          ).addBotScopes("chat:write"),
                        ),
                      )
                    }
    } yield ()
    // end_refreshing_config_api
  }

}
