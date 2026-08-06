import org.typelevel.scalacoptions.ScalacOptions

lazy val `chatops4s` = (project in file("."))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
  )
  .aggregate(
    `chatops4s-slack-client`,
    `chatops4s-slack`,
    `chatops4s-examples`,
  )

lazy val `chatops4s-slack-client` = (project in file("chatops4s-slack-client"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"                      %% "circe-core"    % "0.14.16",
      "io.circe"                      %% "circe-generic" % "0.14.16",
      "io.circe"                      %% "circe-parser"  % "0.14.16",
      "com.softwaremill.sttp.client4" %% "core"          % "4.0.26",
      "com.softwaremill.sttp.client4" %% "circe"         % "4.0.26",
    ),
  )

lazy val `chatops4s-slack` = (project in file("chatops4s-slack"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.slf4j"                      % "slf4j-api"   % "2.0.18",
      "org.typelevel"                 %% "cats-effect" % "3.7.0"  % Test,
      "com.softwaremill.sttp.client4" %% "cats"        % "4.0.26" % Test,
    ),
    Test / parallelExecution := false,
    // Snapshot tests need the real src/test/resources dir (not the classpath copy) to rewrite snapshots.
    Test / testOptions += {
      val resources = (Test / resourceDirectory).value.getAbsolutePath
      Tests.Setup(() => sys.props("chatops4s.test.resources") = resources)
    },
  )
  .dependsOn(`chatops4s-slack-client`)

lazy val `chatops4s-examples` = (project in file("chatops4s-examples"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"                 %% "cats-effect"     % "3.7.0",
      "com.softwaremill.sttp.client4" %% "fs2"             % "4.0.26",
      "ch.qos.logback"                 % "logback-classic" % "1.6.1",
    ),
    Test / parallelExecution := false,
    publish / skip           := true,
    run / fork               := true,
  )
  .dependsOn(`chatops4s-slack`)

lazy val stableVersion = taskKey[String]("stableVersion")
stableVersion := {
  if (isVersionStable.value && !isSnapshot.value) version.value
  else previousStableVersion.value.getOrElse("unreleased")
}

lazy val stableVersionFile = settingKey[File]("File that writeStableVersion writes to")
stableVersionFile := (ThisBuild / baseDirectory).value / "target" / "stable-version.txt"

// Writing to a file instead of printing, because sbt's stdout is not machine-readable
// (ANSI escapes, progress lines, launcher output). Consumed by the website build.
lazy val writeStableVersion = taskKey[Unit]("Writes stableVersion to stableVersionFile")
writeStableVersion := {
  val target = stableVersionFile.value
  IO.write(target, stableVersion.value)
  streams.value.log.info(s"Wrote stable version to $target")
}

lazy val commonSettings = Seq(
  scalaVersion  := "3.8.1",
  scalacOptions ++= Seq(
    "-no-indent",
    "-Xmax-inlines",
    "64",
    "-explain-cyclic",
    "-Ydebug-cyclic",
  ),
  Test / tpolecatExcludeOptions ++= Set(
    ScalacOptions.warnValueDiscard,
    ScalacOptions.warnNonUnitStatement,
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest"                     % "3.2.20" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.8.0"  % Test,
  ),
  organization  := "org.business4s",
  homepage      := Some(url("https://business4s.github.io/chatops4s/")),
  licenses      := List(License.MIT),
  developers    := List(
    Developer(
      "Krever",
      "Voytek Pituła",
      "w.pitula@gmail.com",
      url("https://v.pitula.me"),
    ),
  ),
  versionScheme := Some("semver-spec"),
)

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias("prePR", List("compile", "Test / compile", "test", "scalafmtAll").mkString(";", ";", ""))
