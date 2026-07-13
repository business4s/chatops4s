package chatops4s.slack

import org.scalatest.Assertions

import java.nio.file.{Files, Path, Paths}

object SnapshotTest {

  // Set by Tests.Setup in build.sbt; the fallback covers IDE test runs launched from the repo root.
  private val testResourcesPath: Path = {
    val fromProp = sys.props.get("chatops4s.test.resources").map(Paths.get(_))
    val fallback = Paths.get("chatops4s-slack", "src", "test", "resources")
    fromProp
      .orElse(Option.when(Files.isDirectory(fallback))(fallback))
      .getOrElse(
        sys.error("Cannot locate test resources: run tests through sbt or from the repo root (chatops4s.test.resources property not set)"),
      )
  }

  def testSnapshot(content: String, path: String): Unit = {
    val filePath    = testResourcesPath.resolve(path)
    val existingOpt = Option.when(Files.exists(filePath)) {
      Files.readString(testResourcesPath.resolve(path))
    }

    val isOk = existingOpt.contains(content)

    if (!isOk) {
      Files.createDirectories(filePath.getParent)
      Files.writeString(filePath, content)
      Assertions.fail(s"Snapshot $path was not matching. A new value has been written to $filePath.")
    }
  }
}
