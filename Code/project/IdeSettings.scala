import sbt._
import Keys._
//import org.sbtidea.SbtIdeaPlugin._
import sbtide.Keys._

object IdeSettings {

  private object excludes {
    val moduleTargets = List(
      "base",
      "base-util",
      "base-db",
      "base-test",
      "taskman",
      "taskman-api-logic",
      "taskman-api",
      "taskman-server-logic",
      "taskman-server-schema",
      "taskman-server",
      "webapp",
      "webapp-macro",
      "webapp-base",
      "webapp-base-member",
      "webapp-base-test",
      "webapp-client-public",
      "webapp-client-home",
      "webapp-client-ww-api",
      "webapp-client-ww",
      "webapp-client-project",
      "webapp-gen",
      "webapp-server-logic",
      "webapp-server",
      "benchmark",
      "utils")
      .flatMap(r => List(r, r + "/jvm", r + "/js", r + "/shared"))
      .map(_ + "/target")

    def common = "target" :: moduleTargets
    def root   = common ++ List(".idea", ".idea_modules", ".settings", "target", "log", ".bower")
    def webapp = List("vendor")
  }

  private def prefix(p: String)(ss: List[String]): List[String] = ss.map(p + _)
  private def prefixT(p: String)(ss: List[String]) = ss ++ prefix(p)(ss)

  def allExcludes = excludes.root ++ prefixT("webapp-server/")(excludes.webapp)

  def settingsForRoot = (p: Project) => p.settings(
//    ideaProjectName := "ShipReq",
//    ideaExcludeFolders := excludes.root ++ prefixT("webapp-server/")(excludes.webapp)
    ideExcludedDirectories := allExcludes.map(file)
  )

  /*
  def eclipseSettings = (p: Project) => {
    import com.typesafe.sbteclipse.core.EclipsePlugin._
    p.settings(
      EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17),
      EclipseKeys.withSource := true,
      EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
      // Prevent src/main/java appearing in .classpath
      unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
      // Prevent src/test/java appearing in .classpath
      unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))
      // This is a better way of doing it:
      unmanagedSourceDirectories in Compile ~= { _.filter(_.exists) }
      unmanagedSourceDirectories in Test ~= { _.filter(_.exists) }
    )
  }
  */
}
