import sbt._
import Keys._
import org.sbtidea.SbtIdeaPlugin._
import ShipReq.{Module, Root, Webapp}

object IdeSettings {

  object excludes {
    def common = List("project/target")
    def root   = common ++ List(".idea", ".idea_modules", ".settings")
    def webapp = common ++ List("vendor", "node_modules", ".bower", "src/it/scala", "src/main/webapp/assets/vendor/mathjax")
  }

  def prefix(p: String)(ss: List[String]): List[String] = ss.map(p + _)
  def prefixT(p: String)(ss: List[String]) = ss ++ prefix(p)(ss)

  def intellijSettings = (p: Project) => p.settings(
    ideaProjectName := "ShipReq",
    ideaExcludeFolders := excludes.root ++ prefixT(Webapp.dir + "/")(excludes.webapp)
  )

  /*
  def eclipseSettings = (p: Project) => {
    import com.typesafe.sbteclipse.core.EclipsePlugin._
    p.settings(
      EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17),
      EclipseKeys.withSource := true,
      EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
    )
  }
  */

  def apply(module: Module): Project => Project = module match {
    case Root => _.configure(intellijSettings)
    case _    => identity
  }
}
