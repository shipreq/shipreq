import sbt._
import sbt.Keys._
import sbtdocker.DockerPlugin.autoImport._
import Common.{propOrEnv, releaseMode}

/** This is all about building and publishing of ShipReq docker images. */
object DockerCfg {

  val baseImage = {
    val image = propOrEnv("BASE_IMAGE_URL").getOrElse("shipreq/base")
    val tag = "latest"
    s"$image:$tag"
  }

  def settingsFor(name: String): Project => Project =
    _.settings(

      docker / buildOptions :=
        BuildOptions(pullBaseImage = BuildOptions.Pull.IfMissing),

      docker / imageNames := {
        val imageUrl = propOrEnv(name.toUpperCase + "_IMAGE_URL")
        val isLocal  = imageUrl.isEmpty
        val image    = imageUrl.getOrElse(s"shipreq/$name")

        var versions = Seq[String]("git-" + version.value, "latest")
        if (isLocal && releaseMode) versions :+= "latest-prod"

        versions.map(ver => ImageName(s"$image:$ver"))
      }
    )

  def envVars = Def.task(
    List[(String, String)](
      "SHIPREQ_VERSION"    -> version.value,
      "SHIPREQ_BUILD_MODE" -> (if (releaseMode) "release" else "dev")))

}
