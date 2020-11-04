package shipreq.webapp.base.config

final case class AssetManifest(staticAssetCdn: Option[AssetManifest.StaticAssetCdn]) extends AbstractAssetManifest[String] {

  override protected def modify(p: String): String = {
    staticAssetCdn match {
      case Some(cdn) => cdn.modPath(p)
      case _         => p
    }
  }
}

object AssetManifest {
  type CDN = AbstractAssetManifest.CDN
  val  CDN = AbstractAssetManifest.CDN

  private final val staticPath = "/s/"

  final case class StaticAssetCdn(value: String) {
    val with_/ = value.replaceFirst("/*$", "/")

    def modPath(path: String): String =
      if (path startsWith staticPath) {
        with_/ + path.drop(staticPath.length)
      } else
        path
  }

  implicit def univEqS: UnivEq[StaticAssetCdn] = UnivEq.derive
  implicit def univEq: UnivEq[AssetManifest] = UnivEq.derive
}
