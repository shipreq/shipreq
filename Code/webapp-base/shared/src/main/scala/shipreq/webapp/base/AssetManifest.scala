package shipreq.webapp.base

import shipreq.base.util.Url

final case class AssetManifest(staticAssetCdn: Option[Url.Absolute.Base]) extends AbstractAssetManifest[String] {

  override protected def modify(p: String): String = {
    staticAssetCdn match {
      case Some(cdn) if p startsWith "/s/" => (cdn / Url.Relative(p)).absoluteUrl
      case _                               => p
    }
  }
}

object AssetManifest {
  type CDN = AbstractAssetManifest.CDN
  val  CDN = AbstractAssetManifest.CDN

  implicit def univEq: UnivEq[AssetManifest] = UnivEq.derive
}
