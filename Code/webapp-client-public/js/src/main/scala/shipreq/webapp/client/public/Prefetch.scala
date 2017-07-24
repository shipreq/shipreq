package shipreq.webapp.client.public

import japgolly.scalajs.react.Callback
import shipreq.base.util.Memo
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.util.ResourceHintJs._

object Prefetch {

  /** Run this to prefetch resources for MembersHome */
  val memberHome: () => Unit =
    Memo.fn0 {
      val res: List[ResourceHint] =
        ResourceHint.Prefetch.script(AssetManifest.reactDomServerJs) ::
        ResourceHint.Prefetch.script(AssetManifest.memberJs) ::
        ResourceHint.Prefetch.script(AssetManifest.webappClientHomeJs) ::
        Nil
      Callback(res.foreach(_.install())).delayMs(200).runNow()
    }

}
