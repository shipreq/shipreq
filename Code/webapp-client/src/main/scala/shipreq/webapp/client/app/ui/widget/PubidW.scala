package shipreq.webapp.client.app.ui.widget

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.data._
import shipreq.webapp.client.lib.ui.UI

// TODO PubidW inconsistent style

final case class PubidW(subject       : Pubid,
                // TODO hoverForDetail: Boolean,
                        project       : Project) {
  @inline def render = PubidW.Component(this)
}

object PubidW {

  // TODO Take project out of props and can have caching
  
  val Component =
    ReactComponentB[PubidW]("ReqId")
      .stateless
      .render((p, _) =>
        UI.must(p.project.reqType(p.subject.reqTypeId))(reqtype =>
          <.span(
            s"${reqtype.mnemonic.value}-${p.subject.pos}")))
      .build
}
