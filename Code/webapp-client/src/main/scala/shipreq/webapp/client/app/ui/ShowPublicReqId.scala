package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.webapp.base.data._
import SCRATCH._
import shipreq.webapp.client.lib.ui.UI

// TODO ShowPublicReqId inconsistent style

final case class ShowPublicReqId(subject       : PublicReqId,
                         // TODO hoverForDetail: Boolean,
                                 project       : Project) {
  @inline def render = ShowPublicReqId.Component(this)
}

object ShowPublicReqId {

  // TODO Take project out of props and can have caching
  
  val Component =
    ReactComponentB[ShowPublicReqId]("ReqId")
      .stateless
      .render((p, _) =>
        UI.must(p.project.reqType(p.subject.reqTypeId))(reqtype =>
          <.span(
            s"${reqtype.mnemonic.value}-${p.subject.pos}")))
      .build
  
}
