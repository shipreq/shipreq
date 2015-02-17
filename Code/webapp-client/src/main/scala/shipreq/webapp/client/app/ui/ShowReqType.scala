package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.webapp.base.data._
import SCRATCH._
import shipreq.webapp.client.lib.ui.UI

// TODO ShowReqType inconsistent style

final case class ShowReqType(subject       : ReqType.Id,
                     // TODO hoverForDetail: Boolean,
                             project       : Project) {
  @inline def render = ShowReqType.Component(this)
}

object ShowReqType {

  // TODO Take project out of props and can have caching
  
  val Component =
    ReactComponentB[ShowReqType]("ReqType")
      .stateless
      .render((p, _) =>
        UI.must(p.project.reqType(p.subject))(reqtype =>
          <.span(
            ^.title := reqtype.name,
            s"${reqtype.mnemonic.value}")))
      .build
  
}
