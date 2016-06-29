package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.html
import shipreq.webapp.client.base.jsfacade.Autosize

object AutosizeTextarea {

  @inline def apply(tagMods: TagMod*) =
    Component(TagMod(tagMods: _*))

  @inline def withRef(ref: String)(tagMods: TagMod*) =
    Component.withRef(ref)(TagMod(tagMods: _*))

  val Component = ReactComponentB[TagMod]("AutosizeTextarea")
    .render_P(<.textarea(_))
    .domType[html.TextArea]
    .componentDidMount   ($ => Callback(Autosize.init   (  $.getDOMNode())))
    .componentDidUpdate  (i => Callback(Autosize.update (i.$.getDOMNode())))
    .componentWillUnmount($ => Callback(Autosize.destroy(  $.getDOMNode())))
    .build
}
