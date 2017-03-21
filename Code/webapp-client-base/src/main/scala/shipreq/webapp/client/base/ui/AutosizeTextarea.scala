package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.client.base.jsfacade.Autosize

object AutosizeTextarea {

  def apply(tagMods: TagMod*) =
    Component(TagMod(tagMods: _*))

  val Component = ScalaComponent.builder[TagMod]("AutosizeTextarea")
    .render_P(<.textarea(_))
    .componentDidMount   (i => Callback(Autosize.init   (i.getDOMNode)))
    .componentDidUpdate  (i => Callback(Autosize.update (i.getDOMNode)))
    .componentWillUnmount(i => Callback(Autosize.destroy(i.getDOMNode)))
    .build
}
