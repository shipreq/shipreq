package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import shipreq.webapp.base.jsfacade.Autosize

object AutosizeTextarea {

  def apply(tagMods: TagMod*) =
    Component(TagMod(tagMods: _*))

  val Component = ScalaComponent.builder[TagMod]("AutosizeTextarea")
    .render_P(<.textarea(_))
    .configure(applyTo(e => e))
    .build

  def applyTo[P, C <: Children, S, B](f: dom.Element => Autosize.Targets): ScalaComponent.Config[P, C, S, B] = _
    .componentDidMount   (i => Callback(Autosize        (f(i.getDOMNode.asElement))))
    .componentDidUpdate  (i => Callback(Autosize.update (f(i.getDOMNode.asElement))))
    .componentWillUnmount(i => Callback(Autosize.destroy(f(i.getDOMNode.asElement))))

  def applyToChildren[P, C <: Children, S, B](sel: String): ScalaComponent.Config[P, C, S, B] =
    applyTo(_.querySelectorAll(sel))
}
