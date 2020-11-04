package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import shipreq.webapp.member.jsfacade.Autosize

object AutosizeTextarea {

  def apply(tagMods: TagMod*) =
    Component(TagMod(tagMods: _*))

  val Component = ScalaComponent.builder[TagMod]
    .render_P(<.textarea(_))
    .configure(applyTo(e => e))
    .build

  def applyTo[P, C <: Children, S, B](f: dom.Element => Autosize.Targets): ScalaComponent.Config[P, C, S, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] = _
    .componentDidMount   (i => Callback(Autosize        (f(i.getDOMNode.asElement()))))
    .componentDidUpdate  (i => Callback(Autosize.update (f(i.getDOMNode.asElement()))))
    .componentWillUnmount(i => Callback(Autosize.destroy(f(i.getDOMNode.asElement()))))

  def applyToChildren[P, C <: Children, S, B](sel: String) =
    applyTo[P, C, S, B](_.querySelectorAll(sel))

  // ===================================================================================================================

  @inline def editor: TagMod => VdomNode =
    editor()

  def editor(rows: Int = 3): TagMod => VdomNode =
    t => Component(TagMod(^.rows := rows, t))
}
