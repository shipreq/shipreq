package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import org.scalajs.dom
import shipreq.webapp.base.jsfacade.Autosize
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.validation.Simple

object AutosizeTextarea {

  def apply(tagMods: TagMod*) =
    Component(TagMod(tagMods: _*))

  val Component = ScalaComponent.builder[TagMod]("AutosizeTextarea")
    .render_P(<.textarea(_))
    .configure(applyTo(e => e))
    .build

  def applyTo[P, C <: Children, S, B](f: dom.Element => Autosize.Targets): ScalaComponent.Config[P, C, S, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] = _
    .componentDidMount   (i => Callback(Autosize        (f(i.getDOMNode.asElement))))
    .componentDidUpdate  (i => Callback(Autosize.update (f(i.getDOMNode.asElement))))
    .componentWillUnmount(i => Callback(Autosize.destroy(f(i.getDOMNode.asElement))))

  def applyToChildren[P, C <: Children, S, B](sel: String) =
    applyTo[P, C, S, B](_.querySelectorAll(sel))

  // ===================================================================================================================

  object SemanticUiFormField {
    import shipreq.webapp.base.ui.semantic.Form._

    /** Note: DO NOT use this with Reusability.
     * StateSnapshot + Lens + Reusability = NO!
     */
    def highLevel[S](lens : Lens[S, String],
                     vali : Simple.Validator[String, _, _],
                     rows : Int = 3,
                     label: Option[TagMod] = None): ValidationUX => StateSnapshot[S] => TextField =
      vux => ss => {

        val onChange: ReactEventFromTextArea => Callback =
          _.extract(_.target.value)(v => ss.modState(lens.set(vali.corrector.live(v))))

        val value: String =
          lens.get(ss.value)

        val editor =
          Component(TagMod(
            ^.rows := rows,
            ^.value := value,
            ^.onChange ==> onChange))

        val error: ValidationUX.Outcome[VdomElement] =
          vux.outcomeD(vali(value)).map(GeneralTheme.renderSimpleInvalidity(_)(validationErr))

        TextField(label, editor, error)
      }

    /** Note: DO NOT use this with Reusability.
     * StateSnapshot + Lens + Reusability = NO!
     */
    def unvalidated[S](lens : Lens[S, String],
                       rows : Int = 3,
                       label: Option[TagMod] = None): StateSnapshot[S] => TextField =
      highLevel(lens, Simple.Validator.id, rows, label)(ValidationUX.Off)

  }
}
