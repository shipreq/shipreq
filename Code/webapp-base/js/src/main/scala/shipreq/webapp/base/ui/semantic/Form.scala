package shipreq.webapp.base.ui.semantic

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import shipreq.webapp.base.data.Enabled
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.UiUtil
import shipreq.webapp.base.validation.Simple

object Form {

  sealed abstract class Field {
    def render: VdomTag
    def enabled: Enabled
    def fieldCopy(enabled: Enabled = enabled): Field
    final def disable = fieldCopy(!Enabled)
  }
  object Field {
    private[Form] val plain  = <.div(^.className := "field")
    private[Form] val error  = <.div(^.className := "field error")
    private[Form] val center = plain(^.textAlign.center)
    private[Form] val disabled = ^.cls := "disabled"
  }

  val validationErr = TagMod(
    ^.color := "#9f3a38",
    ^.paddingTop := "0.15rem",
    ^.fontSize := "92%")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class TextField(label  : Option[TagMod],
                             editor : VdomElement,
                             error  : ValidationUX.Outcome[VdomElement],
                             enabled: Enabled = Enabled) extends Field {
    override def fieldCopy(enabled: Enabled = enabled) = copy(enabled = enabled)
    override def render: VdomTag = {
      val labelTag = label.whenDefined(<.label(_))
      val ableness = Field.disabled.unless(enabled is Enabled)
      error match {
        case ValidationUX.Outcome.Valid            => Field.plain(ableness, labelTag, editor)
        case ValidationUX.Outcome.Invalid(None)    => Field.error(ableness, labelTag, editor)
        case ValidationUX.Outcome.Invalid(Some(e)) => Field.error(ableness, labelTag, editor, e)
      }
    }
  }

  object TextField {
    /** Note: DO NOT use this with Reusability.
      * StateSnapshot + Lens + Reusability = NO!
      */
    def highLevel[S](lens : Lens[S, String],
                     vali : Simple.Validator[String, _, _],
                     input: TagMod => VdomTag = <.input.text(_),
                     label: Option[TagMod]    = None): ValidationUX => StateSnapshot[S] => TextField =
      vux => ss => {

        val onChange: ReactEventFromInput => Callback =
          _.extract(_.target.value)(v => ss.modState(lens.set(vali.corrector.live(v))))

        val value: String =
          lens.get(ss.value)

        val editor =
          input(TagMod(
            ^.value := value,
            ^.onChange ==> onChange))

        val error: ValidationUX.Outcome[VdomElement] =
          vux.outcomeD(vali(value)).map(UiUtil.renderSimpleInvalidity(_)(validationErr))

        TextField(label, editor, error)
      }

    /** Note: DO NOT use this with Reusability.
      * StateSnapshot + Lens + Reusability = NO!
      */
    def unvalidated[S](lens : Lens[S, String],
                       input: TagMod => VdomTag = <.input.text(_),
                       label: Option[TagMod]    = None): StateSnapshot[S] => TextField =
      highLevel(lens, Simple.Validator.id, input, label)(ValidationUX.Off)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class CenteredField(content: VdomElement,
                                 enabled: Enabled = Enabled) extends Field {
    override def fieldCopy(enabled: Enabled = enabled) = copy(enabled = enabled)
    override def render: VdomTag =
      Field.center(
        Field.disabled.unless(enabled is Enabled),
        content)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** You're a LIAR! */
  final case class NotAField(render: VdomTag) extends Field {
    override def fieldCopy(enabled: Enabled = enabled) = this // argh, why even bother anymore?
    override def enabled = Enabled
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def apply(field1: Field, fieldN: Field*): VdomTag =
    <.div(^.className := "ui form",
      field1.render,
      fieldN.toTagMod(_.render))

  def apply(fields: NonEmptyVector[Field]): VdomTag =
    apply(fields.head, fields.tail: _*)
}
