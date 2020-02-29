package shipreq.webapp.base.ui.semantic

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.validation.Simple

object Form {

  sealed abstract class Field {
    def render: VdomTag
    def setEnabled(e: Enabled): Field
    final def disable = setEnabled(Disabled)
  }
  object Field {
    private[Form] val plain  = <.div(^.className := "field")
    private[Form] val error  = <.div(^.className := "field error")
    private[Form] val disabled = ^.cls := "disabled"
  }

  val validationErr = TagMod(
    ^.color := "#9f3a38",
    ^.paddingTop := "0.15rem",
    ^.fontSize := "92%")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class TextField(label  : Option[TagMod],
                             editor : VdomNode,
                             error  : ValidationUX.Outcome[VdomElement],
                             enabled: Enabled = Enabled) extends Field {
    def setEnabled(e: Enabled) = copy(enabled = e)
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
                     input: TagMod => VdomNode = <.input.text(_),
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
          vux.outcomeD(vali(value)).map(GeneralTheme.renderSimpleInvalidity(_)(validationErr))

        apply(label, editor, error)
      }

    /** Note: DO NOT use this with Reusability.
      * StateSnapshot + Lens + Reusability = NO!
      */
    def unvalidated[S](lens : Lens[S, String],
                       input: TagMod => VdomNode = <.input.text(_),
                       label: Option[TagMod]    = None): StateSnapshot[S] => TextField =
      highLevel(lens, Simple.Validator.id, input, label)(ValidationUX.Off)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class BooleanSegmentField(label  : VdomNode,
                                       editor : VdomTag,
                                       error  : ValidationUX.Outcome[VdomElement],
                                       enabled: Enabled = Enabled) extends Field {
    def setEnabled(e: Enabled) = copy(enabled = e)
    override def render: VdomTag = {
      val labelTag = <.label(label)
      val ableness = Field.disabled.unless(enabled is Enabled)
      val checkbox = <.div(^.cls := "ui checkbox", ableness, editor, labelTag)
      val field =
        error match {
          case ValidationUX.Outcome.Valid            => Field.plain(checkbox)
          case ValidationUX.Outcome.Invalid(None)    => Field.error(checkbox)
          case ValidationUX.Outcome.Invalid(Some(e)) => Field.error(checkbox(e))
        }
      <.div(^.cls := "ui segment", field)
    }
  }

  object BooleanSegmentField {
    /** Note: DO NOT use this with Reusability.
     * StateSnapshot + Lens + Reusability = NO!
     */
    def highLevel[S](label: VdomNode,
                     lens : Lens[S, Boolean],
                     vali : Simple.Validator[Boolean, _, _], // = Simple.Validator.id[Boolean],
                     input: TagMod => VdomTag = <.input.checkbox(_),
                    ): ValidationUX => StateSnapshot[S] => BooleanSegmentField =
      vux => ss => {

        val onChange: ReactEventFromInput => Callback =
          _.extract(_.target.checked)(v => ss.modState(lens.set(vali.corrector.live(v))))

        val checked: Boolean =
          lens.get(ss.value)

        val editor =
          input(TagMod(
            ^.checked := checked,
            ^.onChange ==> onChange))

        val error: ValidationUX.Outcome[VdomElement] =
          vux.outcomeD(vali(checked)).map(GeneralTheme.renderSimpleInvalidity(_)(validationErr))

        apply(label, editor, error)
      }

    /** Note: DO NOT use this with Reusability.
     * StateSnapshot + Lens + Reusability = NO!
     */
    def unvalidated[S](label: VdomNode,
                       lens : Lens[S, Boolean],
                       input: TagMod => VdomTag = <.input.checkbox(_)): StateSnapshot[S] => BooleanSegmentField =
      highLevel(label, lens, Simple.Validator.id, input)(ValidationUX.Off)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class TwoFields(f1: Field, f2: Field) extends Field {
    def setEnabled(e: Enabled) = TwoFields(f1.setEnabled(e), f2.setEnabled(e))
    override def render: VdomTag =
      <.div(^.cls := "two fields", f1.render, f2.render)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class BasicField(content : TagMod,
                              tagMod  : TagMod   = EmptyVdom,
                              validity: Validity = Valid,
                              enabled : Enabled  = Enabled) extends Field {
    def setEnabled(e: Enabled) = copy(enabled = e)
    override def render: VdomTag = {
      val base = validity match {
        case Valid   => Field.plain
        case Invalid => Field.error
      }
      base(Field.disabled.unless(enabled is Enabled), tagMod, content)
    }
  }

  object BasicField {
    def centered(content: TagMod): BasicField = BasicField(content, ^.textAlign.center)
    def right(content: TagMod): BasicField = BasicField(content, ^.textAlign.right)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** You're a LIAR! */
  final case class NotAField(render: VdomTag) extends Field {
    override def setEnabled(e: Enabled) = this
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def apply(field1: Field, fieldN: Field*): VdomTag =
    <.div(^.className := "ui form",
      field1.render,
      fieldN.toTagMod(_.render))

  def apply(fields: NonEmptyVector[Field]): VdomTag =
    apply(fields.head, fields.tail: _*)
}
