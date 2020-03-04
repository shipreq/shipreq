package shipreq.webapp.base.ui.semantic

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.{Iso, Lens}
import shipreq.base.util.{Identity, Validity}
import shipreq.webapp.base.data.{Disabled, Enabled, On}
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.validation.Simple

object Form {

  def apply(field1: Field[_], fieldN: Field[_]*)(implicit vux: ValidationUX): VdomTag =
    <.div(^.className := "ui form",
      field1.render,
      fieldN.toTagMod(_.render))

  def apply(fields: NonEmptyVector[Field[_]])(implicit vux: ValidationUX): VdomTag =
    apply(fields.head, fields.tail: _*)

  val validationErr = TagMod(
    ^.color      := "#9f3a38",
    ^.paddingTop := "0.15rem",
    ^.fontSize   := "92%")

  sealed trait Field[A] {

    def modEditor(f: (TagMod => VdomNode) => TagMod => VdomNode): Field[A]

    def withLabel       (l: TagMod                           ): Field[A]
    def withValue       (v: A                                ): Field[A]
    def withUpdater     (f: A => Callback                    ): Field[A]
    def withValidity    (v: Option[Validity]                 ): Field[A]
    def withValidator   (v: Option[Simple.Validator[A, _, _]]): Field[A]
    def withValidationUX(v: Option[ValidationUX]             ): Field[A]
    def withEnabled     (e: Enabled                          ): Field[A]
    def withOuterMod    (f: VdomTag => VdomTag               ): Field[A]

    def render(implicit vux: ValidationUX): VdomTag

    final def addTagMod(t: TagMod): Field[A] =
      modEditor(f => t0 => f(TagMod(t0, t)))

    final def withAutoFocus: Field[A] =
      addTagMod(^.autoFocus := true)

    final def withEditor(e: TagMod => VdomNode): Field[A] =
      modEditor(_ => e)

    @inline final def withValidity(v: Validity): Field[A] =
      withValidity(Some(v))

    @inline final def withValidator(v: Simple.Validator[A, _, _]): Field[A] =
      withValidator(Some(v))

    @inline final def disable: Field[A] =
      withEnabled(Disabled)

    @inline final def withValidationUX(v: ValidationUX): Field[A] =
      withValidationUX(Some(v))

    final def asSegment: Field[A] =
      withOuterMod(Segment.tag(_))

    final def withState(ss: StateSnapshot[A]): Field[A] =
      withValue(ss.value).withUpdater(ss.setState)

    final def withStateLens[S](l: Lens[S, A]): StateSnapshot[S] => Field[A] =
      ss => withState(ss.zoomStateL(l))

    final def xmap[B](f: A => B)(g: B => A): Field[B] =
      Field.Xmap(this, f, g)

    final def imap[B](i: Iso[A, B]): Field[B] =
      xmap(i.get)(i.reverseGet)
  }

  object Field {
    private[Form] val plain  = <.div(^.className := "field")
    private[Form] val error  = <.div(^.className := "field error")
    private[Form] val disabled = ^.cls := "disabled"

    private abstract class Const[A] extends Field[A] {
      override final def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = this
      override final def withLabel       (l: TagMod                                    ) = this
      override final def withValue       (v: A                                         ) = this
      override final def withUpdater     (f: A => Callback                             ) = this
      override final def withValidity    (v: Option[Validity]                          ) = this
      override final def withValidator   (v: Option[Simple.Validator[A, _, _]]         ) = this
      override final def withValidationUX(v: Option[ValidationUX]                      ) = this
      override final def withEnabled     (e: Enabled                                   ) = this
      override final def withOuterMod    (f: VdomTag => VdomTag                        ) = this
    }

    private abstract class Delegate[A] extends Field[A] {
      protected def mod: (Field[A] => Field[A]) => Field[A]

      override final def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = mod(_.modEditor       (f))
      override final def withLabel       (l: TagMod                                    ) = mod(_.withLabel       (l))
      override final def withValue       (v: A                                         ) = mod(_.withValue       (v))
      override final def withUpdater     (f: A => Callback                             ) = mod(_.withUpdater     (f))
      override final def withValidity    (v: Option[Validity]                          ) = mod(_.withValidity    (v))
      override final def withValidator   (v: Option[Simple.Validator[A, _, _]]         ) = mod(_.withValidator   (v))
      override final def withValidationUX(v: Option[ValidationUX]                      ) = mod(_.withValidationUX(v))
      override final def withEnabled     (e: Enabled                                   ) = mod(_.withEnabled     (e))
      override final def withOuterMod    (f: VdomTag => VdomTag                        ) = mod(_.withOuterMod    (f))
    }

    private final case class Xmap[A, B](underlying: Field[A], ab: A => B, ba: B => A) extends Field[B] {
      override def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = copy(underlying.modEditor       (f))
      override def withLabel       (l: TagMod                                    ) = copy(underlying.withLabel       (l))
      override def withValue       (v: B                                         ) = copy(underlying.withValue       (ba(v)))
      override def withUpdater     (f: B => Callback                             ) = copy(underlying.withUpdater     (f compose ab))
      override def withValidity    (v: Option[Validity]                          ) = copy(underlying.withValidity    (v))
      override def withValidator   (v: Option[Simple.Validator[B, _, _]]         ) = copy(underlying.withValidator   (v.map(_.xmapInput(ba)(ab))))
      override def withValidationUX(v: Option[ValidationUX]                      ) = copy(underlying.withValidationUX(v))
      override def withEnabled     (e: Enabled                                   ) = copy(underlying.withEnabled     (e))
      override def withOuterMod    (f: VdomTag => VdomTag                        ) = copy(underlying.withOuterMod    (f))

      override def render(implicit vux: ValidationUX): VdomTag =
        underlying.render
    }

    private final case class Generic[A](renderInner: Generic[A] => TagMod,
                                        label      : Option[TagMod],
                                        editor     : TagMod => VdomNode,
                                        value      : A,
                                        updater    : A => Callback,
                                        validity   : Option[Validity],
                                        validator  : Option[Simple.Validator[A, _, _]],
                                        vux        : Option[ValidationUX],
                                        enabled    : Enabled,
                                        outerMod   : VdomTag => VdomTag,
                                       ) extends Field[A] {

      override def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = copy(editor    = f(editor))
      override def withLabel       (l: TagMod                                    ) = copy(label     = Some(l))
      override def withValue       (v: A                                         ) = copy(value     = v)
      override def withUpdater     (f: A => Callback                             ) = copy(updater   = f)
      override def withValidity    (v: Option[Validity]                          ) = copy(validity  = v)
      override def withValidator   (v: Option[Simple.Validator[A, _, _]]         ) = copy(validator = v)
      override def withValidationUX(v: Option[ValidationUX]                      ) = copy(vux       = v)
      override def withEnabled     (e: Enabled                                   ) = copy(enabled   = e)
      override def withOuterMod    (f: VdomTag => VdomTag                        ) = copy(outerMod  = f compose outerMod)

      private val ableness = Field.disabled.unless(enabled is Enabled)

      private lazy val inner = renderInner(this)

      override def render(implicit vuxI: ValidationUX): VdomTag = {
        val vux = this.vux.getOrElse(vuxI)

        val error: ValidationUX.Outcome[VdomElement] =
          validity match {
            case Some(v) =>
              ValidationUX.Outcome(v)

            case None =>
              validator match {
                case Some(v) => vux.outcomeD(v(value)).map(GeneralTheme.renderSimpleInvalidity(_)(validationErr))
                case None    => ValidationUX.Outcome.Valid
              }
          }

        val outer =
          error match {
            case ValidationUX.Outcome.Valid            => Field.plain(ableness, inner)
            case ValidationUX.Outcome.Invalid(None)    => Field.error(ableness, inner)
            case ValidationUX.Outcome.Invalid(Some(e)) => Field.error(ableness, inner, e)
          }

        outerMod(outer)
      }
    }

    private def generic[A](value      : A,
                           editor     : TagMod => VdomNode)(
                           renderInner: Generic[A] => TagMod): Generic[A] =
      Generic(
        renderInner = renderInner,
        label       = None,
        editor      = editor,
        value       = value,
        updater     = (_: Any) => Callback.empty,
        validity    = None,
        validator   = None,
        vux         = None,
        enabled     = Enabled,
        outerMod    = Identity.apply,
      )

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    lazy val text: Field[String] =
      generic("", <.input.text(_)){ f =>
        import f._

        val actualLabel = label.whenDefined(<.label(_))

        val onChange: ReactEventFromInput => Callback =
          _.extract(_.target.value)(s => updater(validator.fold(s)(_.corrector.live(s))))

        val actualEditor =
          editor(TagMod(
            ^.value := value,
            ^.onChange ==> onChange))

        TagMod(actualLabel, actualEditor)
      }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def checkbox(tagMod: TagMod): Field[On] =
      generic(!On, null) { f =>
        import f._

        val onChange: On => Callback =
          on => updater(validator.fold(on)(_.corrector.live(on)))

        val checkbox =
          Input.Checkbox(
            on     = value,
            change = onChange,
            label  = label,
          )

        TagMod(checkbox, tagMod)
      }

    lazy val checkbox: Field[On] =
      checkbox(TagMod.empty)

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def boolean(tagMod: TagMod): Field[Boolean] =
      checkbox(tagMod).imap(On.isoWhen(true))

    lazy val boolean: Field[Boolean] =
      boolean(TagMod.empty)

    lazy val booleanCentered: Field[Boolean] =
      boolean(^.textAlign.center)

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    /** This will take whatever content you provide and wrap it in a .field. */
    def around(content: TagMod*): Field[Unit] = {
      val c = TagMod.fromTraversableOnce(content)
      generic((), null)(_ => c)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    /** Rather than generating a .field, this is a replacement.
      * Put whatever you want in here. If you don't make it a .field that's up to you.
      */
    def replacement(value: VdomTag): Field[Unit] =
      new RowReplacement(value)

    private final class RowReplacement(value: VdomTag) extends Const[Unit] {
      override def render(implicit vuxI: ValidationUX): VdomTag =
        value
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    /** Puts two fields side by side on a single row. */
    def two[A](f1: Field[A], f2: Field[A]): Field[A] =
      new Two(f1, f2)

    private final class Two[A](f1: Field[A], f2: Field[A]) extends Delegate[A] {
      override protected def mod: (Field[A] => Field[A]) => Field[A] =
        f => new Two(f(f1), f(f2))

      override def render(implicit vux: ValidationUX): VdomTag =
        <.div(^.cls := "two fields", f1.render, f2.render)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  }
}
