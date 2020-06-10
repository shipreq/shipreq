package shipreq.webapp.base.ui.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import monocle.{Iso, Lens}
import scala.collection.compat.immutable.ArraySeq
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{Identity, IsoBool, Validity}
import shipreq.webapp.base.data.{Disabled, Enabled, On}
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic.{Input, Segment, UsesSemanticUiManually}
import shipreq.webapp.base.validation.Simple

@UsesSemanticUiManually
object Form {

  def apply(field1: Field[_], fieldN: Field[_]*)(implicit vux: ValidationUX): VdomTag =
    <.div(^.className := "ui form",
      field1.render,
      fieldN.toTagMod(_.render))

  def apply(fields: NonEmptyVector[Field[_]])(implicit vux: ValidationUX): VdomTag =
    apply(fields.head, fields.tail: _*)

  @inline def validationErr = Input.validationErr

  private def renderSimpleInvalidity(i: Simple.Invalidity) =
    GeneralTheme.renderSimpleInvalidity(i)(validationErr)

  type Validated = ValidationUX => ValidationUX.Outcome[VdomNode]

  sealed trait Field[A] {

    def modEditor(f: (TagMod => VdomNode) => TagMod => VdomNode): Field[A]

    protected def validationUX: Option[ValidationUX]

    def withLabel       (l: TagMod                           ): Field[A]
    def withValue       (v: A                                ): Field[A]
    def withUpdater     (f: A => Callback                    ): Field[A]
    def withValidated   (v: Option[Validated]                ): Field[A]
    def withValidator   (v: Option[Simple.Validator[A, _, _]]): Field[A]
    def withValidationUX(v: Option[ValidationUX]             ): Field[A]
    def withEnabled     (e: Enabled                          ): Field[A]
    def withOuterMod    (f: VdomTag => VdomTag               ): Field[A]

    def render(implicit vux: ValidationUX): VdomNode

    final def addTagMod(t: TagMod): Field[A] =
      modEditor(f => t0 => f(TagMod(t0, t)))

    final def withAutoFocus: Field[A] =
      addTagMod(^.autoFocus := true)

    final def withEnabledAndAutoFocus(enabled: Enabled): Field[A] =
      enabled match {
        case Enabled  => withAutoFocus
        case Disabled => disable
      }

    final def withEditor(e: TagMod => VdomNode): Field[A] =
      modEditor(_ => e)

    @inline final def withValidated(v: Validated): Field[A] =
      withValidated(Some(v))

    @inline final def withValidated(v: ValidationUX.Outcome[VdomNode]): Field[A] =
      withValidated(_ => v)

    final def withValidated(v: Simple.Invalidity \/ Any): Field[A] =
      withValidated(vux =>
        vux.outcome(v match {
          case \/-(_) => None
          case -\/(e) => Some(renderSimpleInvalidity(e))
        })
      )

    final def withValidity(o: Option[Validity]): Field[A] =
      withValidated(o.map(v => (_: ValidationUX) => ValidationUX.Outcome(v)))

    @inline final def withValidity(v: Validity): Field[A] =
      withValidity(Some(v))

    @inline final def withValidator(v: Simple.Validator[A, _, _]): Field[A] =
      withValidator(Some(v))

    @inline final def disable: Field[A] =
      withEnabled(Disabled)

    @inline final def inline: Field[A] =
      withOuterMod(_(Field.inline))

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

    final def void: Field[Unit] =
      Field.Void(this)
  }

  object Field {
    private[Form] val plain    = <.div(^.className := "field")
    private[Form] val error    = <.div(^.className := "field error")
    private[Form] val disabled = ^.cls := "disabled"
    private[Form] val inline   = ^.cls := "inline"

    private abstract class Const[A] extends Field[A] {
      override final protected def validationUX = None
      override final def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = this
      override final def withLabel       (l: TagMod                                    ) = this
      override final def withValue       (v: A                                         ) = this
      override final def withUpdater     (f: A => Callback                             ) = this
      override final def withValidated   (v: Option[Validated]                         ) = this
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
      override final def withValidated   (v: Option[Validated]                         ) = mod(_.withValidated   (v))
      override final def withValidator   (v: Option[Simple.Validator[A, _, _]]         ) = mod(_.withValidator   (v))
      override final def withValidationUX(v: Option[ValidationUX]                      ) = mod(_.withValidationUX(v))
      override final def withEnabled     (e: Enabled                                   ) = mod(_.withEnabled     (e))
      override final def withOuterMod    (f: VdomTag => VdomTag                        ) = mod(_.withOuterMod    (f))
    }

    private final case class Xmap[A, B](underlying: Field[A], ab: A => B, ba: B => A) extends Field[B] {
      override protected def validationUX = underlying.validationUX
      override def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = copy(underlying.modEditor       (f))
      override def withLabel       (l: TagMod                                    ) = copy(underlying.withLabel       (l))
      override def withValue       (v: B                                         ) = copy(underlying.withValue       (ba(v)))
      override def withUpdater     (f: B => Callback                             ) = copy(underlying.withUpdater     (f compose ab))
      override def withValidated   (v: Option[Validated]                         ) = copy(underlying.withValidated   (v))
      override def withValidator   (v: Option[Simple.Validator[B, _, _]]         ) = copy(underlying.withValidator   (v.map(_.xmapInput(ba)(ab))))
      override def withValidationUX(v: Option[ValidationUX]                      ) = copy(underlying.withValidationUX(v))
      override def withEnabled     (e: Enabled                                   ) = copy(underlying.withEnabled     (e))
      override def withOuterMod    (f: VdomTag => VdomTag                        ) = copy(underlying.withOuterMod    (f))

      override def render(implicit vux: ValidationUX): VdomNode =
        underlying.render
    }

    private final case class Void[A](underlying: Field[A]) extends Field[Unit] {
      override protected def validationUX = underlying.validationUX
      override def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = copy(underlying.modEditor       (f))
      override def withLabel       (l: TagMod                                    ) = copy(underlying.withLabel       (l))
      override def withValue       (v: Unit                                      ) = this
      override def withUpdater     (f: Unit => Callback                          ) = this
      override def withValidated   (v: Option[Validated]                         ) = copy(underlying.withValidated   (v))
      override def withValidator   (v: Option[Simple.Validator[Unit, _, _]]      ) = this
      override def withValidationUX(v: Option[ValidationUX]                      ) = copy(underlying.withValidationUX(v))
      override def withEnabled     (e: Enabled                                   ) = copy(underlying.withEnabled     (e))
      override def withOuterMod    (f: VdomTag => VdomTag                        ) = copy(underlying.withOuterMod    (f))

      override def render(implicit vux: ValidationUX): VdomNode =
        underlying.render
    }

    private final case class Generic[A](renderInner: Generic[A] => TagMod,
                                        label      : Option[TagMod],
                                        editor     : TagMod => VdomNode,
                                        value      : A,
                                        updater    : A => Callback,
                                        validated  : Option[Validated],
                                        validator  : Option[Simple.Validator[A, _, _]],
                                        vux        : Option[ValidationUX],
                                        enabled    : Enabled,
                                        outerMod   : VdomTag => VdomTag,
                                       ) extends Field[A] {

      override protected def validationUX = vux

      override def modEditor       (f: (TagMod => VdomNode) => TagMod => VdomNode) = copy(editor    = f(editor))
      override def withLabel       (l: TagMod                                    ) = copy(label     = Some(l))
      override def withValue       (v: A                                         ) = copy(value     = v)
      override def withUpdater     (f: A => Callback                             ) = copy(updater   = f)
      override def withValidated   (v: Option[Validated]                         ) = copy(validated = v)
      override def withValidator   (v: Option[Simple.Validator[A, _, _]]         ) = copy(validator = v)
      override def withValidationUX(v: Option[ValidationUX]                      ) = copy(vux       = v)
      override def withEnabled     (e: Enabled                                   ) = copy(enabled   = e)
      override def withOuterMod    (f: VdomTag => VdomTag                        ) = copy(outerMod  = f compose outerMod)

      private val ableness = Field.disabled.unless(enabled is Enabled)

      private lazy val inner = renderInner(this)

      override def render(implicit vuxI: ValidationUX): VdomTag = {
        val vux = this.vux.getOrElse(vuxI)

        val error: ValidationUX.Outcome[VdomNode] =
          validated match {
            case Some(f) => f(vux)
            case None =>
              validator match {
                case Some(v) => vux.outcomeD(v(value)).map(renderSimpleInvalidity)
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
        validated   = None,
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

    def booleanSelect[A: UnivEq](a1: A, a2: A)(render: A => VdomNode): Field[A] = {

      def key(a: A): String =
        if (a == a1) "1" else "2"

      val items: ArraySeq[Dropdown.Item[A]] =
        ArraySeq(
          Dropdown.Item(key(a1), render(a1), a1),
          Dropdown.Item(key(a2), render(a2), a2),
        )

      generic(a1, null) { f =>
        import f._

        val actualLabel = label.whenDefined(<.label(_))

        val onChange: Dropdown.Item[A] => Callback =
          o => updater(validator.fold(o.value)(_.corrector.live(o.value)))

        val editor =
          Dropdown.Props.Optional[A](
            items    = items,
            selected = Some(key(value)),
            enabled  = enabled)(
            onChange = onChange)

        TagMod(actualLabel, editor.render)
      }
    }

    def booleanSelect[B <: IsoBool[B]](b: IsoBool.Object[B])(render: B => String): Field[B] = {
      implicit def univEq: UnivEq[B] = UnivEq.force
      val b1 = b.positive
      val b2 = b.negative
      if (render(b1).compareTo(render(b2)) < 0)
        booleanSelect[B](b1, b2)(render(_))
      else
        booleanSelect[B](b2, b1)(render(_))
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def boolean(tagMod: TagMod): Field[Boolean] =
      checkbox(tagMod).imap(On.isoWhen(true))

    lazy val boolean: Field[Boolean] =
      boolean(TagMod.empty)

    lazy val booleanCentered: Field[Boolean] =
      boolean(^.textAlign.center)

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def ofEditor(editor: TagMod*): Field[Unit] = {
      val actualEditor = TagMod.fromTraversableOnce(editor)
      generic((), null){ f =>
        import f._
        val actualLabel = label.whenDefined(<.label(_))
        TagMod(actualLabel, actualEditor)
      }
    }

    /** This will take whatever content you provide and wrap it in a .field. */
    def around(content: TagMod*): Field[Unit] = {
      val c = TagMod.fromTraversableOnce(content)
      generic((), null)(_ => c)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    /** Rather than generating a .field, this is a replacement.
      * Put whatever you want in here. If you don't make it a .field that's up to you.
      */
    def replacement(value: VdomNode): Field[Unit] =
      new RowReplacement(value)

    private final class RowReplacement(value: VdomNode) extends Const[Unit] {
      override def render(implicit vuxI: ValidationUX): VdomNode =
        value
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    /** Puts two fields side by side on a single row. */
    def two(f1: Field[_], f2: Field[_]): Field[Unit] =
      new Two(f1.void, f2.void)

    private final class Two[A](f1: Field[A], f2: Field[A]) extends Delegate[A] {
      override protected def mod: (Field[A] => Field[A]) => Field[A] =
        f => new Two(f(f1), f(f2))

      override protected def validationUX =
        f1.validationUX.orElse(f2.validationUX)

      override def render(implicit vux: ValidationUX): VdomTag =
        <.div(^.cls := "two fields", f1.render, f2.render)
    }

  }
}
