package shipreq.webapp.client.feature

import japgolly.scalajs.react._, ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.Lens
import scalaz.Equal
import scalaz.std.option.optionEqual
import shipreq.base.util.Intersection
import shipreq.webapp.client.jsfacade.ReactCollapse
import PreviewFeature._

/**
 * Supplies logic to determine whether or not to show a preview for some rich-text editor.
 *
 * Preview available:
 * - when editing and focused and (dirty or has been edited since receiving focus)
 *
 * Usage: Parent
 * =============
 *
 * Create an "key" ADT to uniquely identify all types of children that will use this feature.
 * Embed a single instance of `PreviewFeature.State` in the top-most component's state.
 * Initialise it with `PreviewFeature.initState`.
 *
 * Usage: Child (direct)
 * =====================
 *
 * Request a `PreviewFeature.ForChild` in the component's props.
 * Use `show_?` to see whether a preview should be rendered or not.
 * Wire up all the `onXxxx` callbacks.
 *
 * Usage: Child (composite) / Sandwich-Meat
 * ========================================
 *
 * Request a `PreviewFeature.ForChildren[K]` in the component's props.
 * Request a `PreviewFeature.State[K]` in the component's props.
 * Call `ForChildren.forChild` for each child.
 */
final class PreviewFeature[S, K]($: CompState.Access[S], lens: Lens[S, State[K]])(implicit EK: Equal[K])
  extends ForChildren[K] {

  def mapK[A](p: Intersection[K, A]): PreviewFeature[S, A] = {
    val l = (Lens[S, State[A]]
      (s => lens.get(s).mapK(p))
      (sa => lens.set(sa.mapK(p.reverse))))
    val e: Equal[A] = optionEqual[K].contramap(p.reverse.getOption)
    new PreviewFeature($, l)(e)
  }

  private val hasKey: K => FocusData[K] => Boolean =
    if (EK.equalIsNatural)
      k => _.key == k
    else
      k => fi => EK.equal(fi.key, k)

  def onFocus(k: K): Callback =
    $.modState(s =>
      if (lens.get(s) exists hasKey(k))
        s
      else
        lens.set(Some(FocusData(k, false)))(s))

  def onBlur(k: K): Callback =
    $.state.flatMap(s =>
      Callback.when(lens.get(s) exists hasKey(k))(
        $.setState(lens.set(None)(s))))

  def onEdit(k: K): Callback =
    $.modState(s =>
      if (!lens.get(s).exists(i => i.changedSinceFocus && hasKey(k)(i)))
        lens.set(Some(FocusData(k, true)))(s)
      else
        s)

  def show_?(state: State[K], isDirty: => Boolean): Boolean =
    state.exists(_.changedSinceFocus || isDirty)

  def state(s: S): State[K] =
    lens.get(s)

  def forChild(k: K, s: S): ForChild =
    forChild(k, state(s))

  override def forChild(k: K, s: State[K]): ForChild = {
    val self = this
    new ForChild {
      override val underlyingFeature           = Some(self)
      override val focusData                   = s.filter(hasKey(k))
      override def show_?(isDirty: => Boolean) = self.show_?(focusData, isDirty)
      override def onFocus                     = self onFocus k
      override def onBlur                      = self onBlur k
      override def onEdit                      = self onEdit k
      override def toString                    = focusData.toString
    }
  }
}

object PreviewFeature {

  type State[+K] = Option[FocusData[K]]

  @inline implicit class PFStateOps[K](private val s: State[K]) extends AnyVal {
    def mapK[A](i: Intersection[K, A]): State[A] =
      s.flatMap(_ omap i.getOption)
  }

  def initState: State[Nothing] =
    None

  case class FocusData[+K](key: K, changedSinceFocus: Boolean) {
    def omap[A](f: K => Option[A]): Option[FocusData[A]] =
      f(key).map(FocusData(_, changedSinceFocus))
  }

  trait ForChildren[K] {
    def mapK[A](p: Intersection[K, A]): ForChildren[A]
    def forChild(k: K, s: State[K]): ForChild
  }

  trait ForChild {
    def underlyingFeature: Option[PreviewFeature[_, _]]
    val focusData: Option[FocusData[Any]]

    def show_?(isDirty: => Boolean): Boolean
    def onFocus: Callback
    def onBlur: Callback
    def onEdit: Callback

    final def showOption[A](isDirty: => Boolean)(a: => A): Option[A] =
      if (show_?(isDirty)) Some(a) else None

    final def reactCollapse[A](isDirty: => Boolean): ReactCollapse =
      ReactCollapse(show_?(isDirty))
  }

  private val equalUnderlyingFeature: Equal[Option[PreviewFeature[_, _]]] =
    optionEqual(Equal.equalRef)

  private val equalFocusData: Equal[Option[FocusData[Any]]] =
    optionEqual(Equal.equal((a, b) => (a eq b) || (a == b)))

  implicit val equalForChild: Equal[ForChild] =
    Equal.equal((a, b) =>
      equalUnderlyingFeature.equal(a.underlyingFeature, b.underlyingFeature) &&
        equalFocusData.equal(a.focusData, b.focusData))

  implicit val reusabilityForChild: Reusability[ForChild] =
    Reusability.byRefOrEqual

  object AlwaysShow extends ForChild {
    override def underlyingFeature           = None
    override val focusData                   = Some(FocusData((), true))
    override def show_?(isDirty: => Boolean) = true
    override def onFocus                     = Callback.empty
    override def onBlur                      = Callback.empty
    override def onEdit                      = Callback.empty
  }

  object NeverShow extends ForChild {
    override def underlyingFeature           = None
    override val focusData                   = None
    override def show_?(isDirty: => Boolean) = false
    override def onFocus                     = Callback.empty
    override def onBlur                      = Callback.empty
    override def onEdit                      = Callback.empty
  }

  @inline def FixKey[K] = new FixKey[K]
  final class FixKey[K] {
    type Feature[S]  = PreviewFeature[S, K]
    type ForChildren = PreviewFeature.ForChildren[K]
    type State       = PreviewFeature.State[K]
  }
}
