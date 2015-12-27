package shipreq.webapp.client.lib.ui.feature

import japgolly.scalajs.react._
import monocle._
import scalaz.Equal
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
 * Use `showPreview_?` to see whether a preview should be rendered or not.
 * Wire up all the `onXxxx` callbacks.
 *
 * Usage: Child (composite) / Sandwich-Meat
 * ========================================
 *
 * Request a `PreviewFeature.ForChildren[K]` in the component's props.
 * Request a `PreviewFeature.State[K]` in the component's props.
 * Call `ForChildren.forChild` for each child.
 */
final class PreviewFeature[S, K]($: CompState.WriteAccess[S], lens: Lens[S, State[K]])(implicit EK: Equal[K])
  extends ForChildren[K] {

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
    $.modState(s =>
      if (lens.get(s) exists hasKey(k))
        lens.set(None)(s)
      else
        s)

  def onEdit(k: K): Callback =
    $.modState(s =>
      if (!lens.get(s).exists(i => i.changedSinceFocus && hasKey(k)(i)))
        lens.set(Some(FocusData(k, true)))(s)
      else
        s)

  def showPreview_?(state: State[K], isDirty: => Boolean): Boolean =
    state.exists(_.changedSinceFocus || isDirty)

  def state(s: S): State[K] =
    lens.get(s)

  def forChild(k: K, s: S): ForChild =
    forChild(k, state(s))

  override def forChild(k: K, s: State[K]): ForChild = {
    val self = this
    new ForChild {
      override val focusData                          = s.filter(hasKey(k))
      override def showPreview_?(isDirty: => Boolean) = self.showPreview_?(focusData, isDirty)
      override def onFocus                            = self onFocus k
      override def onBlur                             = self onBlur k
      override def onEdit                             = self onEdit k
      override def toString                           = focusData.toString
    }
  }
}

object PreviewFeature {

  type State[+K] = Option[FocusData[K]]

  def initState: State[Nothing] =
    None

  case class FocusData[+K](key: K, changedSinceFocus: Boolean)

  trait ForChildren[-K] {
    def forChild(k: K, s: State[K]): ForChild
  }

  trait ForChild {
    val focusData: Option[FocusData[Any]]
    def showPreview_?(isDirty: => Boolean): Boolean
    def onFocus: Callback
    def onBlur: Callback
    def onEdit: Callback

    final def preview[A](isDirty: => Boolean)(a: => A): Option[A] =
      if (showPreview_?(isDirty)) Some(a) else None
  }

  object AlwaysShow extends ForChild {
    override val focusData                          = Some(FocusData((), true))
    override def showPreview_?(isDirty: => Boolean) = true
    override def onFocus                            = Callback.empty
    override def onBlur                             = Callback.empty
    override def onEdit                             = Callback.empty
  }

  object NeverShow extends ForChild {
    override val focusData                          = None
    override def showPreview_?(isDirty: => Boolean) = false
    override def onFocus                            = Callback.empty
    override def onBlur                             = Callback.empty
    override def onEdit                             = Callback.empty
  }

  @inline def FixKey[K] = new FixKey[K]
  final class FixKey[K] {
    type Feature[S]  = PreviewFeature[S, K]
    type ForChildren = PreviewFeature.ForChildren[K]
    type State       = PreviewFeature.State[K]
  }
}
