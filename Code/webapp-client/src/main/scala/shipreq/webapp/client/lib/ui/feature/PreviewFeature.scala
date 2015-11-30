package shipreq.webapp.client.lib.ui.feature

import japgolly.scalajs.react._
import monocle._
import scalaz.Equal
import PreviewFeature.{FocusData, ForChild}

/**
 * Preview available:
 * - when editing and focused and (dirty or has been edited since receiving focus)
 */
class PreviewFeature[S, E, K]($: CompState.WriteAccess[S], lens: Lens[S, Option[FocusData[K]]])(implicit EK: Equal[K]) {

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

  def showPreview_?(focusData: Option[FocusData[K]], isDirty: => Boolean): Boolean =
    focusData.exists(_.changedSinceFocus || isDirty)

  def forChild(k: K, fi: Option[FocusData[K]]): ForChild[K] = {
    val self = this
    new ForChild[K] {
      override val focusData                          = fi.filter(hasKey(k))
      override def showPreview_?(isDirty: => Boolean) = self.showPreview_?(focusData, isDirty)
      override def onFocus                            = self onFocus k
      override def onBlur                             = self onBlur k
      override def onEdit                             = self onEdit k
    }
  }
}

object PreviewFeature {

  case class FocusData[+K](key: K, changedSinceFocus: Boolean)

  trait ForChild[+K] {
    val focusData: Option[FocusData[K]]
    def showPreview_?(isDirty: => Boolean): Boolean
    def onFocus: Callback
    def onBlur: Callback
    def onEdit: Callback
  }
}
