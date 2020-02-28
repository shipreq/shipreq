package shipreq.webapp.client.project.app.pages.config_old

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo}
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.webapp.base.UiText
import shipreq.webapp.base.ClientResources

package object shared extends EditorExt {

  type SimpleEditor[I] = SimpleEditor2[I, I]

  type SimpleEditor2[A, B] = Editor[A, B, CallbackTo, Unit, Unit, Callback, VdomElement]

  def rowStatusRowClass(rs: RowStatus): String = rs match {
    case RowStatus.Sync      => "sync"
    case RowStatus.Locked    => "locked"
    case RowStatus.Failed(_) => "failed"
  }

  def rowStatusCtrls(rs: RowStatus, syncCtrls: => TagMod): TagMod =
    rowStatusCtrlsFold(rs, sync = syncCtrls, t => t, t => t)

  def rowStatusCtrlsFold(rs: RowStatus, sync: => TagMod, locked: VdomTag => TagMod, failed: VdomTag => TagMod): TagMod = rs match {
    case RowStatus.Sync      => sync
    case RowStatus.Locked    => locked(ClientResources.spinnerImg)
    case RowStatus.Failed(r) => failed(<.button(^.onClick --> r, UiText.Cfg.retryFailedButton))
  }

  def abortNewButton(cb: Callback): VdomTag =
    <.button(^.onClick --> cb, UiText.Cfg.abortNewButton)
}