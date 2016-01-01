package shipreq.webapp.client.app.cfg

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{CallbackTo, Callback, ReactElement}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.Assets

package object shared extends EditorExt {

  type SimpleEditor[I] = SimpleEditor2[I, I]

  type SimpleEditor2[A, B] = Editor[A, B, CallbackTo, Unit, Unit, Callback, ReactElement]

  def rowStatusRowClass(rs: RowStatus): String = rs match {
    case RowStatus.Sync      => "sync"
    case RowStatus.Locked    => "locked"
    case RowStatus.Failed(_) => "failed"
  }

  def rowStatusCtrls(rs: RowStatus, syncCtrls: => TagMod): TagMod =
    rowStatusCtrlsFold(rs, sync = syncCtrls, t => t, t => t)

  def rowStatusCtrlsFold(rs: RowStatus, sync: => TagMod, locked: ReactTag => TagMod, failed: ReactTag => TagMod): TagMod = rs match {
    case RowStatus.Sync      => sync
    case RowStatus.Locked    => locked(Assets.spinner)
    case RowStatus.Failed(r) => failed(<.button(^.onClick --> r, UiText.Cfg.retryFailedButton))
  }

  def abortNewButton(cb: Callback): ReactTag =
    <.button(^.onClick --> cb, UiText.Cfg.abortNewButton)
}