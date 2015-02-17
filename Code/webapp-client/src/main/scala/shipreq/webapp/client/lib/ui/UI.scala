package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.base.util.Must
import scalaz.effect.IO
import shipreq.webapp.base.UiText

object UI {

  def textChangeRecv[R](f: String => R): ReactEventI => R =
    e => f(e.target.value)

  def checkbox(check: Boolean) =
    <.input(
      ^.`type` := "checkbox",
      ^.checked := check)

  def rowStatusRowClass(rs: RowStatus): String = rs match {
    case RowStatus.Sync      => "sync"
    case RowStatus.Locked    => "locked"
    case RowStatus.Failed(_) => "failed"
  }

  def rowStatusCtrls(rs: RowStatus, syncCtrls: => TagMod): TagMod =
    rowStatusCtrlsFold(rs, sync = syncCtrls, t => t, t => t)

  def rowStatusCtrlsFold(rs: RowStatus, sync: => TagMod, locked: ReactTag => TagMod, failed: ReactTag => TagMod): TagMod = rs match {
    case RowStatus.Sync      => sync
    case RowStatus.Locked    => locked(spinner)
    case RowStatus.Failed(r) => failed(<.button(^.onClick ~~> r, UiText.Cfg.retryFailedButton))
  }

  val spinner =
    <.img(
      ^.cls := "spinner",
      ^.src := "/assets/loading-spin.svg")

  def abortNewButton(cb: IO[Unit]): ReactTag =
    <.button(^.onClick ~~> cb, UiText.Cfg.abortNewButton)

  def must[A](m: Must[A], userMsg: String = "ERR")(render: A => ReactElement): ReactElement = {
    m.fold(e => {
      dom.console.error(e) // side-effect!
      <.span(^.cls := "mustfailed", ^.color.red, userMsg)
    }, render)
  }
}
