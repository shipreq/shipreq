package shipreq.webapp.client.lib.ui

import japgolly.scalajs.jquery.TextComplete
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js.{Dynamic, UndefOr, undefined}
import scalaz.effect.IO
import shipreq.base.util.{Px, Must}
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
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

  def must[A, N](m: Must[A], outputOnFailure: String = UiText.mustFailed)(render: A => N)(implicit x: ReactTag => N): N = {
    m.fold(e => {
      dom.console.error(e) // side-effect!
      <.span(^.cls := "mustfailed", ^.color.red, outputOnFailure)
    }, render)
  }

  /** A is for auto! */
  @inline def mustA[A, N](m: Must[A], outputOnFailure: String = UiText.mustFailed)(implicit x: ReactTag => N, y: A => N): N =
    must(m, outputOnFailure)(y)

  def textComplete[E <: html.Element](target: E, strategies: TextComplete.Strategies, onUpdate: => (String => IO[Unit]))(implicit E: TextEditor.OfType[E]): Unit = {
    if (strategies.nonEmpty) {
      val tgt = Dynamic.global.$(target)
      TextComplete(tgt, strategies)
      TextComplete.onSelect(tgt) {
        onUpdate(E.value(target)).unsafePerformIO()
      }
    }
  }
}
