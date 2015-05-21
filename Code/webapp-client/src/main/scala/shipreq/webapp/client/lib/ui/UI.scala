package shipreq.webapp.client.lib.ui

import japgolly.scalajs.jquery.TextComplete
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js.{Dynamic, UndefOr, undefined}
import scalaz.effect.IO
import shipreq.base.util.Must
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.webapp.base.UiText
import shipreq.webapp.client.util.On

object UI {

  def textChangeRecv[R](f: String => R): ReactEventI => R =
    e => f(e.target.value)

  def checkbox(on: On) =
    <.input(
      ^.`type` := "checkbox",
      ^.checked := (on :: On))

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

  def installTextComplete[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : ComponentScopeM[P, S, B, N] => E,
          strategies: (P, B) => ReusableVal[TextComplete.Strategies],
          onUpdate  : (P, B) => String => IO[Unit])
         (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    _.componentDidMount { $ =>
      val n = getNode($)
      te.focus(n)
      te.select(n)
      textComplete(n, strategies($.props, $.backend), onUpdate($.props, $.backend))
    }
    .componentDidUpdate { ($, p1, _) =>
      val p2 = $.props
      val b = $.backend
      val s1 = strategies(p1, b)
      val s2 = strategies(p2, b)
      if (s1 ~/~ s2) {
        val n = getNode($)
        val $n = Dynamic.global.$(n)
        TextComplete.destroy($n)
        textComplete(n, s2, onUpdate($.props, b))
      }
    }

  def installTextComplete2[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: P => ReusableVal[TextComplete.Strategies],
          onUpdate  : P => String => IO[Unit])
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    installTextComplete(getNode(_).get.getDOMNode(), (p, _) => strategies(p), (p, _) => onUpdate(p))

}
