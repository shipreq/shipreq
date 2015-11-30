package shipreq.webapp.client.lib.ui

import japgolly.scalajs.jquery.TextComplete
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import scala.scalajs.js.{Dynamic, UndefOr, undefined}
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.base.util.{NonEmptyVector, Util}
import shipreq.webapp.base.AppConsts.assetPath
import shipreq.webapp.base.UiText
import shipreq.webapp.client.util.{DomUtil, Off, On}

object UI {

  def textChangeRecv[R](f: String => R): ReactEventI => R =
    e => f(e.target.value)

  val checkbox: On => ReactTag =
    On.memo(on =>
      <.input(
        ^.`type` := "checkbox",
        ^.checked := (on :: On)))

  val checkboxAlwaysOn =
    UI.checkbox(On)(^.readOnly := true, ^.disabled := true)

  def checkboxOfSetPresence[A](as: Set[A])(a: A, update: Set[A] => Callback): ReactTag = {
    val currentState = On <~ as.contains(a)
    def toggled = currentState match {
      case On  => as - a
      case Off => as + a
    }
    checkbox(currentState)(^.onChange --> update(toggled))
  }

  /**
   * Clicking, or pressing space = change.
   */
  def checkboxLikeEventHandlers(onChange: Callback): TagMod = {
    def handleKey: ReactKeyboardEventH => Callback =
      DomUtil.keyCodeSwitch(_) {
        case KeyCode.Space => onChange
      }
    TagMod(
      ^.onClick   --> onChange,
      ^.onKeyDown ==> handleKey)
  }

  val sepComma: TagMod = ", "
  val sepSpace: TagMod = " "

  def vector[A, B](as: Vector[A], separator: TagMod)(renderEach: A => B)(implicit g: B => TagMod): ReactTag =
    <.span(
      NonEmptyVector.option(as)
        .map(_.intercalateF(separator)(g compose renderEach).whole))

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
    case RowStatus.Failed(r) => failed(<.button(^.onClick --> r, UiText.Cfg.retryFailedButton))
  }

  val spinner =
    <.img(
      ^.cls := "spinner",
      ^.src := s"$assetPath/loading-spin.svg")

  def abortNewButton(cb: Callback): ReactTag =
    <.button(^.onClick --> cb, UiText.Cfg.abortNewButton)

  @deprecated("Moved to AutoCompleteFeature","")
  def textComplete[E <: html.Element](target: E, strategies: TextComplete.Strategies, onUpdate: => (String => Callback))(implicit E: TextEditor.OfType[E]): Callback =
    Callback.ifTrue(strategies.nonEmpty, Callback {
      val tgt = Dynamic.global.$(target)
      TextComplete(tgt, strategies)
      TextComplete.onSelect(tgt) {
        onUpdate(E.value(target)).runNow()
      }
    })

  @deprecated("Moved to AutoCompleteFeature","")
  def textCompleteDestroy(node: html.Element) = Callback {
    val $n = Dynamic.global.$(node)
    TextComplete.destroy($n)
  }

  @deprecated("Moved to AutoCompleteFeature","")
  def installTextComplete[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : CompScope.DuringCallbackM[P, S, B, N] => E,
          strategies: (P, B) => ReusableVal[TextComplete.Strategies],
          onUpdate  : (P, B) => String => Callback)
         (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    _.componentDidMount($ => Callback {
      val n = getNode($)
      te.focus(n)
      te.select(n)
      textComplete(n, strategies($.props, $.backend), onUpdate($.props, $.backend)).runNow()
    })
    .componentDidUpdate(i => Callback {
      val $  = i.$
      val p1 = i.prevProps
      val p2 = $.props
      val b  = $.backend
      val s1 = strategies(p1, b)
      val s2 = strategies(p2, b)
      if (s1 ~/~ s2) {
        val n = getNode($)
        textCompleteDestroy(n).runNow()
        textComplete(n, s2, onUpdate($.props, b)).runNow()
      }
    })
    .componentWillUnmount($ =>
      textCompleteDestroy(getNode($)))

  @deprecated("Moved to AutoCompleteFeature","")
  def installTextCompleteP[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: P => ReusableVal[TextComplete.Strategies],
          onUpdate  : P => String => Callback)
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    installTextComplete(getNode(_).get, (p, _) => strategies(p), (p, _) => onUpdate(p))

  @deprecated("Moved to AutoCompleteFeature","")
  def installTextCompleteB[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: B => ReusableVal[TextComplete.Strategies],
          onUpdate  : B => String => Callback)
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    installTextComplete(getNode(_).get, (_, b) => strategies(b), (_, b) => onUpdate(b))

}
