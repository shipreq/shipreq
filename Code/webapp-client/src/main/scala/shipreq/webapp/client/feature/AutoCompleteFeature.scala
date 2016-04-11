package shipreq.webapp.client.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.base.util.Vector1
import shipreq.webapp.client.jsfacade.TextComplete
import shipreq.webapp.client.lib.TextEditor

/**
 * Usage
 * =====
 * - Apply `install` in `ReactComponentB.configure`.
 * - Satisfy `scalac`. In most cases you'll want to add `ForChild` to the component's props and use `installP`.
  **/
object AutoCompleteFeature {

  implicit val reusabilityStrategies: Reusability[Strategies] =
    Reusability.fn((a, b) =>
      (a eq b) ||
      a.corresponds(b)(_ eq _))

  type Strategies = Vector[TextComplete.Strategy]

  implicit def autoLiftSingleStrategy[A](a: A)(implicit f: A => TextComplete.Strategy): Strategies =
    Vector1(f(a))

  type ForChild = Strategies

  /**
   * Public only for unit-tests. For React components, use one of the `install…` methods.
   */
  def lowLevelInstall[E <: html.Element](target    : E,
                                         strategies: TextComplete.Strategies,
                                         onUpdate  : => (String => Callback))
                                        (implicit E: TextEditor.OfType[E]): Callback =
    Callback.unless(strategies.isEmpty)(Callback {
      val tgt = js.Dynamic.global.$(target)
      TextComplete(tgt, strategies)
      TextComplete.onSelect(tgt) {
        onUpdate(E.value(target)).runNow()
      }
    })

  def lowLevelDestroy(node: html.Element): Callback =
    Callback {
      val $n = js.Dynamic.global.$(node)
      TextComplete.destroy($n)
    }

  def install[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : CompScope.DuringCallbackM[P, S, B, N] => E,
          strategies: (P, B) => ForChild,
          onUpdate  : (P, B) => String => Callback)
         (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    _.componentDidMount($ => Callback {
      val n = getNode($)
      te.focus(n)
      te.select(n)
      lowLevelInstall(n, strategies($.props, $.backend).toJsArray, onUpdate($.props, $.backend)).runNow()
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
        lowLevelDestroy(n).runNow()
        lowLevelInstall(n, s2.toJsArray, onUpdate($.props, b)).runNow()
      }
    })
    .componentWillUnmount($ =>
      lowLevelDestroy(getNode($)))

  def installP[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: P => ForChild,
          onUpdate  : P => String => Callback)
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    install(getNode(_).get, (p, _) => strategies(p), (p, _) => onUpdate(p))

  def installB[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: B => ForChild,
          onUpdate  : B => String => Callback)
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    install(getNode(_).get, (_, b) => strategies(b), (_, b) => onUpdate(b))

  def installBP[P, S, B, N <: TopNode, E <: html.Element](
          getNode   : RefSimple[E],
          strategies: B => ForChild,
          onUpdate  : P => String => Callback)
        (implicit te: TextEditor.OfType[E]): EndoFn[ReactComponentB[P, S, B, N]] =
    install(getNode(_).get, (_, b) => strategies(b), (p, _) => onUpdate(p))
}