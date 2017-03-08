package shipreq.webapp.client.project.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.internal.JsUtil
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.base.util.Vector1
import shipreq.webapp.client.base.jsfacade.TextComplete
import shipreq.webapp.client.project.lib.TextEditor

/**
 * Usage
 * =====
 * - Apply `install` in `ReactComponentB.configure`.
 * - Satisfy `scalac`. In most cases you'll want to add `ForChild` to the component's props and use `installP`.
  **/
object AutoCompleteFeature {

  implicit val reusabilityStrategies: Reusability[Strategies] =
    Reusability((a, b) =>
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

  def install[P, C <: Children, S, B, E <: html.Element](
          getNode   : ScalaComponent.MountedImpure[P, S, B] => E,
          strategies: (P, B) => ForChild,
          onUpdate  : (P, B) => String => Callback)
         (implicit te: TextEditor.OfType[E]): ScalaComponentConfig[P, C, S, B] =
    _.componentDidMount($ => Callback {
      val n = getNode($.mountedImpure)
      te.focus(n)
      te.select(n)
      lowLevelInstall(n,
        JsUtil jsArrayFromTraversable strategies($.props, $.backend),
        onUpdate($.props, $.backend))
        .runNow()
    })
    .componentDidUpdate(i => Callback {
      val $ = i.mountedImpure
      val p1 = i.prevProps
      val p2 = i.currentProps
      val b  = i.backend
      val s1 = strategies(p1, b)
      val s2 = strategies(p2, b)
      if (s1 ~/~ s2) {
        val n = getNode($)
        lowLevelDestroy(n).runNow()
        lowLevelInstall(n, JsUtil.jsArrayFromTraversable(s2), onUpdate($.props, b)).runNow()
      }
    })
    .componentWillUnmount($ =>
      lowLevelDestroy(getNode($.mountedImpure)))

  def installP[P, C <: Children, S, B, E <: html.Element](
          getNode   : ScalaComponent.MountedImpure[P, S, B] => E,
          strategies: P => ForChild,
          onUpdate  : P => String => Callback)
        (implicit te: TextEditor.OfType[E]): ScalaComponentConfig[P, C, S, B] =
    install(getNode, (p, _) => strategies(p), (p, _) => onUpdate(p))

  def installB[P, C <: Children, S, B, E <: html.Element](
          getNode   : ScalaComponent.MountedImpure[P, S, B] => E,
          strategies: B => ForChild,
          onUpdate  : B => String => Callback)
        (implicit te: TextEditor.OfType[E]): ScalaComponentConfig[P, C, S, B] =
    install(getNode, (_, b) => strategies(b), (_, b) => onUpdate(b))

  def installBP[P, C <: Children, S, B, E <: html.Element](
          getNode   : ScalaComponent.MountedImpure[P, S, B] => E,
          strategies: B => ForChild,
          onUpdate  : P => String => Callback)
        (implicit te: TextEditor.OfType[E]): ScalaComponentConfig[P, C, S, B] =
    install(getNode, (_, b) => strategies(b), (p, _) => onUpdate(p))
}
