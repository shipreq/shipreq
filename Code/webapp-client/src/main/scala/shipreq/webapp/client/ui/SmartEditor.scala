package shipreq.webapp.client.ui

import scalaz.{Bind, Equal}
import scalaz.syntax.bind._
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.shared.validation.ValidatorPlus

class SmartEditor[S, I: Equal, C, O, M[_] : Bind : Optional2](
    vs     : S => ValidatorPlus[I, C, O],
    s2mc   : S => M[C],
    ig     : InputGatewayS[M, S, I],
    trySave: S => IO[S]) {

  final type IL = WeirdLens[M, S, S, I]

  private def change(iL: IL)(i: I) =
    ReactS.mod((s: S) =>
      iL.set(s, vs(s).liveCorrect(i)) getOrElse s)

  // TODO does flatMap lose previous callbacks? And should it?
  private def cancelChange(iL: IL)(callback: IO[Unit]) =
    ReactS.get[S].flatMap(s => // TODO get.flatMap seems needlessly inefficient
      s2mc(s).mapReactS(c => change(iL)(vs(s) ci c) addCallback callback))

  private def correctInput(iL: IL) =
    ReactS.mod[S](s1 => {
      val v = vs(s1)
      val r = for {
        i1 <- iL.getO(s1)
        c = v.correct(i1)
        i2 = v.ci(c)
        s2 <- iL.setO(s1, i2) if !implicitly[Equal[I]].equal(i1, i2)
      } yield s2
      r getOrElse s1
    })

  private def editEnd(iL: IL) =
    correctInput(iL).liftIO >> ReactS.modT(trySave)

  def render[V](editor: Editor[I, V], T: ComponentStateFocus[S]): M[V] = {
    val s = T.state
    ig(s).map {

      case EditAllowed(i, iL) =>
        val e = vs(s).correctAndValidate(i).swap.toOption.map(_.toText)
        editor.renderRW(i, e, change(iL), cancelChange(iL), editEnd(iL), T)

      case ReadOnly(i) =>
        editor.renderRO(i, T)
    }
  }
}
