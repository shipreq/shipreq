package shipreq.webapp.client.ui

import scalaz.{Bind, Equal}
import scalaz.syntax.bind._
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.shared.validation.ValidatorPlus

sealed trait EditMode
object EditMode {
  case object ReadWrite extends EditMode
  case object ReadOnly extends EditMode
}

class SmartEditor[S, I, C, O, M[_]](
    vs     : S => ValidatorPlus[I, C, O],
    s2mc   : S => M[C],
    ig     : InputGatewayE[M, S, I],
    trySave: S => IO[S])(
    implicit E: Equal[I], B: Bind[M], M: Optional2[M]){

  private def change(setI: (S, I) => Option[S])(i: I) =
    ReactS.mod((s: S) =>
      setI(s, vs(s).liveCorrect(i)) getOrElse s)

  // TODO does flatMap lose previous callbacks? And should it?
  private def cancelChange(setI: (S, I) => Option[S])(callback: IO[Unit]) =
    ReactS.get[S].flatMap(s => // TODO get.flatMap seems needlessly inefficient
      s2mc(s).mapReactS(c => change(setI)(vs(s) ci c) addCallback callback))

  private def correctInput(getI: S => M[I], setI: (S, I) => Option[S]) =
    ReactS.mod[S](s1 => {
      val v = vs(s1)
      val r = for {
        i1 <- M.toOption(getI(s1))
        c = v.correct(i1)
        i2 = v.ci(c)
        s2 <- setI(s1, i2) if !E.equal(i1, i2)
      } yield s2
      r getOrElse s1
    })

  private def editEnd(getI: S => M[I], setI: (S, I) => Option[S]) =
    correctInput(getI, setI).liftIO >> ReactS.modT(trySave)

  def render[V](editor: Editor[I, V], T: ComponentStateFocus[S]): M[V] = {
    import ig.{setA => setI, getA => getI}
    val s = T.state
    ig.getRA(s).map {

      case (EditMode.ReadWrite, i) =>
        val e = vs(s).correctAndValidate(i).swap.toOption.map(_.toText)
        editor.renderRW(i, e, change(setI), cancelChange(setI), editEnd(getI, setI), T)

      case (EditMode.ReadOnly, i) =>
        editor.renderRO(i, T)
    }
  }
}
