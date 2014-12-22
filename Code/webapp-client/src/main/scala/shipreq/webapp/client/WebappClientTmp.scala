package shipreq.webapp.client

import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js._
import scalatags.generic.AttrPair
import scalaz.Maybe
import scalaz.effect.IO

object WebappClientTmp {


  object WCTmpImplicits {


    implicit final class CompStateAccessOps2asdfdaswf[C[_], S](c: C[S])(implicit C: CompStateAccess[C]) {

      @inline def stateIO: IO[S] =
        IO(c.state)

      @inline def setStateIO(s: S, cb: OpCallback = undefined): IO[Unit] =
        IO(c.setState(s, cb))

      @inline def modStateIO(f: S => S, cb: OpCallback = undefined): IO[Unit] =
        IO(c.modState(f, cb))
    }

    implicit def fuckyouscala[P,S](b: BackendScope[P,S]) =
      new CompStateAccessOps2asdfdaswf(b: ComponentScope_SS[S])

    implicit final class SzRExt_Attr222(val a: Attr) extends AnyVal {

      // TODO should also add :=?

      def ~~>?(io: Option    [IO[Unit]]): Modifier = io.map(a ~~> _)
//      def ~~>?(io: Maybe     [IO[Unit]]): Modifier = io.map(a ~~> _)
//      def ~~>?(io: js.UndefOr[IO[Unit]]): Modifier = io.map(a ~~> _)
//      def ~~>?(io: Option[IO[Unit]]): AttrPair[VDomBuilder, js.Function] = io.map(a ~~> _)
//      def ~~>?(io: js.UndefOr[IO[Unit]]) = io.map(a ~~> _)
//      def ~~>?(io: Option[IO[Unit]]) = io.map(a ~~> _)

//      def ~~>[N <: dom.Node, E <: SyntheticEvent[N]](eventHandler: E => IO[Unit]) =
//        a.==>[N, E](eventHandler(_).unsafePerformIO())
    }

    implicit def asdlkkjfjhaslkdjasdf[A <% Modifier](x: js.UndefOr[A]): Modifier =
      x.map(v => v: Modifier).toOption
  }
}
