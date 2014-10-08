package shipreq.webapp.client

import japgolly.scalajs.react._
import org.scalajs.dom.HTMLInputElement
import scalaz.Bind

package object ui {
  type InputEvent = SyntheticEvent[HTMLInputElement]

  type InputGatewayS[M[_], S, I] = S => M[InputGateway[M, S, I]]

  implicit class InputGatewaySExt[M[_], S, A](val ig: InputGatewayS[M, S, A]) extends AnyVal {
    def map[B](f: A => B)(g: (A, B) => A)(implicit M:Bind[M]) =
      InputGatewayS.map[M, S, A, B](ig, f, g)
  }

}