package shipreq.webapp.client.util

import japgolly.scalajs.react._
import org.scalajs.dom.HTMLInputElement

package object ui {
  type InputEvent = SyntheticEvent[HTMLInputElement]

  type InputGatewayE[M[_], S, A] = InputGateway[M, S, EditMode, A, _]
}