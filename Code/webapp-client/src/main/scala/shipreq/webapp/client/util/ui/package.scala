package shipreq.webapp.client.util

import japgolly.scalajs.react._
import org.scalajs.dom.HTMLInputElement

package object ui {
  // TODO stop using InputEvent, use ReactEventI
  type InputEvent = SyntheticEvent[HTMLInputElement]
  type ReactEventI = SyntheticEvent[HTMLInputElement]

  type InputGatewayE[M[_], S, A] = InputGateway[M, S, EditMode, A, _]
}