package shipreq.webapp.client

import org.scalajs.dom.HTMLInputElement
import japgolly.scalajs.react._

package object ui {

  type ErrorMsg = String

  type InputEvent = SyntheticEvent[HTMLInputElement]

  type ValidatorW[S, W, I, C, O] = (S, W) => Validator[I, C, O]

  type ValidateFnW[S, W, O] = (S, W, O) => Option[ErrorMsg]

}