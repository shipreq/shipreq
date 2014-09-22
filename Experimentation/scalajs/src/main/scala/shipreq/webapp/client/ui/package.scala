package shipreq.webapp.client

import org.scalajs.dom.HTMLInputElement
import japgolly.scalajs.react._

package object ui {

  type ErrorMsg = String

  type InputEvent = SyntheticEvent[HTMLInputElement]

  type ValidatorR[S, R, I, C, O] = (S, R) => Validator[I, C, O]

  type ValidateFnR[S, R, O] = (S, R, O) => Option[ErrorMsg]

}