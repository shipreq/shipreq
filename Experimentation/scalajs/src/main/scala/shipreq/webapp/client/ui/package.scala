package shipreq.webapp.client

import org.scalajs.dom.HTMLInputElement
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._

package object ui {

  type ErrorMsg = String

  type InputEvent = SyntheticEvent[HTMLInputElement]

  object Util {

    def textChangeRecv[R](f: String => R): InputEvent => R =
      e => f(e.target.value)

    def checkbox(checked: Boolean) =
      input(`type` := "checkbox", checked && (all.checked := "checked"))
  }
}