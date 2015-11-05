package shipreq.webapp.client.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._

object Util {

  def textChangeRecv[R](f: String => R): InputEvent => R =
    e => f(e.target.value)

  def checkbox(checked: Boolean) =
    input(`type` := "checkbox", checked && (all.checked := "checked"))
}
