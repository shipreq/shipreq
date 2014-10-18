package shipreq.webapp.client.util.ui

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._

object Util {

  def textChangeRecv[R](f: String => R): InputEvent => R =
    e => f(e.target.value)

  def checkbox(check: Boolean) =
    input(`type` := "checkbox", checked := check)
}
