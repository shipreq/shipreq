package shipreq.webapp.client

import testate.domzipper.{DomZipper => DZ}
import testate.typeclass.Display

package object test
  extends testate.domzipper.sizzle.Exports {

  object PrepareEnv {
    def apply(): Unit = ()

    // Initialise styles
    shipreq.webapp.client.app.Style

    // console.error is undefined by Scala.JS due to PhantomJS being a piece of shit
    def console = scalajs.js.Dynamic.global.console
    console.error = console.info
  }

  // TODO Hmmmm
  implicit def displayDomZipper[D <: DZ.Base, N <: DZ.NextBase, Out[_]]: Display[DZ[D, N, Out]] =
    Display(_.describeLoc)

  import japgolly.scalajs.react._

  def removeReactIds(html: String): String = // TODO Remove
    html.replaceAll(""" data-reactid=".*?"""", "")

  def reactDomZipper[D <: TopNode](c: CompScope.Mounted[D]): DomZipperAt[D] =
    DomZipper("React component", c.getDOMNode()) // TODO Can't we use .displayName?
      .scrubHtml(removeReactIds)
}
