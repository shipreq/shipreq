package shipreq.webapp.client

import org.scalajs.dom.html
import teststate.domzipper.{DomZipper => DZ}
import teststate.typeclass.Show

package object test
  extends teststate.domzipper.sizzle.Exports {

  object PrepareEnv {
    def apply(): Unit = ()

    // Initialise styles
    shipreq.webapp.client.app.Style

    // console.error is undefined by Scala.JS due to PhantomJS being a piece of shit
    def console = scalajs.js.Dynamic.global.console
    console.error = console.info
  }

  // TODO Hmmmm
  implicit def showDomZipper[D <: DZ.Base, N <: DZ.NextBase, Out[_]]: Show[DZ[D, N, Out]] =
    Show(_.describeLoc)

  import japgolly.scalajs.react._

  def removeReactIds(html: String): String = // TODO Remove
    html.replaceAll(""" data-reactid=".*?"""", "")

  def reactDomZipper[D <: TopNode](c: CompScope.Mounted[D]): DomZipperAt[D] =
    DomZipper("React component", c.getDOMNode()) // TODO Can't we use .displayName?
      .scrubHtml(removeReactIds)
}
