package shipreq.webapp.client

package object test {

  object PrepareEnv {
    def apply(): Unit = ()

    // Initialise styles
    shipreq.webapp.client.app.Style

    // console.error is undefined by Scala.JS due to PhantomJS being a piece of shit
    def console = scalajs.js.Dynamic.global.console
    console.error = console.info
  }

  import shipreq.webapp.client.test.{domzipper => dz}
  import dz.DomZipper.DOM

  type DomZipperAt[+D <: DOM] = dz.DomZipperAt[D]
  type DomZipper              = DomZipperAt[DOM]
  val DomZipper               = dz.DomZipper
}
