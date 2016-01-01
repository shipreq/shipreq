package shipreq.webapp.client

package object test {

  @inline final def $ = Sizzle

  object PrepareEnv {
    def apply(): Unit = ()

    // Initialise styles
    shipreq.webapp.client.app.Style

    // console.error is undefined by Scala.JS due to PhantomJS being a piece of shit
    def console = scalajs.js.Dynamic.global.console
    console.error = console.info
  }
}
