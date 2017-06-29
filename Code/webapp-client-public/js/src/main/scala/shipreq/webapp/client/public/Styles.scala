package shipreq.webapp.client.public

import japgolly.scalajs.react.vdom.all._

/**
  * Not using ScalaCss because of how much increase it adds to the JS.
  */
object Styles {

  private class CssBuilder {
    final type CSS = String

    private var i = 0
    private var css: CSS = ""

    private def ensurePreInstall() = assert(i >= 0, "CSS already installed.")

    def add(f: String => CSS): String = {
      ensurePreInstall()
      val cn = "s_____q_" + i
      css += f(cn)
      i += 1
      cn
    }

    def addClass(f: String => CSS): TagMod =
      className := add(n => f("." + n))

    def addToDocument(): Unit = {
      import org.scalajs.dom.document
      import org.scalajs.dom.raw.HTMLStyleElement

      ensurePreInstall()
      i = -1

      def createStyleElement(styleStr: String): HTMLStyleElement = {
        val e = document.createElement("style").asInstanceOf[HTMLStyleElement]
        e.`type` = "text/css"
        e appendChild document.createTextNode(styleStr)
        e
      }

      def installStyle(style: HTMLStyleElement): Unit =
        document.head appendChild style

      val cssMin = css
        .replaceAll("[\r\n]+", " ")
        .replaceAll(" +", " ")
        .replaceAll(" ?([{};:]) ?", "$1")
        .trim

      installStyle(createStyleElement(cssMin))
    }
  }

  private val cssBuilder = new CssBuilder
  def addToDocument(): Unit = cssBuilder.addToDocument()

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object layout {

    val cont = TagMod(
      display.flex,
      flexDirection.column,
      alignItems.stretch,
      minHeight := "100%")

    val header = TagMod(
      width := "100%",
      display.flex,
      alignItems.flexStart)

    private def headerLogoEm = 4

    val headerSides = TagMod(
      width := s"${headerLogoEm + 1}em",
      padding := "0.5em")

    val headerMid = TagMod(
      flex := "1",
      textAlign.center,
      paddingTop := "0.2em")

    val headerLogo = TagMod(
      width := s"${headerLogoEm}em",
      height := s"${headerLogoEm}em",
      display.block)

    val linkSep = TagMod(
      padding := "0 1em",
      color := "#aaa")

    val linkActive = TagMod(
      fontWeight.bold,
      color := "#000")

    val footer = TagMod(
      fontSize := "0.85rem",
      paddingTop := "0.1em",
      paddingBottom := "0.1em",
      background := "#edeeef",
      borderTop := "1px solid rgba(34,36,38,.15)",
      textAlign.center)

    val footerTxt =
      color := "#888"

    val main = TagMod(
      flex := "1",
      paddingBottom := "2em")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object landingPage {

    val cont = TagMod(
      maxWidth := "144ex",
      margin := "1em auto 0 auto",
      padding := "0 4vw")

    val banner: TagMod =
      width := "80%"

    // TODO Fix the serif in the LandingPage tagline. Chrome/Firefox diffs
    val tagline: TagMod =
      cssBuilder.addClass(c =>
        s"""
           |$c {
           |  margin-top: 0.6rem;
           |  font-family: Georgia,"Times New Roman",Times,serif;
           |  font-size: 2.3em;
           |  color: #252a2f;
           |  line-height:1em;
           |}
           |@media screen and (max-width: 128ex) { $c { font-size: 3.4vw; }}
         """.stripMargin)

    val part2 = TagMod(
      display.flex,
      marginTop := "5em")

    val yap1 = TagMod(
      color := "hsl(209, 100%, 15%)", // "Ship" in ShipReq with lower L%
      fontSize := "1.6em",
      lineHeight := "1.3em")

    val yap2 = TagMod(
      color := "hsl(207, 100%, 5%)", // "Req" in ShipReq with lower L%
      fontSize := "1.15em",
      marginTop := "3em",
      lineHeight := "1.5em")

    val pointAtForm: TagMod =
      cssBuilder.addClass(c =>
        s"""$c {
           |  -webkit-transform: rotate(360deg);
           |  border-style: dashed;
           |  border-color: rgba(0, 0, 0, 0);
           |  border-width: .53em;
           |  display: -moz-inline-box;
           |  display: inline-block;
           |  height: 0;
           |  line-height: 0;
           |  position: relative;
           |  vertical-align: middle;
           |  width: 0;
           |  top: -.13em;
           |  border-left: solid 1em #9fc4e8;
           |  left: .25em;
           |}
         """.stripMargin)

    val formCont = TagMod(
      width := "44ex",
      maxWidth := "60%")

    val form = TagMod(
      background := "#e5edf3",
      border := "solid 1px #c0d6e9",
      boxShadow := "0 2px 4px 0 rgba(192, 214, 233, .15), 0 2px 10px 0 rgba(192, 214, 233, .25)",
      borderRadius := ".28571429rem",
      padding := "1em")

    val formSubmit = marginRight := "0"

    val yap =
      cssBuilder.addClass(c =>
        s"""
           |$c { flex:1; padding:2.5em; }
           |@media screen and (max-width: 75ex) { $c { font-size: 2.0vw; padding:2em;}}
         """.stripMargin)
  }
  landingPage // eager eval

}
