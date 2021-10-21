package shipreq.webapp.client.public

import japgolly.scalajs.react.vdom.all._

/**
  * Not using ScalaCss because of how much increase it adds to the JS.
  */
object Styles {

  private class CssBuilder {
    final type CSS = String

    private var open = true
    private var css: CSS = ""

    private def ensurePreInstall() = assert(open, "CSS already installed.")

    def add(cssStr: CSS): Unit = {
      ensurePreInstall()
      css += cssStr
    }

    def add(key: Int, f: String => CSS): String = {
      val className = "s___x" + key + "x"
      assert(!css.contains(className))
      add(f(className))
      className
    }

    def addClass(key: Int, f: String => CSS): TagMod =
      className := add(key, className => f("." + className))

    def addToDocument(): Unit = {
      import org.scalajs.dom.document
      import org.scalajs.dom.HTMLStyleElement

      ensurePreInstall()
      open = false

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

    cssBuilder.add("body {background:#fbfcfd}")

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

    val loggedIn = TagMod(
      fontWeight.bold,
      color := "#d00")

    val footer = TagMod(
      position.relative,
      fontSize := "0.85rem",
      paddingTop := "0.1em",
      paddingBottom := "0.1em",
      background := "#edeeef",
      borderTop := "1px solid rgba(34,36,38,.15)",
      textAlign.center)

    val copyright = TagMod(
      position.absolute,
      top := "0",
      right := "1ex",
      color := "#a4a4a4")

    val main = TagMod(
      flex := "1",
      paddingBottom := "2em")
  }
  layout // eager eval

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
      cssBuilder.addClass(1, c =>
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
      color := "hsla(209,100%,15%,.86)", // "Ship" in ShipReq with lower L%
      fontSize := "1.5em",
      letterSpacing := "0.25px",
      lineHeight := "1.3em")

    val yap2 = TagMod(
      color := "hsla(207,100%,5%,.9)", // "Req" in ShipReq with lower L%
      fontSize := "1.15em",
      marginTop := "3em",
      lineHeight := "1.4em")

    val pointAtForm: TagMod =
      cssBuilder.addClass(2, c =>
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
      boxShadow := "0 2px 4px 0 rgba(192,214,233,.15), 0 2px 10px 0 rgba(192,214,233,.25)",
      borderRadius := ".28571429rem",
      padding := "1em")

    val yap =
      cssBuilder.addClass(3, c =>
        s"""
           |$c { flex:1; padding:2.5em; }
           |@media screen and (max-width: 75ex) { $c { font-size: 2.0vw; padding:2em;}}
         """.stripMargin)
  }
  landingPage // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object login {
    val part1          = TagMod(width := "46ex", margin := "4em auto 0 auto")
    val part2          = TagMod(width := "70ex", margin := "6em auto 0 auto")
    val form           = TagMod(paddingTop := "3em")
    val passwordLabel  = TagMod(display.flex, justifyContent.spaceBetween)
    val forgotPassword = TagMod(fontWeight.normal, cursor.pointer)
    val bottomRow      = TagMod(display.flex, width := "100%", alignItems.baseline)
    val rememberMe     = TagMod(flex := "1")
    val submitCont     = TagMod(textAlign.right, paddingTop := "0.4em")
  }
  login // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object resetPassword {
    def part1        = login.part1
    val part2        = TagMod(width := "54ex", margin := "6em auto 0 auto")
    def submitCont   = login.submitCont
  }
  resetPassword // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object register1 {
    val part1        = TagMod(width := "48ex", margin := "5em auto 0 auto")
    val part2        = TagMod(width := "60ex", margin := "6em auto 0 auto")
    val part0        = TagMod(width := "66ex", margin := "6em auto 0 auto")
    def submitCont   = login.submitCont
  }
  register1 // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object register2 {
    val part1        = TagMod(width := "72ex", margin := "4em auto 0 auto")
    val part2        = TagMod(width := "64ex", margin := "6em auto 0 auto")
    def submitCont   = login.submitCont
    val begin        = fontWeight.bold
  }
  register2 // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object legal {
    val cont: TagMod =
      cssBuilder.addClass(4, c =>
        s"""
           |$c {
           |  max-width:144ex;
           |  margin: 1em auto 0 auto;
           |  padding:0 4vw;
           |  color: #444;
           |}
           |$c section {margin-top: 2em}
           |$c section .h { color: #000; font-weight:bold; }
           |$c section .p { margin-top:0.7ex; }
           |$c section ul { margin-top:1ex; }
           |$c section li { margin-top:0.7ex; }
           |$c section li .a { text-decoration: underline; }
           |$c footer {margin-top: 2em}
           |$c footer .g { font-size: 0.72em; color: #bbb; }
         """.stripMargin)
    val sectionH = cls := "h"
    val sectionP = cls := "p"
    val liA = cls := "a"
    val generatedBy = cls := "g"
  }
  legal // eager eval

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object common {
    def tokenInvalidCont = resetPassword.part2
  }
}
