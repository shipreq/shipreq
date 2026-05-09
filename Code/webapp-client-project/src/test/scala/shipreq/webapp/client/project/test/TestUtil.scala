package shipreq.webapp.client.project.test

import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.{EventTarget, KeyboardEvent}
import scala.scalajs.js
import scala.scalajs.js.{UndefOr, undefined}
import shipreq.webapp.member.test._

object TestUtil extends WebappTestUtil with WebappTestEquality {

  def sole[A](a: js.Array[A]): A = {
    assertEq(s"sole(Array(${a.mkString(", ")}))", a.length, 1)
    a.head
  }

  implicit class JsArrayTestExt[A](private val a: js.Array[A]) extends AnyVal {
    def sole(): A =
      a.length match {
        case 1 => a(0)
        case n => fail(s"Expected an array with one element, found $n: ${a.mkString("[",",","]")}")
    }

    def soleDom[N <: A]()(implicit ev: A <:< org.scalajs.dom.Element): N =
      sole().asInstanceOf[N]
  }

  implicit def autodomnode(c: GenericComponent.MountedRaw) =
    ReactDOM.findDOMNode(c.raw).get.asElement()

  val nopJsFn: js.Function0[js.Any] = () => ((): js.Any)

  def fakeKeyboardEvent(key            : UndefOr[String]      = undefined,
                        keyCode        : UndefOr[Int]         = undefined,
                        target         : UndefOr[EventTarget] = undefined,
                        location       : UndefOr[Double]      = undefined,
                        altKey         : Boolean              = false,
                        ctrlKey        : Boolean              = false,
                        metaKey        : Boolean              = false,
                        shiftKey       : Boolean              = false,
                        repeat         : Boolean              = false,
                        locale         : String               = "en",
                        preventDefault : js.Function0[js.Any] = nopJsFn,
                        stopPropagation: js.Function0[js.Any] = nopJsFn): KeyboardEvent = {
    val o = js.Dynamic.literal()
    key     .foreach(v => o.key      = v)
    keyCode .foreach(v => o.keyCode  = v)
    target  .foreach(v => o.target   = v)
    location.foreach(v => o.location = v)
    o.altKey          = altKey
    o.ctrlKey         = ctrlKey
    o.metaKey         = metaKey
    o.shiftKey        = shiftKey
    o.repeat          = repeat
    o.locale          = locale
    o.preventDefault  = preventDefault
    o.stopPropagation = stopPropagation
    o.asInstanceOf[dom.KeyboardEvent]
  }

  def removeEditInstructionText(s: String): String =
    s.replace("esc to cancel.", "")
      .replace("ctrl-enter to save,", "")
      .replace("enter for new line,", "")

  def removeSemanticUiFromBody(): Unit = {
    val body = dom.document.body
    var i = body.childNodes.length
    while (i > 0) {
      i -= 1
      body.childNodes(i) match {
        case e: dom.Element if e.classList.contains("ui") =>
          body.removeChild(e)
        case _ =>
      }
    }
  }
}
