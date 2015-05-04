package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.test._
import scala.scalajs.js.Dynamic
import scalaz.std.string.stringInstance
import scalaz.std.vector.vectorEqual
import org.scalajs.dom.raw.HTMLInputElement
import utest._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject2._
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.test.Sizzle

object AutoCompleteTest extends TestSuite {

  shipreq.webapp.client.app.ui.Style // Ensure initialised

  def editor(ac: TC.Strategies) =
    ReactComponentB[String]("AutoComplete test")
      .getInitialState(s => s)
      .render { $ =>
        def change = (e: ReactEventI) => $.setStateIO(e.target.value)
        <.input(^.`type` := "text", ^.value := $.state, ^.onChange ~~> change)
      }
      .componentDidMount { $ =>
        val n = $.getDOMNode().asInstanceOf[HTMLInputElement]
        UI.textComplete(n, ac, $.setStateIO(_))
      }
      .domType[HTMLInputElement]
      .build

  def trigger(c: ReactComponentM[_, String, _, TopNode], text: String): Unit =
    Dynamic.global.$(c.getDOMNode()).textcomplete("trigger", text)

  lazy val acReqItems = AutoComplete.reqItems(project, plainText)
  lazy val acReqP     = AutoComplete.req(textSearch, acReqItems, prefix = true)
  lazy val cReqP      = ReactTestUtils renderIntoDocument editor(acReqP)("")

  // TODO Write more AutoComplete tests

  override def tests = TestSuite {

    'reqPrefixed {
      def getCompleteOptions =
        for (e <- Sizzle(".textcomplete-item a div div:first-child")) yield e.textContent

      def testMF(input: String)(exp: Int*): Unit =
        test(input)(exp.map("MF-" + _): _*)

      def test(input: String)(exp: String*): Unit = {
        trigger(cReqP, input)
        assertEq(getCompleteOptions.toVector, exp.toVector)
      }

      'pubid {
        testMF("[mf1")(1, 10, 11, 12, 13, 14, 15, 16, 17, 18)
        testMF("[mf8")(8)
        testMF("[mf-8")(8)
        test("[FR")("FR-1", "FR-2")
        testMF("[14")(14)
      }
      'title {
        testMF("[level")(12, 22)
        testMF("[Collab")(9, 10, 11)
      }
      'ignoreCase {
        testMF("[require")(12, 13, 22, 23, 24)
        testMF("[REQUIRE")(12, 13, 22, 23, 24)
      }
    }

  }
}
