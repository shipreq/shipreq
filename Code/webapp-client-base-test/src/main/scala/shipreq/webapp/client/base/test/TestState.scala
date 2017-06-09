package shipreq.webapp.client.base.test

import japgolly.microlibs.testutil.TestUtil
import japgolly.univeq.UnivEq
import org.scalajs.dom.Element
import scalacss.internal.StyleA
import teststate.run.Report.AssertionSettings
import shipreq.base.util.DebugImplicits

object TestState
 extends teststate.Exports
    with teststate.domzipper.sizzle.Exports
    with teststate.ExtNyaya
    with teststate.ExtScalaJsReact
    with teststate.ExtScalaz
    with DebugImplicits {

  implicit class StyleAExt(private val self: StyleA) extends AnyVal {
    def selector: String =
      "." + self.className.value
  }

  implicit val displayTestReq: Display[TestClientProtocol.Req] =
    Display(i => s"${i.r.fn}: ${i.input}")

  // TODO Add to DomZipper exports
  implicit def univEqDomElement[D <: Element] = UnivEq.force[D]

  def KB = japgolly.scalajs.react.test.SimEvent.Keyboard

  // TODO Patch TestState to support using custom fail instead of throwing
  def assertTestState(r: Report[String], onFailure: => Unit = ())(implicit as: AssertionSettings, se: DisplayError[String]): Unit =
    r.failureReason match {
      case None =>
        as.onPass.print(r)
      case Some(f) =>
        onFailure
        as.onFail.print(r)
        f.cause.foreach(_.printStackTrace())
        TestUtil.fail(f.failure)
    }
}
