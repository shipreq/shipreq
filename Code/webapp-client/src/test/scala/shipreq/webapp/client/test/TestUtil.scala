package shipreq.webapp.client.test

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom, dom.{EventTarget, KeyboardEvent}
import scala.scalajs.js, js.{undefined, UndefOr}
import scalaz.Equal
import scalaz.std.AllInstances._
import nyaya.gen.Gen
import shipreq.base.util.Debug._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.test.WebappTestUtil
import shipreq.webapp.client.app.cfg.shared._
import RowStatus._

object TestUtil extends WebappTestUtil {

  def assertRowStatusFailed(r: RowStatus): RowStatus.Failed =
    r match {
      case f@ RowStatus.Failed(_) => f
      case f => sys.error(s"Expected a failed row. Got: $f")
    }

  case class AB[A,B](a: A, b: B)

  def genAB[A, B](ga: Gen[A], gb: Gen[B]): Gen[AB[A,B]] =
    Gen.apply2(AB.apply[A, B])(ga, gb)

  type TestFields2[A, B] = FieldSet2[AB[A, B], A, B]

  def fields2[A,B](empty: (A,B)): TestFields2[A, B] =
    FieldSet2[AB[A,B]](_.a, _.b)(empty)

  implicit def eqCallbackEvent[B] = Equal.equalA[CallbackEvent[B]]

  implicit val eqRowStatus = Equal.equalA[RowStatus]

  val failedRowStatus =
    Failed(Callback.empty)

  def genRowStatus: Gen[RowStatus] =
    Gen.choose(Sync, Locked, failedRowStatus)

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

  implicit def autodomnode(c: CompScope.Mounted[TopNode]) = c.getDOMNode()

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
}