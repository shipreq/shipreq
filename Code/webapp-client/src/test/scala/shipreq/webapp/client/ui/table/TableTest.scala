package shipreq.webapp.client.ui
package table

import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import japgolly.scalajs.react.test.{Simulation, ReactTestUtils}
import org.scalajs.dom
import scalaz.effect.IO
import scalaz.std.string.stringInstance
import scalaz.Equal
import utest._
import shipreq.webapp.shared.validation.{GenericValidators, ValidatorPlus}
import shipreq.webapp.client.protocol.FailureIO
import ValidatorPlus.Implicits._

object TableTest extends TestSuite {

  case class Data(name: String, desc: Option[String]) //, num: Int)
  implicit def DataEqual = Equal.equalA[Data]

  val nameV = ValidatorPlus.nop[String]
  val descV = GenericValidators.optionalLargeText("desc").toPlus

  val prespec = TableSpecBuilder[Data](
    FieldSpec[Data](_.name)(nameV)(Editors.TextInputEditor),
    FieldSpec[Data](_.desc)(descV)(Editors.TextareaEditor)
  ).buildU(Data).dataId[Int]

  private val prespec2 = prespec
      .tableConstraints(None, None)
      .saveNotNeededWhenP

  class Tester {
    var fs = List.empty[FailureIO]
  }

  def assertRowValues[D, P, A](spec: TableSpec[_, _, D, _, P, _, _])(f: (RowStatus, P) => A) =
    (c: spec.CSF) => new {
      def apply(m: (D, A)*): Unit = {
        val actual = spec.getSaved(c).map { case (r, d, p) => d -> f(r, p)}.toMap
        val expect = m.toMap
        assert(actual == expect)
      }
    }

  override def tests = TestSuite {

    'asyncSaveFailure {

      type X = Tester
      def save(x: X, o: Option[(Int, Data)], u: Data, f: FailureIO) = IO[Unit] {
        x.fs ::= f
      }
      val spec = prespec2.asyncSave(save)

      val refs = Ref.param[Int, TopNode](_.toString)

      val C = ReactComponentB[(X, Map[Int, Data])]("C")
        .getInitialState(p => spec.initialState(p._2))
        .render(T => {
          implicit def x = T.props._1
          val savedRow = spec.savedRow((_, d, _, vv) => {
            val (name, desc) = vv
            div(keyAttr := d, ref := refs(d), name, desc)
          })
          val savedRows = spec.savedRows(T, savedRow)(_.sortBy(_._3.name))
          div(savedRows)
        }).build

      val t = new Tester
      val data = Map(2 -> Data("ABC", None), 3 -> Data("DEF", Some("YAG")))
      val c = ReactTestUtils renderIntoDocument C((t, data))

      val r2 = refs(2)(c).get
      val i2 = ReactTestUtils.findRenderedDOMComponentWithTag(r2, "input").domType[dom.HTMLInputElement]

      val r3 = refs(3)(c).get
      val i3 = ReactTestUtils.findRenderedDOMComponentWithTag(r3, "input").domType[dom.HTMLInputElement]

      val initialState = c.state

      def assertRowStatuses = assertRowValues(spec)((r, p) => r)(c)

      def setup(): Unit = {
        c setState initialState
        t.fs = Nil
        Simulation.focusChangeBlur("blar2") run i2
        assertRowStatuses(2 -> RowStatus.Locked, 3 -> RowStatus.Sync)
        Simulation.focusChangeBlur("blar3") run i3
        assertRowStatuses(2 -> RowStatus.Locked, 3 -> RowStatus.Locked)
      }

      'inOrder {
        setup()
        val List(f3, f2) = t.fs
        f2.io.unsafePerformIO()
        assertRowStatuses(2 -> RowStatus.Failed, 3 -> RowStatus.Locked)
        f3.io.unsafePerformIO()
        assertRowStatuses(2 -> RowStatus.Failed, 3 -> RowStatus.Failed)
      }

      'outOfOrder{
        setup()
        val List(f3, f2) = t.fs
        f3.io.unsafePerformIO()
        assertRowStatuses(2 -> RowStatus.Locked, 3 -> RowStatus.Failed)
        f2.io.unsafePerformIO()
        assertRowStatuses(2 -> RowStatus.Failed, 3 -> RowStatus.Failed)
      }

    }
  }
}