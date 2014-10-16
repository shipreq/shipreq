package shipreq.webapp.client.ui
package table

import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import japgolly.scalajs.react.test._
import org.scalajs.dom
import scalaz.effect.IO
import scalaz.std.string.stringInstance
import scalaz.Equal
import utest._
import shipreq.base.util.Debug._
import shipreq.webapp.shared.validation.{GenericValidators, ValidatorPlus}
import shipreq.webapp.client.protocol.FailureIO
import ValidatorPlus.Implicits._
import TableTestUtils._

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

  class AsyncTester {
    def reset(): Unit = { ss = Nil; fs = Nil; upds = Nil }
    var ss = List.empty[SuccessIO]
    var fs = List.empty[FailureIO]
    var upds = List.empty[(Option[(Int, Data)], Data)]
  }

  type Arb = AsyncTester
  def save(x: Arb, o: Option[(Int, Data)], u: Data, s: SuccessIO, f: FailureIO) = IO[Unit] {
    x.ss ::= s
    x.fs ::= f
    x.upds ::= (o,u)
  }
  val spec = prespec2.asyncSave(save)

  val refs = Ref.param[Int, TopNode](_.toString)

  val newRowS = spec.unsavedInitS(("nem","desk"))

  val C = ReactComponentB[(Arb, Map[Int, Data])]("C")
    .getInitialState(p => spec.initialState(p._2))
    .render(T => {
      implicit def x = T.props._1
      val newRow = spec.unsavedRow((F, rs, vv) => {
        val (name, desc) = vv
        div(ref := refs(-1), name, desc)
      })
      val savedRow = spec.savedRow((_, d, _, vv) => {
          val (name, desc) = vv
          div(keyAttr := d, ref := refs(d), name, desc)
        })
      val savedRows = spec.savedRows(T, savedRow)(_.sortBy(_.p.name))
      div(
        button(cls := "new", onclick ~~> T.runState(newRowS), disabled := spec.unsavedRowExists(T), "New"),
        newRow(T),
        savedRows)
    }).build

  val data = Map(2 -> Data("ABC", None), 3 -> Data("DEF", Some("YAG")))
  val t = new AsyncTester
  val c = ReactTestUtils renderIntoDocument C((t, data))
  val ta = TableAssertions(spec, c)
  import ta._

  def nameRef(i: Int) = Sel("input").find(refs(i)(c)).domType[dom.HTMLInputElement]

  val simChange = Simulation.focusChangeBlur("x")

  override def tests = TestSuite {
    ta.resetState()
    t.reset()

    'asyncSaveFailure {
      val List(i2, i3) = List(2, 3) map nameRef
      simChange run i2; assertRowStatuses(2 -> Locked, 3 -> Sync)
      simChange run i3; assertRowStatuses(2 -> Locked, 3 -> Locked)

      'inOrder {
        val List(f3, f2) = t.fs
        f2.io.unsafePerformIO(); assertRowStatuses(2 -> Failed, 3 -> Locked)
        f3.io.unsafePerformIO(); assertRowStatuses(2 -> Failed, 3 -> Failed)
      }

      'outOfOrder{
        val List(f3, f2) = t.fs
        f3.io.unsafePerformIO(); assertRowStatuses(2 -> Locked, 3 -> Failed)
        f2.io.unsafePerformIO(); assertRowStatuses(2 -> Failed, 3 -> Failed)
      }
    }

    'newItem {
      Simulation.click run (Sel("button.new") find c)
      assertUnsavedRowStatus(Some(Sync))
      simChange run nameRef(-1)
      assertMatch(t.upds){case List((None, _))=>}
      assertUnsavedRowStatus(Some(Locked))
      t.ss.head.io.unsafePerformIO()
      assertUnsavedRowStatus(None)
    }
  }
}