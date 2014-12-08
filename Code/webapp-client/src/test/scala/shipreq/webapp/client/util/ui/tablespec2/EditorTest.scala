package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.test._
import org.scalajs.dom.HTMLInputElement
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.Scalaz.Id
import utest._
import shipreq.base.util.ScalaExt._
import Editors._
import TestUtil._, SampleData_Person._

object EditorTest extends TestSuite {

  /*
  CallbackH
  - pmodB
  - paddST

  EditorI
  - modCallbackH

  Editor
  - modCallbacksA
  - modCallbacks
  - pmodB
  - paddST
  - zoomU

  Editors
  - text{Input,Area}
  - checkbox
  */

  type TestEditor[B, M[_], S, C] = Editor[CallbackH[B, M, S, C], B, M, S, C, CallbackH[B, M, S, C], CallbackH[B, M, S, C]]
  type TestEditorI[B, M[_], S, C] = EditorI[CallbackH[B, M, S, C], B, M, S, C, CallbackH[B, M, S, C]]

  type SimpleTestEditor[B] = TestEditor[B, Id, Unit, Unit]
  type SimpleTestEditorI[B] = TestEditorI[B, Id, Unit, Unit]

  def TestEditorI[B, M[_], S, C](a: CallbackH[B, M, S, C]): TestEditorI[B, M, S, C] =
    EditorI(a, "", Some(identity[CallbackH[B, M, S, C]]))

  def TestEditor[B, M[_], S, C]: TestEditor[B, M, S, C] =
    Editor(i => { val cbh = i.data; i.editable.fold(cbh)(_(cbh)) })

  def SimpleTestEditorI[B](b: CallbackEvent[B]): SimpleTestEditorI[B] =
    TestEditorI(CallbackH(b, ReactS.ret(()), ()))

  def SimpleTestEditor[B]: SimpleTestEditor[B] =
    TestEditor[B, Id, Unit, Unit]

  def testSimpleEditor[B: Equal](e: SimpleTestEditor[B])(i: CallbackEvent[B], expect: CallbackEvent[B]): Unit = {
    val o = e.render(SimpleTestEditorI(i))
    val actual = o.event
    assertEq(actual, expect)
  }

  def testSimpleEditorB[B: Equal](e: SimpleTestEditor[B])(b: B, onChange: B, onEditFinished: B): Unit = {
    testSimpleEditor(e)(OnChange(b), OnChange(onChange))
    testSimpleEditor(e)(OnEditFinished(b), OnEditFinished(onEditFinished))
  }

  def testUpdateRevert[P,B,N<:TopNode](c: ReactComponentM[P, NewAndSavedRowState, B, N]): Unit = {
    import NewAndSavedRowState._
//    val c = ReactTestUtils.renderIntoDocument(Component(props))

    def test(tgtsel: Sel, revertable: Boolean, teste: String => Unit, testn: => Unit): Unit = {
      val tgt = tgtsel.findIn(c).domType[HTMLInputElement]
      val expect4 = savedRowStoreS.getI(4)(c.state)

      def test1(expect: String): Unit = {
        val nodeValue = tgt.getDOMNode().value
        assertEq(nodeValue, expect)
        teste(expect)
        val state4 = savedRowStoreS.getI(4)(c.state)
        assertEq(state4, expect4)
        testn
      }

      (Simulation.focus >> ChangeEventData("yo").simulation) run tgt
      test1("yo")

      ChangeEventData("yoy") simulate tgt
      test1("yoy")

      Simulation.blur run tgt
      test1("yoy")

      (Simulation.focus >> KeyboardEventData(key = "Escape").simulationKeyDown) run tgt
      test1(if (revertable) "mike" else "yoy")
    }

    def testSavedUpdateAndRevert(): Unit = {
      val expectN = newRowStoreS.getI(c.state)
      test(Sel(".id-7 .username"), true, expect => {
        val state = savedRowStoreS.getI(7)(c.state)
        assertEq(state, (expect, ""))
      }, {
        val stateN = newRowStoreS.getI(c.state)
        assertEq(stateN, expectN)
      })
    }

    def testNewUpdateAndRevert(): Unit =
      test(Sel(".new .username"), false, expect => {
        val state = newRowStoreS.getI(c.state)
        assertEq(state, (expect, "TODO").some)
      }, ())

    // !∃ new | test saved
    assert(!newRowStoreS.editing(c.state))
    testSavedUpdateAndRevert()

    // ∃ new | test saved
    c.runState(
      ReactS.mod(newRowStoreS.enableEdit) >> ReactS.mod(newRowStoreS.setField(fields.f1 * "omg"))
    ).unsafePerformIO()
    assert(newRowStoreS.editing(c.state))
    testSavedUpdateAndRevert()

    // test new | saved
    testNewUpdateAndRevert()
  }

  override def tests = TestSuite {

    'applyLiveCorrection {
      val e = SimpleTestEditor[String].applyLiveCorrection(usernameV)
      testSimpleEditorB(e)("HeHe ", onChange = "hehe ", onEditFinished = "HeHe ")
    }

    'applyPostCorrection {
      val e = SimpleTestEditor[String].applyPostCorrection(usernameVU.cp)(_ => ())
      testSimpleEditorB(e)("hehe ", onChange = "hehe ", onEditFinished = "hehe")
    }

    'applyPostCorrectionU {
      val e = SimpleTestEditor[String].applyPostCorrectionU(usernameVU.cp)
      testSimpleEditorB(e)("hehe ", onChange = "hehe ", onEditFinished = "hehe")
    }

    'applyInputValidation {
      val e = textInputEditor.applyInputValidationU(usernameVU)
      def test(i: String, expect: Option[String]): Unit = {
        val re: ReactElement = e.render(EditorI(i, "", None))
        val tgt = ReactTestUtils.renderIntoDocument(re.asInstanceOf[ReactComponentU_]) // TODO fix react
        val actual = Sel(".errorMsg").findInO(tgt).map(_.getDOMNode().innerHTML)
        assertEq(actual, expect)
      }
      test("Start!ed", "Username can only contain letters, numbers and underscores.".some)
      test("Happy", None)
    }

    'applyRowUpdateAndRevert {
      import NewAndSavedRowState._
      val props = NewAndSavedRowState.Props(fieldValidation = false, updateRevert = true)
      val c = ReactTestUtils.renderIntoDocument(Component(props))
      testUpdateRevert(c)
    }

    'combos {
      * -{
        val e = SimpleTestEditor[String].applyLiveCorrection(usernameV).applyPostCorrectionU(usernameVU.cp)
        testSimpleEditorB(e)("HeHe ", onChange = "hehe ", onEditFinished = "hehe")
      }
      * -{
        import NewAndSavedRowState._
        val props = NewAndSavedRowState.Props(fieldValidation = true, updateRevert = true)
        val c = ReactTestUtils.renderIntoDocument(Component(props))
        testUpdateRevert(c)
        val u4 = Sel(".id-4 .username").findIn(c).domType[HTMLInputElement].getDOMNode()
        (Simulation.focus >> ChangeEventData("  HeHe").simulation) run u4
        assertEq("Live correction", u4.value, "  hehe")
        Simulation.blur run u4
        assertEq("Post correction", u4.value, "hehe")
      }
    }
  }
}
