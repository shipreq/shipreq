package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.test._
import org.scalajs.dom.raw.HTMLInputElement
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.Scalaz.Id
import utest._
import shipreq.base.util.ScalaExt._
import Editors._
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.client.test.SampleDataPerson._
import NewAndSavedRowState._

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
        assertEq(state, (expect, "TO"+"DO").some)
      }, ())

    // !∃ new | test saved
    assert(!newRowStoreS.editing(c.state))
    testSavedUpdateAndRevert()

    // ∃ new | test saved
    c.runState(
      ReactS.mod(newRowStoreS.enableEdit) >> ReactS.mod(newRowStoreS.setField(fields.f1 * "omg"))
    ).runNow()
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
        val tgt = ReactTestUtils.renderIntoDocument(re)
        val actual = Sel(".errorMsg").findInO(tgt).map(_.getDOMNode().innerHTML)
        assertEq(actual, expect)
      }
      test("Start!ed", "Username can only contain letters, numbers and underscores.".some)
      test("Happy", None)
    }

    'applyRowUpdateAndRevert {
      val props = Props(fieldValidation = false, updateRevert = true, saveIO = None)
      val c = ReactTestUtils.renderIntoDocument(Component(props))
      testUpdateRevert(c)
    }

    'weirdKeyDownIssue {
      val props = Props(fieldValidation = false, updateRevert = true, saveIO = None)
      val c = ReactTestUtils.renderIntoDocument(Component(props))
      val tgt = Sel(".id-7 .username").findIn(c).domType[HTMLInputElement].getDOMNode()
      def test(prefix: String, expect: String): Unit = {
        assertEq(s"$prefix.input", tgt.value, expect)
        assertEq(s"$prefix.state", savedRowStoreS.getI(7)(c.state)._1, expect)
      }
      (Simulation.focus >> ChangeEventData("abc").simulation) run tgt
      test("pre", "abc")
      KeyboardEventData(key = "F1").simulationKeyDown run tgt
      test("post", "abc")
    }

    'combos {
      'liveAndPostCorrection {
        val e = SimpleTestEditor[String].applyLiveCorrection(usernameV).applyPostCorrectionU(usernameVU.cp)
        testSimpleEditorB(e)("HeHe ", onChange = "hehe ", onEditFinished = "hehe")
      }

      'validationAndUpdateRevert {
        val props = Props(fieldValidation = true, updateRevert = true, saveIO = None)
        val c = ReactTestUtils.renderIntoDocument(Component(props))
        testUpdateRevert(c)
        val u4 = Sel(".id-4 .username").findIn(c).domType[HTMLInputElement].getDOMNode()
        (Simulation.focus >> ChangeEventData("  HeHe").simulation) run u4
        assertEq("Live correction", u4.value, "  hehe")
        Simulation.blur run u4
        assertEq("Post correction", u4.value, "hehe")
      }

      'allWithAsyncCreateAndUpdate {
        var saves = Vector.empty[SaveI]
        val s: SaveIO = i => Callback(saves :+= i)

        def testRetry(retry: Callback): Unit = {
          retry.runNow()
          assert(saves.length == 2)
          assertEq(saves(0).p, saves(1).p)
          assertEq(saves(0).u, saves(1).u)
        }

        val props = Props(fieldValidation = true, updateRevert = true, saveIO = s.some)
        val c = ReactTestUtils.renderIntoDocument(Component(props))

        'saved {
          val tgt = Sel(".id-7 .username").findIn(c).domType[HTMLInputElement].getDOMNode()

          def assertNoSave(): Unit = {
            assert(saves.isEmpty)
            assertEq(savedRowStoreS.getStatus(7)(c.state), RowStatus.Sync)
          }

          def assertSave(): Unit = {
            assert(saves.length == 1)
            assertEq(savedRowStoreS.getStatus(7)(c.state), RowStatus.Locked)
            assertEq(saves(0).p, person7.some)
          }

          'validationFails {
            Simulation.focusChangeBlur(" X@  ") run tgt
            assertNoSave()
            // Confirm post-correction still applied
            assertEq(tgt.value, "x@")
            assertEq(savedRowStoreS.getI(7)(c.state)._1, "x@")
          }

          'noNeedToSave {
            Simulation.focusChangeBlur(" mike ") run tgt
            assertNoSave()
          }

          'save {
            Simulation.focusChangeBlur("right") run tgt
            assertSave()
            assertEq(saves(0).u._1, Username("right"))
          }

          'rpcSuccess {
            Simulation.focusChangeBlur("blahblah") run tgt
            assertSave()
            saves(0).s.runNow()
            assertEq(savedRowStoreS.getStatus(7)(c.state), RowStatus.Locked) // success = nop
          }

          'rpcFailure {
            Simulation.focusChangeBlur("blahblah") run tgt
            assertSave()
            saves(0).f.runNow()
            val retry = assertRowStatusFailed(savedRowStoreS.getStatus(7)(c.state)).retry
            testRetry(retry)
          }
        }

        'new {
          c.modState(newRowStoreS.enableEdit).runNow()
          val tgt = Sel(".new .username").findIn(c).domType[HTMLInputElement].getDOMNode()

          def assertNoSave(): Unit = {
            assert(saves.isEmpty)
            assertEq(newRowStoreS.getStatus(c.state), RowStatus.Sync.some)
          }

          def assertSave(): Unit = {
            assert(saves.length == 1)
            assertEq(newRowStoreS.getStatus(c.state), RowStatus.Locked.some)
          }

          'validationFails {
            Simulation.focusChangeBlur("  X@  ") run tgt
            assertNoSave()
            // Confirm post-correction still applied
            assertEq(tgt.value, "x@")
            assertEq(newRowStoreS.getI(c.state).map(_._1), "x@".some)
          }

          'save {
            Simulation.focusChangeBlur("blahblah") run tgt
            assertSave()
            assertEq(saves(0).u._1, Username("blahblah"))
          }

          'rpcSuccess {
            Simulation.focusChangeBlur("blahblah") run tgt
            assertSave()
            saves(0).s.runNow()
            assertEq(newRowStoreS.getStatus(c.state), None)
          }

          'rpcFailure {
            Simulation.focusChangeBlur("blahblah") run tgt
            assertSave()
            saves(0).f.runNow()
            val retry = assertRowStatusFailed(newRowStoreS.getStatus(c.state).get).retry
            testRetry(retry)
          }
        }
      }
    }
  }
}
