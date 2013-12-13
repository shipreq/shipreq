package com.beardedlogic.shipreq
package snippet.uce

import java.util.regex.Pattern
import net.liftweb.http.js.JsCmd
import org.scalatest.FunSpec
import org.mockito.Mockito.when
import scala.util.matching.Regex
import xml.NodeSeq

import db.UseCaseRev
import lib.Types._
import feature.uc._
import change.{ChangeConstraint, UseCaseUpdater}
import field.{StepField, ExceptionCourseField, NormalCourseField}
import persist.UseCaseSaveCheckpoint
import step.StepNode
import step.StepLabels.{MaxStepDepth, MaxStepsPerLevel}
import Renderer.TitleId
import test.{CssTestHelpers, TestData, TestHelpers}
import UseCaseEditor._
import UseCaseEditorFns._
import com.beardedlogic.shipreq.lib.Misc
import app.AppConfig

class UseCaseEditorTest extends FunSpec with TestHelpers with TestData with CssTestHelpers {

  def unquoteJs(in: String): String =
    "\\\\u([0-9a-f]{4})".r.replaceAllIn(in, m => Integer.parseInt(m.group(1), 16).toChar.toString).replace("\\\"","\"")

  implicit def js2str(js: JsCmd): String = unquoteJs(js.toJsCmd).trim

  class UseCaseEditor2(
    state: State
    , rels: UseCaseRelations = UseCaseRelations.Empty
    , changeConstraint: Option[ChangeConstraint] = None)
    extends UseCaseEditor(state, rels, changeConstraint) {

    def setState2(newState: State) = { super.setState(newState); this }
    override def update(m: UcModifier): JsCmd = inMockSession {super.update(m)}
    def update2(f: UseCaseUpdater => UcUpdateResult): (UseCaseEditor2, String) = (this, update(UcModifier(f, None, None)))
  }

  def mockRev = {
    val m = mock[UseCaseRev]
    when(m.rev).thenReturn(3: Short)
    m
  }

  def loadedState(uc: UseCase) = State(uc, Some(UseCaseSaveCheckpoint(uc, mockRev, EmptySavedSteps, List.empty)), false)

  lazy val State1 = loadedState(MockUc1.sampleUC)
  lazy val State2a = loadedState(MockUc2a.UC)
  lazy val State2b = loadedState(MockUc2b.UC)
  lazy val State3 = loadedState(MockUc3.UC)

  def UceDemo = new UseCaseEditor2(UseCaseEditorDemo.state, UseCaseEditorDemo.relations, UseCaseEditorDemo.changeConstraint)
  def UCE1 = new UseCaseEditor2(State1)
  def UCE2a = new UseCaseEditor2(State2a)
  def UCE2b = new UseCaseEditor2(State2b)
  def UCE3 = new UseCaseEditor2(State3)

  /**
   * Creates UCE with UC with EC like:
   *
   * 1.E.99
   * 1.E.99.99
   * 1.E.99.99.cu
   */
  def uceWithThreeFullLevelsOfEC = {
    val uce = new UseCaseEditor2(State2a)
    def tree = ECF(uce.fieldValues).tree
    def last = tree.nodes.last
    MaxStepsPerLevel.times(uce.update2(ECF.addTailStep)) // Creates (1-99)
    MaxStepsPerLevel.times(uce.update2(ECF.addStep(last.id))) // Creates (1-99)
    uce.update2(ECF.increaseIndent(last(-1).id)) // Creates 99.98.a
    uce.update2(ECF.addStep(last.id)) // Creates 99.99
    MaxStepsPerLevel.times(uce.update2(ECF.addStep(last(-1)(-1).id))) // Creates 99.98.(a-xx)
    tree.sizeRecursive should be(MaxStepsPerLevel * 3)
    uce
  }

  /**
   * Creates UCE with UC with EC like:
   *
   * 1.E.1
   * 1.E.1.1
   * 1.E.1.1.a
   * 1.E.1.1.a.i
   * 1.E.1.1.a.i.1
   * 1.E.1.1.a.i.2
   */
  def uceWithDeepestLevelEC = {
    val uce = new UseCaseEditor2(State2a)
    def tree = ECF(uce.fieldValues).tree
    var last: StepNode = null
    (MaxStepDepth + 1) times {
      uce.update2(ECF.addTailStep)
      last = tree.nodes.last
      MaxStepDepth times { uce.update2(ECF.increaseIndent(last.id)) }
    }
    (uce,last)
  }

  def assertUpdateFails(uce: UseCaseEditor2, f: UseCaseUpdater => UcUpdateResult): Unit = {
    val oldUC = uce.uc
    val (_,resp) = uce.update2(f)
    assertFailResponse(resp)
    uce.uc should be theSameInstanceAs(oldUC)
  }

  def assertIdAndAction(resp: String, id: AnyLocalId, actionStr: String): Unit =
    assertIdAndActionR(resp, id, Pattern.quote(actionStr).r)

  def assertIdAndActionR(resp: String, id: AnyLocalId, actionRegex: Regex): Unit =
    resp should include regex(s"""$id[^;\n]+?$actionRegex""")

  def assertIdRelabeled(resp: String, id: AnyLocalId, newLabel: String) {
    assertIdAndActionR(resp, s"$id-l".tag[IsLocalId], s"""['"]$newLabel['"]""".r)
  }

  def assertNewStepFound(resp: String) {
    resp should include("class=\"step\"")
    resp should include("<textarea")
  }

  def assertFailResponse(resp: String): Unit = resp should include("alert(")

  def render(uce: UseCaseEditor) = {
    lazy val xml = inMockSession(uce.dispatch("render")(Renderer.Templates.EntirePage.get))
    lazy val html = xml.toString
    (xml, html)
  }

  describe("Full-page rendering") {
    lazy val (_, html) = render(UCE1)

    it("should render the title") {
      html should include(MockUc1.sampleUC.title)
    }

    it("should render text fields") {
      html should include(TF1.defn.title)
      html should include(TF2.defn.title)
      html should include(">blah<") // TF1
      html should include(">hehe<") // TF3
    }


    it("should render NC field steps") {
      html should include("Normal")
      html should include("Alternative")
      html should include("I'm the title")
      html should include("Finally")
      html should include("7.0")
    }

    it("should render EC field steps") {
      html should include("Exceptions")
      html should include("EC-1E1")
      html should include("EC-1E1-1")
      html should include("EC-1E2")
      html should include("7.E.1")
    }

    it("should render the demo page") {
      val (_, html) = render(UceDemo)
      html should include("[UC-2: Request Refund]")
      html should include("data-lvl=\"2\"")
    }
  }

  describe("setState()") {
    it("should use the given state") {
      val uce = UCE1
      uce.state should be theSameInstanceAs (State1)
    }
  }

  describe("AJAX callbacks") {

    describe("title change") {
      lazy val (uce,resp) = new UseCaseEditor2(State1).update2(_.updateTitle("  bananas  "))
      it("should update the editor state") {
        uce.uc.title should be("bananas")
      }
      it("should set the title via ajax") {
        assertIdAndActionR(resp, TitleId.tag[IsLocalId], """['"]bananas['"]""".r)
      }

      it("should restore the original value after receiving an inconsequential change") {
        val uce = new UseCaseEditor2(State1)
        val resp: String = uce.update(uce.renderer.modTitle(" YES!"))
        resp should (include("\"YES") and (not(include(" YES"))))
        uce.state should be theSameInstanceAs (State1)
      }
    }

    describe("text field change") {
      lazy val (uce,resp) = new UseCaseEditor2(State1).update2(TF1.updateText("bananas"))
      it("should update the editor state") {
        TF1(uce.fieldValues).text should be("bananas")
      }
      it("should set the field value via ajax") {
        resp should include("bananas")
        resp should include(uce.textFieldIds(TF1))
      }

      it("should restore the original value after receiving an inconsequential change") {
        val uce = new UseCaseEditor2(State1)
        val resp: String = uce.update(uce.renderer.modTextField(TF1)(" blah"))
        resp should (include("\"blah") and (not(include(" blah"))))
        uce.state should be theSameInstanceAs (State1)
      }
    }

    describe("step field change") {
      lazy val (uce,resp) = UCE2b.update2(NCF.updateText(X1, "bananas --> 7.0.1"))
      it("should update the editor state") {
        NCF(uce.fieldValues).textmap(X1).text should startWith("bananas")
      }
      it("should set the field value via ajax") {
        resp should include("bananas ➡ [7.0.1]")
        resp should include(X1)
      }
      it("should update affected steps via ajax") {
        resp should include("⬅ [7.0]")
        resp should include(X3)
      }
      it("should not update unaffected steps via ajax") {
        resp should not include(X2)
      }
      it("should be able to affect the text of empty steps") {
        import MockUc1._
        val (uce,resp) = UCE1.update2(NCF.updateText(NcSfv.tree(0).id, "bananas --> 7.0.2"))
        resp should include("⬅ [7.0]")
        resp should include(NcSfv.tree(0)(1).id)
      }

      it("should restore the original value after receiving an inconsequential change") {
        val uce = new UseCaseEditor2(State2a)
        val resp: String = uce.update(uce.renderer.stepRenderers(NCF).modText(X2)(" blar"))
        resp should (include("\"blar") and (not(include(" blar"))))
        uce.state should be theSameInstanceAs (State2a)
      }
    }

    def testMaxSteps(uce: UseCaseEditor2, addFn: => UseCaseUpdater => UcUpdateResult, atMax: UseCase => Boolean) {
      var i = 0
      while (!atMax(uce.uc) && i < 110) {
        i += 1
        val oldUc = uce.uc
        val (_, resp) = uce.update2(addFn)
        if (uce.uc eq oldUc) fail(s"AddFn didn't add. (attempt: $i)\nResponse: $resp\n${uce.uc}")
      }
      val oldUc = uce.uc
      val (n, resp) = uce.update2(addFn)
      assertFailResponse(resp)
      uce.uc should be theSameInstanceAs (oldUc)
    }

    def itRespectsMaxStepsPossible(name: String, uceFn: => () => UseCaseEditor2, addFn: => UseCaseUpdater => UcUpdateResult, labelAtMax: String) = {
      it(s"should not exceed the max-steps limit ($name)") {
        testMaxSteps(uceFn(), addFn, _.stepsAndLabels.value.ba.contains(labelAtMax.asLabel))
      }
    }

    def itRespectsMaxStepConstraint(name: String, testNCF: Boolean, addFn: StepField => (UseCaseUpdater => UcUpdateResult)) = {
      it(s"should not exceed a step limit imposed by check-constraints ($name)") {
        val uce = UceDemo
        val fl = uce.uc.fields
        val ncf = Misc.filterCovar[NormalCourseField](fl).head
        val ecf = Misc.filterCovar[ExceptionCourseField](fl).head
        val addField = if (testNCF) ncf else ecf
        val sizeFn = (uc: UseCase) => ncf(uc).tree.sizeRecursive + ecf(uc).tree.sizeRecursive
        testMaxSteps(uce, addFn(addField), sizeFn(_) == AppConfig.DemoUseCaseMaxSteps)
      }
    }

    describe("adding tail step") {
      def assertTailStepAdded(resp: String, lbl: String, coursesCss: String) {
        resp should include(s">$lbl<")
        resp should include(s"$coursesCss .addTailStep")
        assertNewStepFound(resp)
      }
      it("should add 7.2 for NC") {
        val (uce,resp) = UCE1.update2(NCF.addTailStep)
        stepTreeLens.get(uce.uc, NCF).nodes.size should be(3)
        assertTailStepAdded(resp, "7.2", ".courses-a")
      }
      it("should add 7.E.3 for NC") {
        val (uce,resp) = UCE1.update2(ECF.addTailStep)
        stepTreeLens.get(uce.uc, ECF).nodes.size should be(3)
        assertTailStepAdded(resp, "7.E.3", ".courses-e")
      }
      it("should not allow more steps than the max") {
        val (uce,resp) = UCE1.update2(ECF.addTailStep)
        stepTreeLens.get(uce.uc, ECF).nodes.size should be(3)
        assertTailStepAdded(resp, "7.E.3", ".courses-e")
      }
      it should behave like(itRespectsMaxStepsPossible("NC", UCE1 _, NCF.addTailStep, "7.99"))
      it should behave like(itRespectsMaxStepsPossible("EC", UCE1 _, ECF.addTailStep, "7.E.99"))
      it should behave like(itRespectsMaxStepConstraint("NC", true, f => f.addTailStep))
      it should behave like(itRespectsMaxStepConstraint("EC", false, f => f.addTailStep))
    }

    describe("adding a step") {
      lazy val (uce,resp) = UCE3.update2(NCF.addStep(X3))
      it("should update the state") {
        val ch = stepTreeLens.get(uce.uc, NCF)(0).children
        ch.size should be(4)
        ch.last.id should be(X5)
      }
      it("should push the new step to the client") {
        resp should include(X3)
        assertNewStepFound(resp)
      }
      it("should relabel proceeding steps") {
        assertIdRelabeled(resp, X2, "3") // 7.0.3
        assertIdRelabeled(resp, X5, "4") // 7.0.4
      }
      it("should update referencing text field text") {
        assertIdAndAction(resp, uce.textFieldIds(TF1), "[7.0.3]")
      }
      it("should update referencing step field text") {
        assertIdAndAction(resp, X1, "root [7.0.4]")
      }
      it should behave like(itRespectsMaxStepsPossible("NC", UCE2b _, NCF.addStep(X3), "7.0.99"))
      it should behave like(itRespectsMaxStepsPossible("EC", UCE1 _, ECF.addStep(MockUc1.EcSfv.tree(1).id), "7.E.2.99"))
      it should behave like(itRespectsMaxStepConstraint("NC", true, f => f.addStep("F898146860208051SD4".tag)))
      it should behave like(itRespectsMaxStepConstraint("EC", false, f => f.addStep("F898146860506XT0ZUD".tag)))
    }

    describe("remove a step") {
      lazy val (uce,resp) = UCE3.update2(NCF.removeStep(X2))
      it("should update the state") {
        stepTreeLens.get(uce.uc, NCF).sizeRecursive should be(3)
      }
      it("should remove the step and its children via ajax") {
        assertIdAndAction(resp, X2, "remove")
        assertIdAndAction(resp, X4, "remove")
      }
      it("should relabel proceeding steps") {
        assertIdRelabeled(resp, X5, "2") // X5 -> 7.0.2
      }
      it("should update referencing text field text") {
        assertIdAndAction(resp, uce.textFieldIds(TF1), "DELETED")
      }
      it("should update referencing step field text") {
        assertIdAndAction(resp, X1, "root [7.0.2]")
      }
    }

    describe("indenting a step") {
      lazy val (uce,resp) = UCE3.update2(NCF.increaseIndent(X5))
      it("should update the state"){
        uce.uc.stepsAndLabels.value.ab(X5) should be("7.0.2.b")
      }
      it("should update the client"){
        assertIdAndAction(resp, X5, "attr")
        assertIdRelabeled(resp, X5, "b")
      }
      it("should not transition from AC to NC when node is 7.0.3") {
        resp should not include("ac_to_nc")
      }
      it("should transition from AC to NC when node is 7.1") {
        val (_,resp) = UCE1.update2(NCF.increaseIndent(MockUc1.NcSfv.tree(1).id))
        resp should include("ac_to_nc")
      }
      it("should fail when it causes a breach of max steps per level") {
        val uce = uceWithThreeFullLevelsOfEC
        def tree = ECF(uce.fieldValues).tree
        uce.update2(ECF.removeStep(tree(3).id))
        uce.update2(ECF.addTailStep)
        assertUpdateFails(uce, ECF.increaseIndent(tree.nodes.last.id)) // L0 --> L1
      }
      it("should fail when increasing the deepest level allowed") {
        val (uce, last) = uceWithDeepestLevelEC
        uce.update2(ECF.addStep(last.id))
        val n = deepestLast(ECF(uce.uc.fieldValues).tree.nodes.last)
        assertUpdateFails(uce, ECF.increaseIndent(n.id))
      }
      it("should fail when increasing a step which has children at the lowest level") {
        val (uce, _) = uceWithDeepestLevelEC
        def tree = ECF(uce.fieldValues).tree
        uce.update2(ECF.addStep(tree(0).id)) // add 1.E.1.1
        val n = tree(0)(0)
        uce.update2(ECF.decreaseIndent(n.id)) // dec 1.E.1.1 into 1.E.2
        assertUpdateFails(uce, ECF.increaseIndent(n.id)) // inc 1.E.2 with its new children
      }
    }

    describe("decreasing a step indent") {
      lazy val (uce,resp) = UCE3.update2(NCF.decreaseIndent(X5))
      it("should update the state"){
        uce.uc.stepsAndLabels.value.ab(X5) should be("7.1")
      }
      it("should update the client"){
        assertIdAndAction(resp, X5, "attr")
        assertIdRelabeled(resp, X5, "7.1")
      }
      it("should transition from NC to AC when creating 7.1") {
        resp should include("nc_to_ac")
      }
      it("should not transition from NC to AC when creating 7.0.3") {
        val (_,resp) = UCE3.update2(NCF.decreaseIndent(X4))
        resp should not include("nc_to_ac")
        assertIdAndAction(resp, X4, "attr")
      }
      it("should not transition from NC to AC when creating 7.E.2") {
        val id = MockUc1.EcSfv.tree(0)(0).id
        val (_,resp) = UCE1.update2(ECF.decreaseIndent(id))
        resp should not include("nc_to_ac")
        assertIdAndAction(resp, id, "attr")
      }
      it("should fail when it causes a breach of max steps per level") {
        val uce = uceWithThreeFullLevelsOfEC
        def last = ECF(uce.fieldValues).tree.nodes.last
        assertUpdateFails(uce, ECF.decreaseIndent(last(0).id)) // L0 <-- L1
        assertUpdateFails(uce, ECF.decreaseIndent(last(-1)(0).id)) // L1 <-- L2
      }
    }
  }

  describe("The Save button") {

    describe("when page first rendered") {
      def saveButtonO(xml: NodeSeq) = findCssO(xml, "#save")
      def saveButton(xml: NodeSeq) = findCss(xml, "#save")

      it("should be removed when UC is anonymous") {
        lazy val (xml, html) = render(UceDemo)
        saveButtonO(xml) ==== None
      }

      it("should be disabled when UC is loaded") {
        lazy val (xml, html) = render(UCE2b)
        saveButton(xml).toString.toLowerCase should include ("disabled")
      }
    }

    describe("after an AJAX event") {
      def assertSaveButtonDisabled(js: String) = js should include ("attr('disabled")
      def assertSaveButtonEnabled(js: String) = js should include ("removeAttr('disabled")

      it("should be disabled after a save") {
        assertSaveButtonDisabled(UCE2b.jsPostSave)
        assertSaveButtonDisabled(UCE2b.jsPostSaveNop)
      }

      it("should be disabled only after changes which make it differ from last save") {
        val uce = new UseCaseEditor2(State1)

        val resp1 = uce.update(UcModifier(TF1.updateText("bananas"), None, None))
        uce.state.saveEnabled ==== true
        assertSaveButtonEnabled(resp1)

        val resp2 = uce.update(UcModifier(TF1.updateText("blah"), None, None))
        uce.state.saveEnabled ==== false
        assertSaveButtonDisabled(resp2)
      }
    }
  }
}
