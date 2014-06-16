package shipreq.webapp
package test

import lib.Misc._
import lib.Types._
import db.UseCaseHeader
import feature.uc._
import feature.uc.field._
import feature.uc.text.{FlowFrom, FlowTo, StepText, FreeText}
import feature.uc.text.FreeTextTerms._
import feature.uc.step.{StepNode, StepTree}
import persist.FieldLoadCtx
import test.NodeUtils._
import app.Defaults
import UseCaseFns._

trait TestData extends TestHelpers2 {

  // TODO Create TestData MockUc1
  // TODO Move TestData into TestHelpers?

  lazy val FL: List[Field] = List(TF1, TF2, NCF, ECF, TF3, FGF)
  lazy val EmptyFieldValues: FieldValues = FL.map(f => (f ~> f.empty)).toMap
  lazy val UCN = UseCaseNumber(7)
  lazy val UCH = UseCaseHeader("YES!")
  lazy val EmptyLoadCtx = FieldLoadCtx(UCH, List.empty)
  lazy val EmptyUC = UseCase(UCN, UCH, FL, EmptyFieldValues, StepAndLabelBiMap.empty)
  lazy val EmptyUcWithoutNCF = removeNcField(EmptyUC)

  def ucWithValues(values: (Any, Any)*): UseCase = {
    val fvs = EmptyFieldValues ++ Map(values: _*).asInstanceOf[FieldValues]
    UseCase(UCN, UCH, FL, fvs, generateStepAndLabelBiMap(UCN, fvs))
  }

  object MockUc1 {
    lazy val sampleTextOnlyUC = ucWithValues(
      TF1 -> freeText("blah"),
      TF3 -> freeText("hehe")
    )

    lazy val NcSteps = parseStepTree( """
        7.0. I'm the title
          1. First
          2. _
          3. Finally
        7.1. Sweet """)
    lazy val EcSteps = parseStepTree( """
        7.E.1. EC-1E1
          1. EC-1E1-1
        7.E.2. EC-1E2 """)
    lazy val NcSfv = NcSteps.toStepFieldValue(NCF)
    lazy val EcSfv = EcSteps.toStepFieldValue(ECF)
    lazy val sampleUC = ucWithValues(
      TF1 -> freeText("blah"),
      TF3 -> freeText("hehe"),
      NCF -> NcSfv,
      ECF -> EcSfv
    )
  }

  object MockUc2a {
    lazy val NcStepTree = StepTree(StepNode(X1, 0, 0, List(StepNode(X2, 1, 1, StepNode(X4, 2, 1, Nil) :: Nil))) :: Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(freeText("I'm the root"), None, None), // 7.0
      X2 -> StepText(freeText("blar"), None, None), // 7.0.1
      X4 -> StepText(freeText("deeper"), None, None) // 7.0.1.a
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText(PlainText("Linking to ") :: StepRef(X2, StepLabel("7.0.1")) :: Nil)
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }

  // Inserted a step at 7.0.1, changing 7.0.1 to 7.0.2, updating TF1 text.
  object MockUc2b {
    lazy val NcStepTree = StepTree(StepNode(X1, 0, 0, List(StepNode(X3, 1, 1, Nil), StepNode(X2, 1, 2, StepNode(X4, 2, 1, Nil) :: Nil))) :: Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(freeText("I'm the root"), None, None), // 7.0
      X3 -> StepText(freeText("I was inserted"), None, None), // 7.0.1
      X2 -> StepText(freeText("blar"), None, None), // 7.0.2
      X4 -> StepText(freeText("deeper"), None, None) // 7.0.2.a
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText(PlainText("Linking to ") :: StepRef(X2, StepLabel("7.0.2")) :: Nil)
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }

  object MockUc3 {
    lazy val X1sChildren =
      StepNode(X3, 1, 1, Nil) ::
        StepNode(X2, 1, 2, StepNode(X4, 2, 1, Nil) :: Nil) ::
        StepNode(X5, 1, 3, Nil) :: Nil
    lazy val NcStepTree = StepTree(StepNode(X1, 0, 0, X1sChildren) :: Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(FreeText(PlainText("I'm the root ") :: StepRef(X5, StepLabel("7.0.3")) :: Nil), None, None), // 7.0
      X3 -> StepText(freeText("I was inserted"), None, None), // 7.0.1
      X2 -> StepText(freeText("blar"), None, None), // 7.0.2
      X4 -> StepText(freeText("deeper"), None, None), // 7.0.2.a
      X5 -> StepText(freeText("last"), None, None) // 7.0.3
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText(PlainText("Linking to ") :: StepRef(X2, StepLabel("7.0.2")) :: Nil)
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }

  object MockUc4 {
    implicit def autoLabel(x: String) = StepLabel(x)

    lazy val X1sChildren =
      StepNode(X3, 1, 1, Nil) ::
      StepNode(X2, 1, 2, StepNode(X4, 2, 1, Nil) :: Nil) ::
      StepNode(X5, 1, 3, Nil) :: Nil
    lazy val NcStepTree = StepTree(
      StepNode(X1, 0, 0, X1sChildren) ::
      StepNode(X6, 0, 1, Nil) ::
      StepNode(X7, 0, 2, StepNode(X8, 1, 1, Nil) :: Nil) ::
      Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(FreeText(PlainText("I'm the root ") :: StepRef(X5, "7.0.3"):: Nil), None, None), // 7.0
      X3 -> StepText(freeText("I was inserted"), FlowFrom.create(Map(X4 -> "7.0.2.a")), None), // 7.0.1 <- 2a
      X2 -> StepText(freeText("blar"), None, None), // 7.0.2
      X4 -> StepText(freeText("deeper"), None, FlowTo.create(Map(X3 -> "7.0.1", X6 -> "7.1"))), // 7.0.2.a
      X5 -> StepText(freeText("last"), None, None), // 7.0.3
      X6 -> StepText(freeText("AC 1"), FlowFrom.create(Map(X4 -> "7.0.2.a")), None), // 7.1
      X7 -> StepText(freeText("AC 2"), None, None), // 7.2
      X8 -> StepText(freeText("AC 2.1"), None, FlowTo.create(Map(X5 -> "7.0.3"))) // 7.2.1
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)

    lazy val TFV1 = FreeText(PlainText("Linking to ") :: StepRef(X2, "7.0.2") :: Nil)
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }
}

/**
 * Uses real fields with real DB FK entries.
 */
trait LoadedTestData extends TestData {
  lazy val FLRec = Defaults.fieldList.value
  override lazy val FL = FLRec.fields
  lazy val TxtFields = filterCovar[TextField](FL)
  lazy override val NCF = filterCovar[NormalCourseField](FL).head
  lazy override val ECF = filterCovar[ExceptionCourseField](FL).head
  lazy override val TF1 = TxtFields(0)
  lazy override val TF2 = TxtFields(1)
  lazy override val TF3 = TxtFields(2)
  lazy override val TF4 = TxtFields(3)
}