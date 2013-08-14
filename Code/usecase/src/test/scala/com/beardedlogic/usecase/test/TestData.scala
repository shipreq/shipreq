package com.beardedlogic.usecase
package test

import lib._
import Types._
import UseCaseFns._
import field._
import test.NodeUtils._
import text.{StepText, FreeText}

trait TestData extends TestHelpers {

  def lens = FieldLenses.uc

  // TODO Create TestData MockUc1
  // TODO Move TestData into TestHelpers?

  lazy val FL: List[Field] = List(TF1, TF2, NCF, ECF, TF3)
  lazy val EmptyFieldValues: FieldValues = FL.map(f => (f ~> f.empty)).toMap
  lazy val UCH = UseCaseHeader("YES!", 7)
  lazy val EmptyUC = UseCase(UCH, FL, EmptyFieldValues, EmptyStepAndLabelBiMap)
  lazy val EmptyUcWithoutNCF = removeNcField(EmptyUC)

  def ucWithValues(values: (Any, Any)*): UseCase = {
    val fvs = EmptyFieldValues ++ Map(values: _*).asInstanceOf[FieldValues]
    UseCase(UCH, FL, fvs, generateStepAndLabelBiMap(fvs, UCH))
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
      X1 -> StepText(X1, freeText("I'm the root"), None, None), // 7.0
      X2 -> StepText(X2, freeText("blar"), None, None), // 7.0.1
      X4 -> StepText(X4, freeText("deeper"), None, None) // 7.0.1.a
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText("Linking to [7.0.1]", Map(X2 -> "7.0.1".asLabel))
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }

  // Inserted a step at 7.0.1, changing 7.0.1 to 7.0.2, updating TF1 text.
  object MockUc2b {
    lazy val NcStepTree = StepTree(StepNode(X1, 0, 0, List(StepNode(X3, 1, 1, Nil), StepNode(X2, 1, 2, StepNode(X4, 2, 1, Nil) :: Nil))) :: Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(X1, freeText("I'm the root"), None, None), // 7.0
      X3 -> StepText(X3, freeText("I was inserted"), None, None), // 7.0.1
      X2 -> StepText(X2, freeText("blar"), None, None), // 7.0.2
      X4 -> StepText(X4, freeText("deeper"), None, None) // 7.0.2.a
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText("Linking to [7.0.2]", Map(X2 -> "7.0.2".asLabel))
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }

  object MockUc3 {
    lazy val X1sChildren =
      StepNode(X3, 1, 1, Nil) ::
        StepNode(X2, 1, 2, StepNode(X4, 2, 1, Nil) :: Nil) ::
        StepNode(X5, 1, 3, Nil) :: Nil
    lazy val NcStepTree = StepTree(StepNode(X1, 0, 0, X1sChildren) :: Nil)
    lazy val NcStepText = Map(
      X1 -> StepText(X1, FreeText("I'm the root [7.0.3]", Map(X5 -> "7.0.3".asLabel)), None, None), // 7.0
      X3 -> StepText(X3, freeText("I was inserted"), None, None), // 7.0.1
      X2 -> StepText(X2, freeText("blar"), None, None), // 7.0.2
      X4 -> StepText(X4, freeText("deeper"), None, None), // 7.0.2.a
      X5 -> StepText(X5, freeText("last"), None, None) // 7.0.3
    )
    lazy val NcSfv = StepFieldValue(NCF, NcStepTree, NcStepText)
    lazy val TFV1 = FreeText("Linking to [7.0.2]", Map(X2 -> "7.0.2".asLabel))
    lazy val UC = ucWithValues(TF1 -> TFV1, NCF -> NcSfv)
  }
}

/**
 * Uses real fields with real DB FK entries.
 */
trait LoadedTestData extends TestData {
  lazy val FLRec = Defaults.FieldList.get
  override lazy val FL = FLRec.fields
  lazy val TxtFields = filter[TextField](FL)
  lazy override val NCF = filter[NormalCourseField](FL).head
  lazy override val ECF = filter[ExceptionCourseField](FL).head
  lazy override val TF1 = TxtFields(0)
  lazy override val TF2 = TxtFields(1)
  lazy override val TF3 = TxtFields(2)
  lazy override val TF4 = TxtFields(3)
}