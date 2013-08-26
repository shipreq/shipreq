package com.beardedlogic.usecase.lib
package field

import scala.annotation.tailrec
import com.beardedlogic.usecase.model._
import StepLabels.{MaxStepsPerLevel, MaxStepDepth}
import tree.TreeOps._
import Types._
import change._
import Changes._
import StepField._

object StepField {
  def MaxStepViolationMsg = Some(s"That would cause you to have ${MaxStepsPerLevel + 1} steps at the same level, which exceeds the maximum allowed.")
  def MaxLevelViolationMsg = Some(s"That would cause your steps to be ${MaxStepDepth + 1} levels deep, which exceeds the maximum allowed.")

  trait StartingLabelIndices {
    def startingLabelIndex(level: Int): Int
  }

  object StartingRootLabelIndexAt0 extends StartingLabelIndices {
    override def startingLabelIndex(level: Int) = if (level == 0) 0 else 1
  }

  object StartingLabelIndicesAt1 extends StartingLabelIndices {
    override def startingLabelIndex(level: Int) = 1
  }

  @inline final def resultIfTreeIsValid(steps: List[StepNode], validResult: => UcUpdateResult): UcUpdateResult =
    validateTree(steps) match {
      case Some(error) => ChangeFailure(error)
      case _ => validResult
    }

  def validateTree(steps: List[StepNode]): Option[String] = {
    @inline def check(l: List[StepNode]): Option[String] = {
      if (l.isEmpty) None
      else if (l.last.labelIndex > MaxStepsPerLevel) MaxStepViolationMsg
      else if (l.head.level >= MaxStepDepth) MaxLevelViolationMsg
      else checkChildren(l)
    }
    @tailrec def checkChildren(nodes: List[StepNode]):Option[String] = nodes match {
      case Nil => None
      case h :: t =>
        val r = check(h.children)
        if (r.isDefined) r else checkChildren(t)
    }
    check(steps)
  }
}

// =====================================================================================================================

/**
 * Abstract field that consists of a tree of structured StepTexts.
 */
abstract class StepField extends Field with StepFieldPersistenceMixin {
  override type Value = StepFieldValue

  def rootLabelPrefix(uch: UseCaseHeader): String

  def sli: StartingLabelIndices

  def prohibitRemoval_?(v: Value, id: LocalStepId): Boolean

  /** If this is true, then title changes will be propagated to the root course text when safe. */
  def preferTitleInRoot_?() = false

  override def toString = s"${getClass.getSimpleName}[#${rec.id}]"

  def updateText(id: LocalStepId, newText: String)(uc: UseCase): UcUpdateResult = {
    implicit val lens = alens(FieldLenses.uc.stepText, (uc, this, id))
    uc.update(this, lens.get.update(newText)(uc.stepsAndLabels))
  }

  def addTailStep(uc: UseCase): UcUpdateResult = {
    implicit val lens = alens(FieldLenses.uc.stepField, (uc, this))
    val curNodes = lens.get.tree.nodes
    val labelIndex = sli.startingLabelIndex(0) + curNodes.size
    if (labelIndex > MaxStepsPerLevel)
      ChangeFailure(MaxStepViolationMsg.get)
    else {
      val tailStep = StepNodeBuilder(0, labelIndex)
      val newSfv = lens.get.withNewStep(StepTree(curNodes :+ tailStep), tailStep.id)
      val cr = newSfv @: TailStepAdded(tailStep)
      uc.update(this, cr)
    }
  }

  def addStep(precedingNodeId: LocalStepId)(uc: UseCase): UcUpdateResult =
    updateTree(uc,
      sfv => stepInsert(precedingNodeId, sfv.tree, StepNodeBuilder),
      (sfv, newNodes, newNode) => {
        val newSfv = sfv.withNewStep(StepTree(newNodes), newNode.id)
        newSfv @: StepAdded(precedingNodeId, newNode)
      }
    )

  def removeStep(id: LocalStepId)(uc: UseCase): UcUpdateResult = {
    implicit val lens = alens(FieldLenses.uc.stepField, (uc, this))
    val sfv = lens.get
    if (prohibitRemoval_?(sfv, id)) NoChange
    else
      stepRemove(id, sfv.tree) match {
        case (newNodes, Some(removedNode)) =>
          var newTextmap = sfv.textmap
          removedNode.foreachRecursive(n => newTextmap -= n.id)
          val newSfv = sfv.copy(tree = StepTree(newNodes), textmap = newTextmap)
          val cr = newSfv @: StepRemoved(removedNode)
          uc.update(this, cr)
        case _ => NoChange
      }
  }

  def decreaseIndent(id: LocalStepId)(uc: UseCase): UcUpdateResult =
    updateTree(uc,
      sfv => indentDecrease(id, sfv.tree),
      (sfv, newNodes, tgtNode) => {
        val newSfv = sfv.copy(tree = StepTree(newNodes))
        newSfv @: StepIndentDecreased(tgtNode, sfv.tree)
      }
    )

  def increaseIndent(id: LocalStepId)(uc: UseCase): UcUpdateResult =
    updateTree(uc,
      sfv => indentIncrease(id, sfv.tree),
      (sfv, newNodes, tgtNode) => {
        val newSfv = sfv.copy(tree = StepTree(newNodes))
        newSfv @: StepIndentIncreased(tgtNode, sfv.tree)
      }
    )

  private def updateTree(uc: UseCase,
    updateFn: StepFieldValue => (List[StepNode], Option[StepNode]),
    newFn: (StepFieldValue, List[StepNode], StepNode) => Changed[StepFieldValue, Change]
    ): UcUpdateResult = {

    implicit val lens = alens(FieldLenses.uc.stepField, (uc, this))
    val sfv = lens.get
    updateFn(sfv) match {
      case (newNodes, Some(tgtNode: StepNode)) =>
        resultIfTreeIsValid(newNodes, {
          val cr = newFn(sfv, newNodes, tgtNode)
          uc.update(this, cr)
        })
      case _ => NoChange
    }
  }
}

// =====================================================================================================================

case object NormalCourseFieldDefinition extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.NormalAndAlternateCourses
  override val fieldKeyData = None
  override def field(rec: FieldKeyRec) = NormalCourseField(rec)
}

object NormalCourseField {
  final val EmptyTree = StepTree(StepNodeBuilder(0, 0, Nil) :: Nil)
  final val DefaultTree = StepTree(StepNodeBuilder(0, 0, List(StepNodeBuilder(1, 1))) :: Nil)
}

case class NormalCourseField(override val rec: FieldKeyRec) extends StepField {
  import NormalCourseField._
  override val defn = NormalCourseFieldDefinition
  override val empty = StepFieldValue.forTree(this, EmptyTree)
  override def rootLabelPrefix(uch: UseCaseHeader) = s"${uch.number}."
  override val sli = StartingRootLabelIndexAt0
  override def prohibitRemoval_?(v: Value, id: LocalStepId) = v.tree(0).id == id
  override def preferTitleInRoot_?() = true
  override val defaultLoadValue = {
    val sfv = StepFieldValue.forTree(this, DefaultTree)
    (Some(sfv.tree), () => sfv)
  }
}

// =====================================================================================================================

case object ExceptionCourseFieldDefinition extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.ExceptionCourses
  override val fieldKeyData = None
  override def field(rec: FieldKeyRec) = ExceptionCourseField(rec)
}

case class ExceptionCourseField(override val rec: FieldKeyRec) extends StepField {
  override val defn = ExceptionCourseFieldDefinition
  override val empty = StepFieldValue.empty(this)
  override def rootLabelPrefix(uch: UseCaseHeader) = s"${uch.number}.E."
  override val sli = StartingLabelIndicesAt1
  override def prohibitRemoval_?(v: Value, id: LocalStepId) = false
  override val defaultLoadValue = (None, empty _)
}
