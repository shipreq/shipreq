package shipreq.webapp.feature.uc
package field

import scala.annotation.tailrec
import shipreq.webapp.db._
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.validation.{Validators, VFailure}
import shipreq.webapp.util.AppliedLens
import change._
import step.StepLabels.{MaxStepsPerLevel, MaxStepDepth}
import step.{StepTree, StepNodeBuilder, StepNode}
import step.TreeOps._
import text.StepTextUpdater
import Changes._
import Lenses._
import StepFieldConsts._

object StepFieldConsts {
  def MaxStepViolationMsg = Some(s"That would cause you to have ${MaxStepsPerLevel + 1} steps at the same level, which exceeds the maximum allowed.")
  def MaxStepViolationChangeFailure = ChangeFailure(VFailure.looseMsg(MaxStepViolationMsg.get))
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
      case Some(error) => ChangeFailure(VFailure.looseMsg(error))
      case None        => validResult
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
trait StepFieldLike { this: Field with StepField =>
  override type Value = StepFieldValue

  def defaultLoadValue(h: UseCaseHeader): (Option[StepTree], () => StepFieldValue)

  def rootLabelPrefix(ucn: UseCaseNumber): String

  def sli: StartingLabelIndices

  def prohibitRemoval_?(v: Value, id: LocalStepId): Boolean

  /** If this is true, then title changes will be propagated to the root course text when safe. */
  def preferTitleInRoot_?() = false

  override def toString = s"${getClass.getSimpleName}[#${rec.id.value}]"

  override val changeResponder = new StepFieldValueChangeResponder(this)

  def updateText(id: LocalStepId, newText: String)(u: UseCaseUpdater): UcUpdateResult =
    ChangeResult.fromValidation(Validators.usecase.stepFieldText.correctAndValidate(newText))(t => {
      implicit val lens = AppliedLens(ucStepTextInstL, (u.uc, (this, id)))
      val updater = new StepTextUpdater(this, id)
      val cr = updater.updateCorrected(lens.get, t)(u.ctx)
      u.update(cr)
    })

  def addTailStep(u: UseCaseUpdater): UcUpdateResult = {
    implicit val lens = AppliedLens(ucStepFieldL, (u.uc, this))
    val curNodes = lens.get.tree.nodes
    val labelIndex = sli.startingLabelIndex(0) + curNodes.size
    if (labelIndex > MaxStepsPerLevel)
      MaxStepViolationChangeFailure
    else {
      val tailStep = StepNodeBuilder(0, labelIndex)
      val newSfv = lens.get.withNewStep(StepTree(curNodes :+ tailStep), tailStep.id)
      val cr = newSfv @: TailStepAdded(this, tailStep)
      u.update(cr)
    }
  }

  def addStep(precedingNodeId: LocalStepId)(u: UseCaseUpdater): UcUpdateResult =
    updateTree(u,
      sfv => stepInsert(precedingNodeId, sfv.tree, StepNodeBuilder),
      (sfv, newNodes, newNode) => {
        val newSfv = sfv.withNewStep(StepTree(newNodes), newNode.id)
        newSfv @: StepAdded(this, precedingNodeId, newNode)
      }
    )

  def removeStep(id: LocalStepId)(u: UseCaseUpdater): UcUpdateResult = {
    implicit val lens = AppliedLens(ucStepFieldL, (u.uc, this))
    val sfv = lens.get
    if (prohibitRemoval_?(sfv, id)) NoChange
    else
      stepRemove(id, sfv.tree) match {
        case (newNodes, Some(removedNode)) =>
          var newTextmap = sfv.textmap
          removedNode.foreachRecursive(n => newTextmap -= n.id)
          val newSfv = sfv.copy(tree = StepTree(newNodes), textmap = newTextmap)
          val cr = newSfv @: StepRemoved(this, removedNode)
          u.update(cr)
        case (_, None) => NoChange
      }
  }

  def decreaseIndent(id: LocalStepId)(u: UseCaseUpdater): UcUpdateResult =
    updateTree(u,
      sfv => indentDecrease(id, sfv.tree),
      (sfv, newNodes, tgtNode) => {
        val newSfv = sfv.copy(tree = StepTree(newNodes))
        newSfv @: StepIndentDecreased(this, tgtNode, sfv.tree)
      }
    )

  def increaseIndent(id: LocalStepId)(u: UseCaseUpdater): UcUpdateResult =
    updateTree(u,
      sfv => indentIncrease(id, sfv.tree),
      (sfv, newNodes, tgtNode) => {
        val newSfv = sfv.copy(tree = StepTree(newNodes))
        newSfv @: StepIndentIncreased(this, tgtNode, sfv.tree)
      }
    )

  private def updateTree(u: UseCaseUpdater,
    updateFn: StepFieldValue => (List[StepNode], Option[StepNode]),
    newFn: (StepFieldValue, List[StepNode], StepNode) => Changed[StepFieldValue, Change]
    ): UcUpdateResult = {

    implicit val lens = AppliedLens(ucStepFieldL, (u.uc, this))
    val sfv = lens.get
    updateFn(sfv) match {
      case (newNodes, Some(tgtNode: StepNode)) =>
        resultIfTreeIsValid(newNodes, {
          val cr = newFn(sfv, newNodes, tgtNode)
          u.update(cr)
        })
      case (_, None) => NoChange
    }
  }
}

// =====================================================================================================================

case object NormalCourseFieldDefinition extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.NormalAndAlternateCourses
  override val fieldKeyData = None
  override def field(rec: FieldKeyRec) = NormalCourseField(rec)
}

object NormalCourseFieldConsts {
  val EmptyTree = StepTree(StepNodeBuilder(0, 0, Nil) :: Nil)
  val DefaultTree = StepTree(StepNodeBuilder(0, 0, List(StepNodeBuilder(1, 1))) :: Nil)

  private val addFullStop = "(?<=[a-zA-Z0-9])$".r.pattern
  def titleToMainClause(t: String) = addFullStop.matcher(t).replaceFirst(".")
}

trait NormalCourseFieldLike extends StepFieldLike { this: Field with StepField =>
  import NormalCourseFieldConsts._
  override def defn = NormalCourseFieldDefinition
  override val empty = StepFieldValue.forTree(this, EmptyTree)
  override def rootLabelPrefix(ucn: UseCaseNumber) = s"${ucn.value}."
  override val sli = StartingRootLabelIndexAt0
  override def prohibitRemoval_?(v: Value, id: LocalStepId) = v.tree(0).id == id
  override def preferTitleInRoot_?() = true
  override def defaultLoadValue(h: UseCaseHeader) = {
    val sfv1 = StepFieldValue.forTree(this, DefaultTree)
    val id = sfv1.tree.head.id
    val ncText = titleToMainClause(h.title)
    val sfv = sfvStepTextTextL.set((sfv1, id), (ncText, UcParsingCtx.Empty))
    (Some(sfv.tree), () => sfv)
  }
}

// =====================================================================================================================

case object ExceptionCourseFieldDefinition extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.ExceptionCourses
  override val fieldKeyData = None
  override def field(rec: FieldKeyRec) = ExceptionCourseField(rec)
}

trait ExceptionCourseFieldLike extends StepFieldLike { this: Field with StepField =>
  override def defn = ExceptionCourseFieldDefinition
  override val empty = StepFieldValue.empty(this)
  override def rootLabelPrefix(ucn: UseCaseNumber) = s"${ucn.value}.E."
  override val sli = StartingLabelIndicesAt1
  override def prohibitRemoval_?(v: Value, id: LocalStepId) = false
  override def defaultLoadValue(h: UseCaseHeader) = defaultLoadValue_
  private val defaultLoadValue_ = (None, empty _)
}
