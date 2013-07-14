package com.beardedlogic.usecase
package lib
package field

import tree._
import model._
import CourseFields._
import TypeTags._

class CourseFieldStateLoader(val fieldKey: FieldKey, val li: StartingLabelIndices) extends FieldStateLoader[CourseFieldState] {

  override def load(loadCtx: FieldLoadCtx, saveCtx: MutableFieldSaveCtx) = {
    val stepStates = (
                       for {
                         fv <- loadCtx.fieldValues.get(fieldKey.taggedId)
                         has <- loadCtx.relations.get(RelationType.Has)
                       } yield unpackSteps(fv.valueId, 0, has, loadCtx.stepData, saveCtx)
                       ).getOrElse(List.empty[StepState])
    CourseFieldState(StepStateTree(stepStates))
  }

  /**
   * When loading, turns data from the `FieldLoadCtx` into a tree of `StepNode`s.
   *
   * @param parentId The value ID of this level's step parent.
   */
  private def unpackSteps(
    parentId: Long,
    level: Int,
    relations: Map[Long, List[Long]],
    stepData: Map[Long_StepValueId, (PlainValue[DataType.Step], String)],
    saveCtx: MutableFieldSaveCtx): List[StepState] = {

    relations.get(parentId).map { ids =>
      var labelIndex = li.startingLabelIndex(level)
      ids.map { stepValueId =>
        val (stepValue, text) = stepData(stepValueId.tag[StepValueId])
        val children = unpackSteps(stepValueId, level + 1, relations, stepData, saveCtx)
        val localStepId = s"s$stepValueId".asLocalId
        val ss = StepState(localStepId, text.hasNormalisedRefs, children)
        saveCtx.stepValues += (localStepId -> stepValue)
        labelIndex += 1
        ss
      }
    }.getOrElse(List.empty[StepState])
  }
}

// TODO CourseFieldState redundant now that StepStateTree exists
case class CourseFieldState(courses: StepStateTree) {
  val stepMap: Map[LocalIdStr, StepState] = courses.mapRecursive(ss => (ss.id -> ss)).toMap
}

case class StepState(
  id: LocalIdStr,
  text: String @@ NormalisedRefs,
  children: List[StepState])
  extends TreeNodeLike[StepState] {

  // Manually specify else it will recurse forever because this is Traversable
  override def toString = s"StepState($id, '$text', $children)"
}

case class StepStateTree(override val nodes: List[StepState]) extends TreeRoot[StepState]