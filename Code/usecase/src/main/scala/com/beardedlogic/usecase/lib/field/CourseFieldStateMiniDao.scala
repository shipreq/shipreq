package com.beardedlogic.usecase
package lib
package field

import StepTree._
import model._
import FieldValue.FieldValueData
import CourseFields.CourseFieldState

/**
 * Loads and saves courses.
 */
class CourseFieldStateMiniDao(val field: CourseFields) extends FieldStateMiniDao[CourseFieldState] {

  override def load(ctx: FieldLoadCtx) =
    (
      for {
        fv <- ctx.fieldValues.get(field.fieldKey.valueId)
        has <- ctx.relations.get(RelationType.Has)
      } yield unpackSteps(fv.valueId, 0, has, ctx.stepData)
    )
    .getOrElse(List.empty[StepNode])

  /**
   * When loading, turns data from the `FieldLoadCtx` into a tree of `StepNode`s.
   *
   * @param parentId The value ID of this level's step parent.
   */
  private def unpackSteps(parentId: Long, level: Int, relations: Map[Long, List[Long]], stepData: Map[Long, String]): List[StepNode] = {
    relations.get(parentId)
    .map { ids =>
      var labelIndex = field.firstLabelIndexForLevel(level)
      ids.map { id =>
        val children = unpackSteps(id, level + 1, relations, stepData)
        val step = Step(stepData.getOrElse(id, ""))
        val sn = new StepNode(s"v$id", level, labelIndex, step, children)
        labelIndex += 1
        sn
      }
    }.getOrElse(List.empty[StepNode])
  }

  override def save_?(state: CourseFieldState): Boolean = state.nonEmpty

  override def presave(state: CourseFieldState, ctx: FieldSaveCtx) {
    for (n <- flattenNodes(state)) {
      val value = ctx.db.createInitialValue(DataType.Step)
      ctx.stepValues += (n.id -> value)
    }
  }

  override def save(state: CourseFieldState, ctx: FieldSaveCtx): FieldValueData = {
    saveNodes(state, ctx, ctx.fieldValues(field), 0)
    None
  }

  private def saveNodes(courses: List[StepNode], ctx: FieldSaveCtx, parent: Value[_ <: StepParent], index: Int): Unit = courses match {
    case h :: t =>
      // TODO references, same as text fields
      val value = ctx.stepValues(h.id)
      ctx.db.createStep(value, h.step.text)
      ctx.db.relate_stepParent_has_step(parent, index.toShort, value)

      saveNodes(h.children, ctx, value, 0)
      saveNodes(t, ctx, parent, index + 1)

    case _ =>
  }
}