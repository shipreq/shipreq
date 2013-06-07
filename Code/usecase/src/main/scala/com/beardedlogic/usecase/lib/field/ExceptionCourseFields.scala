package com.beardedlogic.usecase
package lib
package field

import model.{FieldKey, FieldKeyType}
import CourseFields._

object ExceptionCourseFields extends FieldDef[CourseFieldState] {
  import Fields.Template

  override def newFieldInstance(ucCtx: UseCaseCtx, fieldKey: FieldKey) =
    new ExceptionCourseFields(ucCtx, fieldKey)

  override def fieldKeyType = FieldKeyType.ExceptionCourses
  override def fieldKeyData = None

  def EC_StartingLabelIndices = StartingLabelIndicesAt1
  override def stateLoader(fieldKey: FieldKey) = new CourseFieldStateLoader(fieldKey, EC_StartingLabelIndices)

  val ExceptionTemplate = AddStepTemplate(Template("template-courses-e"))
  val AddTailStepCss = s".courses-e .$AddTailStepClass"
}

/**
 * Provides the field Exceptions, into which the user enters a hierarchy of steps.
 */
class ExceptionCourseFields(override val ucCtx: UseCaseCtx, override val fieldKey: FieldKey) extends CourseFields {
  import ExceptionCourseFields._

  override def recalcRootLabelPrefix = Some(s"${ucCtx.number}.E.")
  override def startingLabelIndices = EC_StartingLabelIndices

  override def render = renderStepsWithAddTailStep(courses)(ExceptionTemplate)

  override protected def buildNewTailStep() = StepNodeBuilder(0, courses.size + 1)

  override def tailStepCss = AddTailStepCss
}
