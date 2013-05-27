package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
import StepTree._
import model.{FieldKey, FieldKeyType}

object ExceptionCourseFields extends FieldDef {
  import Fields.Template
  import CourseFields._

  override def newFieldInstance(state: UCEditorState, fieldKey: FieldKey) =
    new ExceptionCourseFields(state, fieldKey)

  override def fieldKeyType = FieldKeyType.ExceptionCourses
  override def fieldKeyData = None

  val ExceptionTemplate = AddStepTemplate(Template("template-courses-e"))
  val AddTailStepCss = s".courses-e .$AddTailStepClass"
}

/**
 * Provides the field Exceptions, into which the user enters a hierarchy of steps.
 */
class ExceptionCourseFields(override val state: UCEditorState, override val fieldKey: FieldKey) extends CourseFields {
  import ExceptionCourseFields._

  val rootLabelPrefix = Some(s"${state.ucNumber}.E.")

  override def labelPrefixForLevel(level: Int) = if (level==0) rootLabelPrefix else None
  override def firstLabelIndexForLevel(level: Int) = 1

  override def render = (
    renderSteps(courses, AddTailStepCss, newTailStep _)(ExceptionTemplate)
  )

  /**
   * Creates a new top-level step to add to the end of the list.
   */
  private def newTailStep() =
    StepNode(nextFuncName, 0, rootLabelPrefix, courses.size + 1, NewStep, Nil)
}
