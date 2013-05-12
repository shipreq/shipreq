package com.beardedlogic.usecase.lib
package field

import net.liftweb.http.SHtml
import net.liftweb.http.js.{ JsCmd, JsCmds, JE }
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.util.Helpers._
import scala.annotation.tailrec
import scala.xml._

import JsExt._
import StepTree._

object ExceptionCourseFields extends FieldDef {
  import Fields.Template
  import CourseFields._

  override def newFieldInstance(state: UCEditorState) = new ExceptionCourseFields(state)

  val ExceptionCourseId = "courses-e"
  val ExceptionTemplate = AddStepTemplate(Template(ExceptionCourseId))
  val AddTailStepCss = s"#${ExceptionCourseId} .${AddTailStepClass}"
}

/**
 * Provides the field Exceptions, into which the user enters a hierarchy of steps.
 */
class ExceptionCourseFields(val state: UCEditorState) extends CourseFields {
  import ExceptionCourseFields._
  import CourseFields._

  val ncLabelPrefix = Some(id + ".E.")

  override def render = (
    renderSteps(courses, AddTailStepCss, newTailStep _)(ExceptionTemplate)
  )

  /**
   * Creates a new top-level step to add to the end of the list.
   */
  private def newTailStep() =
    StepNode(nextFuncName, 0, ncLabelPrefix, courses.size + 1, NewStep, Nil)
}
