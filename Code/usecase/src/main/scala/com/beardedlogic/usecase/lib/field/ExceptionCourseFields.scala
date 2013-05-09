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

  override def newFieldInstance = new ExceptionCourseFields

  val ExceptionCourseId = "courses-e"
  val ExceptionTemplate = AddStepTemplate(Template(ExceptionCourseId))
}

/**
 * Provides the field Exceptions, into which the user enters a hierarchy of steps.
 */
class ExceptionCourseFields extends CourseFields {
  import ExceptionCourseFields._
  import CourseFields._

  val ncLabelPrefix = Some(id + ".E.")
  var courses: List[StepNode] = Nil

  def render = (
    renderSteps(courses, onFirstAdd _)(ExceptionTemplate)
  )

  /**
   * Adds the first alternate course. (ie. X.1.)
   *
   * The button that invokes this will only be visible (via a CSS rule) when the alternate course section is empty.
   */
  def onFirstAdd(): JsCmd = ???
}
