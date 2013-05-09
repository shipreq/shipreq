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

object NormalAndAlternateCourseFields extends FieldDef {
  import Fields.Template
  import CourseFields._

  override def newFieldInstance = new NormalAndAlternateCourseFields

  val NormalCourseId = "courses-n"
  val AlternateCourseId = "courses-a"

  val NormalCourseTemplate = AddStepTemplate(Template(NormalCourseId))
  val AlternateCourseTemplate = AddStepTemplate(Template(AlternateCourseId))
}

/**
 * Provides two fields, Normal Course and Alternate Courses, into which the user enters a hierarchy of steps.
 *
 * Steps can be moved between the two.
 */
class NormalAndAlternateCourseFields extends CourseFields {
  import NormalAndAlternateCourseFields._
  import CourseFields._

  val ncLabelPrefix = Some(id + ".")
  var courses: List[StepNode] =
    StepNode(nextFuncName, 0, ncLabelPrefix, 0, NewStep,
      new StepNode(nextFuncName, 1, 1, NewStep) :: Nil
    ) :: Nil

  def render = (
    renderSteps(courses.head :: Nil)(NormalCourseTemplate) ++
    renderSteps(courses.tail, onFirstAddAlternateCourse _)(AlternateCourseTemplate)
  )

  /**
   * Adds the first alternate course. (ie. X.1.)
   *
   * The button that invokes this will only be visible (via a CSS rule) when the alternate course section is empty.
   */
  def onFirstAddAlternateCourse(): JsCmd =
    if (courses.size == 1) {
      val newNode = StepNode(nextFuncName, 0, ncLabelPrefix, 1, NewStep, Nil)
      courses = courses :+ newNode
      (
        JqExpr(s"#${AlternateCourseId} .${AddFirstStepClass}") ~> JqBefore(renderSingleStepXml(newNode))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
      )
    } else JsCmds.Noop

  protected override def customiseIndentDecreaseJs(nodeId: String, updateJs: JsCmd): JsCmd =
    courses match {
      // Move steps from NC to AC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        JsCmds.Run(s"nc_to_ac('${nodeId}', ${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent decrease normally
      case _ => updateJs
    }

  protected override def customiseIndentIncreaseJs(nodeId: String, newNode: StepNode, oldCourses: List[StepNode], updateJs: JsCmd): JsCmd =
    oldCourses match {
      // Move steps from AC to NC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        val movedToNc = newNode :: flattenNodes(newNode.children)
        val ids = movedToNc.map("#" + _.id).mkString(",")
        JsCmds.Run(s"ac_to_nc('${ids}', ${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent normally
      case _ => updateJs
    }
}
