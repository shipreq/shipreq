package com.beardedlogic.usecase
package lib
package field

import net.liftweb.http.js.{ JsCmd, JsCmds, JE }
import net.liftweb.util.Helpers._

import StepTree._
import model.FieldKeyType

object NormalAndAlternateCourseFields extends FieldDef {
  import Fields.Template
  import CourseFields._

  override def newFieldInstance(state: UCEditorState) = new NormalAndAlternateCourseFields(state)

  override def fieldKeyType = FieldKeyType.NormalAndAlternateCourses
  override def fieldKeyData = None

  val NormalCourseTemplate = AddStepTemplate(Template("template-courses-n"))
  val AlternateCourseTemplate = AddStepTemplate(Template("template-courses-a"))
  val AddTailStepCss = s".courses-a .$AddTailStepClass"
}

/**
 * Provides two fields, Normal Course and Alternate Courses, into which the user enters a hierarchy of steps.
 *
 * Steps can be moved between the two.
 */
class NormalAndAlternateCourseFields(val state: UCEditorState) extends CourseFields {
  import NormalAndAlternateCourseFields._
  import CourseFields._

  val ncLabelPrefix = Some(s"$id.")

  // This will do for now but if this is moved into init() it will cause problems with TextFields due to the stepRefMap
  courses =
    StepNode(nextFuncName, 0, ncLabelPrefix, 0, NewStep,
      new StepNode(nextFuncName, 1, 1, NewStep) :: Nil
    ) :: Nil

  override def render = (
    renderSteps(courses.head :: Nil)(NormalCourseTemplate) ++
    renderSteps(courses.tail, AddTailStepCss, newTailStep _)(AlternateCourseTemplate)
  )

  /**
   * Creates a new top-level step to add to the end of the list.
   */
  private def newTailStep() =
    StepNode(nextFuncName, 0, ncLabelPrefix, courses.size, NewStep, Nil)

  /**
   * Prevent removal of the normal course head, ie. 1.0.
   */
  override def prohibitRemoval(id: String) = (id == courses.head.id)

  protected override def customiseIndentDecreaseJs(nodeId: String, updateJs: JsCmd): JsCmd =
    courses match {
      // Move steps from NC to AC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        JsCmds.Run(s"nc_to_ac('#uce','${nodeId}',${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent decrease normally
      case _ => updateJs
    }

  protected override def customiseIndentIncreaseJs(nodeId: String, newNode: StepNode, oldCourses: List[StepNode], updateJs: JsCmd): JsCmd =
    oldCourses match {
      // Move steps from AC to NC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        JsCmds.Run(s"ac_to_nc('#uce','${ExprForNodeAndChildren(newNode)}',${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent normally
      case _ => updateJs
    }
}
