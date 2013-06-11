package com.beardedlogic.usecase
package lib
package field

import net.liftweb.http.js.{ JsCmd, JsCmds, JE }
import TypeTags._
import model.{FieldKey, FieldKeyType}
import CourseFields._
import msg.NoReactionOrNewMessages

object NormalAndAlternateCourseFields extends FieldDef[CourseFieldState] {
  import TemplateCache._

  override def newFieldInstance(ucCtx: UseCaseCtx, fieldKey: FieldKey) =
    new NormalAndAlternateCourseFields(ucCtx, fieldKey)

  override def fieldKeyType = FieldKeyType.NormalAndAlternateCourses
  override def fieldKeyData = None

  def NCAC_StartingLabelIndices = StartingRootLabelIndexAt0
  override def stateLoader(fieldKey: FieldKey) = new CourseFieldStateLoader(fieldKey, NCAC_StartingLabelIndices)

  val NormalCourseTemplate = AddStepTemplate(UseCaseEditorTemplate.extract("template-courses-n"))
  val AlternateCourseTemplate = AddStepTemplate(UseCaseEditorTemplate.extract("template-courses-a"))
  val AddTailStepCss = s".courses-a .$AddTailStepClass"
}

/**
 * Provides two fields, Normal Course and Alternate Courses, into which the user enters a hierarchy of steps.
 *
 * Steps can be moved between the two.
 */
class NormalAndAlternateCourseFields(override val ucCtx: UseCaseCtx, override val fieldKey: FieldKey) extends CourseFields {
  import NormalAndAlternateCourseFields._

  // TODO This will do for now but if this is moved into init() it will cause problems with TextFields due to the stepRefMap
  setCourses(
    StepNodeBuilder(0, 0, List(StepNodeBuilder(1, 1))) :: Nil
  )(NoReactionOrNewMessages)

  override def recalcRootLabelPrefix = Some(s"${ucCtx.number}.")
  override def startingLabelIndices = NCAC_StartingLabelIndices

  override def render = (
    renderSteps(courses.head :: Nil)(NormalCourseTemplate) ++
    renderStepsWithAddTailStep(courses.tail)(AlternateCourseTemplate)
  )

  override protected def buildNewTailStep() = StepNodeBuilder(0, courses.size)

  override def tailStepCss = AddTailStepCss

  /**
   * Prevent removal of the normal course head, ie. 1.0.
   */
  override def prohibitRemoval_?(id: String @@ LocalId) = (id == courses.head.id)

  protected override def customiseIndentDecreaseJs(nodeId: String @@ LocalId, updateJs: JsCmd): JsCmd =
    courses match {
      // Move steps from NC to AC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        JsCmds.Run(s"nc_to_ac('#uce','${nodeId}',${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent decrease normally
      case _ => updateJs
    }

  protected override def customiseIndentIncreaseJs(nodeId: String @@ LocalId, newNode: StepNode, oldCourses: List[StepNode], updateJs: JsCmd): JsCmd =
    oldCourses match {
      // Move steps from AC to NC
      case nc :: ac1 :: acN if ac1.id == nodeId =>
        JsCmds.Run(s"ac_to_nc('#uce','${ExprForNodeAndChildren(newNode)}',${JE.AnonFunc(updateJs).toJsCmd})")

      // Apply indent normally
      case _ => updateJs
    }
}
