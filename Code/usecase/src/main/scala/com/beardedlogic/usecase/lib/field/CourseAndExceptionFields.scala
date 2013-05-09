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

object CourseAndExceptionFields extends FieldDef {
  import Fields.Template

  def newFieldInstance = new CourseAndExceptionFields

  val StepTemplate = Template("template-step")
  val AddStepTemplate = ".steps * " #> StepTemplate

  val NormalCourseId = "courses-n"
  val AlternateCourseId = "courses-a"
  val ExceptionCourseId = "courses-e"

  val NormalCourseTemplate = AddStepTemplate(Template(NormalCourseId))
  val AlternateCourseTemplate = AddStepTemplate(Template(AlternateCourseId))
  val ExceptionTemplate = AddStepTemplate(Template(ExceptionCourseId))

  val AddFirstStepTemplate = Template("template-courses-addFirstStep")

  val AttrLevel = "data-lvl"
  val AddFirstStepClass = "addFirstStep"
}

class CourseAndExceptionFields extends Field {
  import CourseAndExceptionFields._

  val id = 1
  val ncLabelPrefix = Some(id + ".")
  var courses: List[StepNode] =
    StepNode(nextFuncName, 0, ncLabelPrefix, 0, NewStep,
      new StepNode(nextFuncName, 1, 1, NewStep) :: Nil
    ) :: Nil

  var coursesEmpty__tmp: List[StepNode] = Nil

  def render = (
    renderSteps(courses)(NormalCourseTemplate) ++
    renderSteps(coursesEmpty__tmp, onFirstAddAlternateCourse _)(AlternateCourseTemplate) ++
    renderSteps(coursesEmpty__tmp)(ExceptionTemplate)
  )

  private def renderSteps(steps: List[StepNode], addFirstStepFn: () => JsCmd = null) =
    if (steps.isEmpty && addFirstStepFn != null) (
      ".step" #> AddFirstStepTemplate andThen
      ".step [class!]" #> "step" &
      "button" #> SHtml.ajaxButton("+", addFirstStepFn)
    )
    else (
      ".step" #> flattenNodes(steps).map(renderStep)
    )

  private def renderStep(n: StepNode) = (
    ".step [id]" #> n.id
    & s".step [$AttrLevel]" #> n.level
    & ".label span *" #> n.label
    & ".label span [id]" #> n.labelId
    & "@text" #> SHtml.textarea(n.step.text, (_) => (), "rows" -> "1", "id" -> n.stepTextId)
    & ".add" #> SHtml.ajaxButton("+", () => onStepAdd(n.id))
    & ".delete" #> SHtml.ajaxButton("-", () => onStepRemove(n.id))
    & ".indentDec" #> SHtml.ajaxButton("«", () => onIndentDecrease(n.id))
    & ".indentInc" #> SHtml.ajaxButton("»", () => onIndentIncrease(n.id))
  )

  @inline private def renderStepXml(n: StepNode) = {
    val fn = ".step" #> renderStep(n)
    fn(StepTemplate)
  }

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
        JqExpr(s"#${AlternateCourseId} .${AddFirstStepClass}") ~> JqBefore(renderStepXml(newNode))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
      )
    } else JsCmds.Noop

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def onStepAdd(preceedingNodeId: String): JsCmd = stepInsert(NewStep, preceedingNodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      courses = newCourses
      (
        JqId(preceedingNodeId) ~> JqAfter(renderStepXml(newNode))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
        & UpdateLabels(flattenNodes(courses))
      )
    case _ => JsCmds.Noop
  }

  /**
   * Removes a new step and all its children, shuffling up following steps and renumbering if necessary.
   */
  def onStepRemove(id: String): JsCmd = stepRemove(id, courses) match {
    case (newCourses, true) =>
      courses = newCourses
      FadeOut(JqId(id), 240)(
        _ ~> JqJE.JqRemove()
          & UpdateLabels(flattenNodes(courses))
      )
    case _ => JsCmds.Noop
  }

  /**
   * Decreases the indentation level of a given step.
   */
  def onIndentDecrease(nodeId: String): JsCmd = indentDecrease(nodeId, courses) match {
    case (newCourses, Some(_)) =>
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      val updateJs = UpdateIndentation(flattenedCourses) & UpdateLabels(flattenedCourses)

      newCourses match {
        // Move steps from NC to AC
        case nc :: ac1 :: acN if ac1.id == nodeId =>
          JsCmds.Run(s"nc_to_ac('${nodeId}', ${JE.AnonFunc(updateJs).toJsCmd})")

        // Apply indent decrease normally
        case _ => updateJs
      }

    case _ => JsCmds.Noop
  }

  /**
   * Increases the indentation level of a given step.
   */
  def onIndentIncrease(nodeId: String): JsCmd = indentIncrease(nodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      val oldCourses = courses
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      val updateJs = UpdateIndentation(flattenedCourses) & UpdateLabels(flattenedCourses)

      oldCourses match {
        // Move steps from AC to NC
        case nc :: ac1 :: acN if ac1.id == nodeId =>
          val movedToNc = newNode :: flattenNodes(newNode.children)
          val ids = movedToNc.map("#" + _.id).mkString(",")
          JsCmds.Run(s"ac_to_nc('${ids}', ${JE.AnonFunc(updateJs).toJsCmd})")

        // Apply indent normally
        case _ => updateJs
      }

    case _ => JsCmds.Noop
  }

  /**
   * Creates Javascript to update the indentation levels of all given nodes.
   */
  private def UpdateIndentation(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JqId(n.id) ~> JqJE.JqAttr(AttrLevel, n.level.toString) toJsCmd
    )) mkString ";\n"
  )

  /**
   * Creates Javascript to update the label text of all given nodes.
   */
  private def UpdateLabels(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JsCmds.SetHtml(n.labelId, Text(n.label)).toJsCmd
    )) mkString "\n"
  )
}
