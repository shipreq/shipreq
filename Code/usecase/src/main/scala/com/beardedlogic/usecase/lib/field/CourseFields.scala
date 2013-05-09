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

object CourseFields {
  import Fields.Template

  val StepTemplate = Template("template-step")

  val AttrLevel = "data-lvl" // TODO rename this unclear thing

  val AddStepTemplate = ".steps * " #> StepTemplate
  val AddFirstStepTemplate = Template("template-courses-addFirstStep")
  val AddFirstStepClass = "addFirstStep"
}

abstract class CourseFields extends Field {
  import CourseFields._

  val id = 1
  var courses: List[StepNode]

  /**
   * Renders a list of steps and their trees of children.
   *
   * @param addFirstStepFn If non-null, then a button will be provided in the absence of any steps that allows the user
   * to create the first step.
   */
  protected def renderSteps(steps: List[StepNode], addFirstStepFn: () => JsCmd = null) = {
    val renderedSteps = ".step" #> flattenNodes(steps).map(renderSingleStep)
    if (addFirstStepFn == null) {
      renderedSteps
    } else {
      val t = "button" #> SHtml.ajaxButton("+", addFirstStepFn)
      val addFirstStep = t(AddFirstStepTemplate)
      renderedSteps andThen ".steps *+" #> addFirstStep // Append to .steps, after each .step
    }
  }

  /**
   * Provides the transformation to render a single step. (Does not render step children.)
   */
  protected def renderSingleStep(n: StepNode) = (
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

  /**
   * Renders a single step into XML. (Does not render step children.)
   */
  @inline protected def renderSingleStepXml(n: StepNode) = {
    val fn = ".step" #> renderSingleStep(n)
    fn(StepTemplate)
  }

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def onStepAdd(preceedingNodeId: String): JsCmd = stepInsert(NewStep, preceedingNodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      courses = newCourses
      (
        JqId(preceedingNodeId) ~> JqAfter(renderSingleStepXml(newNode))
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
        _ ~> JqJE.JqRemove() & UpdateLabels(flattenNodes(courses))
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
  protected def UpdateIndentation(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JqId(n.id) ~> JqJE.JqAttr(AttrLevel, n.level.toString) toJsCmd
    )) mkString ";\n"
  )

  /**
   * Creates Javascript to update the label text of all given nodes.
   */
  protected def UpdateLabels(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JsCmds.SetHtml(n.labelId, Text(n.label)).toJsCmd
    )) mkString "\n"
  )
}
