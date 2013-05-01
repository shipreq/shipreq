package com.beardedlogic.usecase
package snippet

import lib.JsExt.{JqAfter, JqHide, JqId, JqSlideDownFast}
import net.liftweb.http.{StatefulSnippet, Templates}
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml.ElemAttr.pairToBasic
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.{jsExpToJsCmd, seqJsToJs}
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}
import scala.xml.Text

/**
 * @since 29/04/13
 */
object UCEditor {

  case class Step(text: String)

  case class StepNode(id: String,
                      level: Int,
                      label: String,
                      step: Step,
                      children: List[StepNode]) {
    def labelId = id + "-l"
    def stepTextId = id + "-t"
  }

  def NewStep = Step("")

  val StepTemplate = {
    val ExtractStepTemplate = ".step ^^" #> ""
    val index = Templates("index" :: Nil).open_!
    ExtractStepTemplate(ClearClearable(index))
  }

  /**
   * Flattens a list of step nodes with children, into a single list that contains all recursive contents.
   */
  @tailrec def flattenNodes(nodes: List[StepNode], results: List[StepNode] = Nil): List[StepNode] = nodes match {
    case Nil    => results
    case h :: t => flattenNodes(h.children ::: t, results :+ h)
  }

  def incrementPosition(n: StepNode) = {
    // TODO pos hack
    val posHack = (n.label.toInt + 1).toString
    n.copy(label = posHack)
  }

  @tailrec def incrementPosition(nodes: List[StepNode], results: List[StepNode] = Nil): List[StepNode] = nodes match {
    case h :: t => incrementPosition(t, results :+ incrementPosition(h))
    case Nil    => results
  }

  def insertStep(
    step: Step,
    afterId: String,
    nodes: List[StepNode],
    results: List[StepNode] = Nil,
    resultNode: Option[StepNode] = None): Tuple2[List[StepNode], Option[StepNode]] = nodes match {

    case Nil => (results, resultNode)

    case h :: t if h.id == afterId && h.level == 0 =>
      val n = StepNode(nextFuncName, h.level + 1, "1", step, Nil)
      val c = n :: incrementPosition(h.children)
      (results ::: h.copy(children = c) :: t, Some(n))

    case h :: t if h.id == afterId =>
      val n = StepNode(nextFuncName, h.level, h.label, step, Nil)
      (results ::: h :: incrementPosition(n :: t), Some(n))

    case h :: t =>
      val (c, n) = insertStep(step, afterId, h.children, Nil, resultNode)
      insertStep(step, afterId, t, results :+ h.copy(children = c), n)
  }
}

/**
 *
 * @since 26/04/2013
 */
class UCEditor extends StatefulSnippet {
  import UCEditor._
  override def dispatch = { case _ => render }

  val id = 1
  var title = ""
  val nodes = MutableMap[String, StepNode]()

  var courses: List[StepNode] =
    StepNode(nextFuncName, 0, s"${id}.0", NewStep,
      StepNode(nextFuncName, 1, "1", NewStep, Nil) :: Nil
    ) :: Nil

  def render = (
    "#steps *" #> StepTemplate
    andThen ".step" #> renderSteps(courses)
    & "#uc_id_num" #> id
    & "@title" #> SHtml.ajaxText(title, onTitleChange(_))
  )

  private def renderStep(n: StepNode) = (
    ".step [id]" #> n.id
    & ".step [class+]" #> s"lvl-${n.level}"
    & ".label *" #> n.label
    & ".label [id]" #> n.labelId
    & "@text" #> SHtml.textarea(n.step.text, (_) => (), "rows" -> "4", "id" -> n.stepTextId)
    & ".add *" #> SHtml.ajaxButton("Add", () => onAddStep(n.id))
  )

  private def renderSteps(nodes: List[StepNode]) = flattenNodes(nodes).map(renderStep)

  /**
   * When the Use Case title is changed, this will update the Normal Course title unless the user has overridden it.
   */
  def onTitleChange(newTitle: String): JsCmd = {
    val oldTitle = title
    title = newTitle
    val ncId = courses.head.stepTextId
    (
      JsCmds.JsIf(
        JE.JsEq(oldTitle, JE.ValById(ncId)),
        JsCmds.SetValById(ncId, newTitle))
    )
  }

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def onAddStep(preceedingNodeId: String): JsCmd = {
    val (newCourses, newNode) = insertStep(NewStep, preceedingNodeId, courses)
    if (newNode.isDefined) {
      courses = newCourses
      val n = newNode.get
      val fn = ".step" #> renderStep(n)
      (
        JqId(preceedingNodeId) ~> JqAfter(fn(StepTemplate))
        & JqId(n.id) ~> JqHide ~> JqSlideDownFast
        & (for (n <- flattenNodes(courses))
          yield JsCmds.SetHtml(n.labelId, Text(n.label)))
      )
    } else
      JsCmds.Noop
  }
}
