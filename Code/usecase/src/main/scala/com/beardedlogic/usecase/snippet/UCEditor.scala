package com.beardedlogic.usecase
package snippet

import lib.JsExt.{ JqAfter, JqHide, JqId, JqSlideDownFast }
import lib.StepLabels.LABEL_MAKERS
import lib.StepTree.{ NewStep, StepNode, flattenNodes, insertStep }
import net.liftweb.http.{ StatefulSnippet, Templates }
import net.liftweb.http.SHtml
import net.liftweb.http.js.{ JE, JsCmd, JsCmds }
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import scala.xml.Text

/**
 * @since 29/04/13
 */
object UCEditor {

  val StepTemplate = {
    val ExtractStepTemplate = ".step ^^" #> ""
    val index = Templates("index" :: Nil).open_!
    ExtractStepTemplate(ClearClearable(index))
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
  var courses: List[StepNode] =
    StepNode(nextFuncName, 0, s"${id}.0", NewStep,
      StepNode(nextFuncName, 1, LABEL_MAKERS(0)(1), NewStep, Nil) :: Nil
    ) :: Nil

  def render = (
    "#steps *" #> StepTemplate andThen
    ".step" #> renderSteps(courses)
    & "#uc_id_num" #> id
    & "@title" #> SHtml.ajaxText(title, onTitleChange(_))
  )

  private def renderSteps(nodes: List[StepNode]) = flattenNodes(nodes).map(renderStep)

  private def renderStep(n: StepNode) = (
    ".step [id]" #> n.id
    & ".step [class+]" #> s"lvl-${n.level}"
    & ".label *" #> n.label
    & ".label [id]" #> n.labelId
    & "@text" #> SHtml.textarea(n.step.text, (_) => (), "rows" -> "4", "id" -> n.stepTextId)
    & ".add *" #> SHtml.ajaxButton("Add", () => onAddStep(n.id))
  )

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
    if (newNode.isEmpty) JsCmds.Noop
    else {
      courses = newCourses
      val n = newNode.get
      val fn = ".step" #> renderStep(n)
      (
        JqId(preceedingNodeId) ~> JqAfter(fn(StepTemplate))
        & JqId(n.id) ~> JqHide ~> JqSlideDownFast
        & (for (n <- flattenNodes(courses)) yield JsCmds.SetHtml(n.labelId, Text(n.label)))
      )
    }
  }
}
