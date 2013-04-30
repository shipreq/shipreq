package com.beardedlogic.usecase
package snippet

import net.liftweb.http.SHtml
import net.liftweb.http.StatefulSnippet
import net.liftweb.http.Templates
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.jquery.JqJsCmds
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scala.xml.Text
import scala.collection.mutable.{ Map => MutableMap }
import net.liftweb.util.ClearClearable

/**
 * @since 29/04/13
 */
object UCEditor {

  case class Step(text: String)
  case class StepNode(level: Int, position: String, id: String, children: List[StepNode])

  // TODO laziness here is temporary
  lazy val StepTemplate = ClearClearable(Templates("_step_form" :: Nil).open_!)

  def flattenNodes(nodes: List[StepNode]): List[StepNode] = nodes match {
    case h :: t => h :: flattenNodes(h.children ::: t)
    case _      => Nil
  }
}

/**
 *
 * @since 26/04/2013
 */
class UCEditor extends StatefulSnippet {
  import UCEditor._

  val id = 1
  var title = ""

  val steps = MutableMap[String, Step]()
  val courses: List[StepNode] = init

  def init = {
    val step01 = ("a", Step(""))
    val step0 = ("b", Step(""))
    steps += (step0, step01)
    val step0Children: List[StepNode] = StepNode(1, "1", step01._1, Nil) :: Nil
    StepNode(0, s"${id}.0", step0._1, step0Children) :: Nil
  }

  override def dispatch = { case _ => render }

  def render =
    "#uc_id_num" #> id &
      "@title" #> SHtml.ajaxText(title, onTitleChange(_)) &
      "#steps *" #> StepTemplate andThen
      ".step" #> renderSteps(courses)

  private def renderSteps(nodes: List[StepNode]) =
    flattenNodes(nodes).map(renderStep)

  private def renderStep(n: StepNode) = {
    val s = steps(n.id)
    var todo = ""
    ".step [id]" #> id &
      ".step [class+]" #> s"lvl-${n.level}" &
      ".posTarget" #> n.position.toString &
      "@text" #> SHtml.textarea(s.text, todo = _, "rows" -> "4")
  }

  def onTitleChange(title: String): JsCmd = JsCmds.Noop

  //  private def renderStep(s: Step) = {
  //    val id = nextFuncName
  //    var desc = ""
  //    ".step [id]" #> id &
  //      "@desc" #> SHtml.textarea(s.desc, desc = _, "rows" -> "4") &
  //      "@add" #> SHtml.ajaxSubmit("Add", () => addStep()) &
  //      "@del" #> SHtml.ajaxSubmit("Del", () => deleteStep(id))
  //  }
  //
  //  private def addStep(): JsCmd = {
  //    stepCount += 1
  //    val newStepHtml: NodeSeq = renderStep(NewStep)(StepTemplate)
  //    JqJsCmds.AppendHtml("uce", newStepHtml) & updateStepCount
  //  }
  //
  //  private def deleteStep(id: String): JsCmd = {
  //    stepCount -= 1
  //    JsCmds.SetHtml(id, NodeSeq.Empty) &
  //      (if (stepCount == 0) addStep else JsCmds.Noop) &
  //      updateStepCount
  //  }

}
