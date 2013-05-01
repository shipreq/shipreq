package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{ StatefulSnippet, Templates }
import net.liftweb.http.SHtml
import net.liftweb.http.js.{ JE, JsCmd, JsCmds }
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import scala.collection.mutable.{ Map => MutableMap }

/**
 * @since 29/04/13
 */
object UCEditor {

  case class Step(text: String)
  case class StepNode(level: Int, position: String, id: String, children: List[StepNode])

  val StepTemplate = ClearClearable(Templates("_step_form" :: Nil).open_!)

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
    val step01 = (nextFuncName, Step(""))
    val step0 = (nextFuncName, Step(""))
    steps += (step0, step01)
    val step0Children: List[StepNode] = StepNode(1, "1", step01._1, Nil) :: Nil
    StepNode(0, s"${id}.0", step0._1, step0Children) :: Nil
  }

  override def dispatch = { case _ => render }

  def render =
    "#steps *" #> StepTemplate andThen
      ".step" #> renderSteps(courses) &
      "#uc_id_num" #> id &
      "@title" #> SHtml.ajaxText(title, onTitleChange(_))

  private def renderStep(n: StepNode) = {
    val s = steps(n.id)
    var todo = ""
    ".step [id]" #> id &
      ".step [class+]" #> s"lvl-${n.level}" &
      ".posTarget" #> n.position.toString &
      "@text" #> SHtml.textarea(s.text, todo = _, "rows" -> "4", "id" -> stepTextId(n))
  }

  private def renderSteps(nodes: List[StepNode]) =
    flattenNodes(nodes).map(renderStep)

  private def stepTextId(n: StepNode) = s"${n.id}-t"

  /**
   * When the Use Case title is changed, this will update the Normal Course title unless the user has overridden it.
   */
  def onTitleChange(newTitle: String): JsCmd = {
    val oldTitle = title
    title = newTitle
    val ncId = stepTextId(courses.head)

    JsCmds.JsIf(
      JE.JsEq(oldTitle, JE.ValById(ncId)),
      JsCmds.SetValById(ncId, newTitle)
    )
  }
}
