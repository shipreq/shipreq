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

/**
 * @since 29/04/13
 */
object UCEditor {

  case class UCStep(desc: String)

  val StepTemplate = Templates("_step_form" :: Nil).open_!

  val NewStep = UCStep("")
}

/**
 *
 * @since 26/04/2013
 */
class UCEditor extends StatefulSnippet {
  import UCEditor._

  private var steps = Vector(UCStep("example 1"), UCStep("example 2"))
  private var stepCount = steps.size

  override def dispatch = { case _ => render }

  def render =
    "#uce *" #> StepTemplate andThen
      "#uce .step" #> steps.map(renderStep)

  private def renderStep(s: UCStep) = {
    val id = nextFuncName
    var desc = ""
    ".step [id]" #> id &
      "@desc" #> SHtml.textarea(s.desc, desc = _, "rows" -> "4") &
      "@add" #> SHtml.ajaxSubmit("Add", () => addStep()) &
      "@del" #> SHtml.ajaxSubmit("Del", () => deleteStep(id))
  }

  private def addStep(): JsCmd = {
    stepCount += 1
    val newStepHtml: NodeSeq = renderStep(NewStep)(StepTemplate)
    JqJsCmds.AppendHtml("uce", newStepHtml)
  }

  private def deleteStep(id: String): JsCmd = {
    stepCount -= 1
    var r = JsCmds.SetHtml(id, NodeSeq.Empty)
    if (stepCount == 0) addStep & r else r
  }
}
