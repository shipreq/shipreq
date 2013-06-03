package com.beardedlogic.usecase
package comet

import scala.xml.NodeSeq
import net.liftweb.http.{CometActor, SHtml, StatefulSnippet}
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.JsExp.strToJsExp
import lib.field.Field
import lib.UseCaseCtx
import lib.msg.Messages._

import net.liftweb.common.Logger

class UCEditor extends CometActor with Logger {

  val state = new UseCaseCtx(this)

  override def localSetup() {
    state.init
    state.msgCentre.enabled = true
  }

  override def render = (
      ".ucdata *" #> renderFields(state.fields) andThen
        ".title .ucid *" #> state.number.toString
          & ".title @title" #> SHtml.ajaxText(state.title, onTitleChange(_))
      )

  @inline def renderFields(fields: List[Field[_]]) =
    fields.map(_.render).foldLeft(NodeSeq.Empty)(_ ++: _)

  // TODO Title -> NC change should be done via actors
  /**
   * When the Use Case title is changed, this will update the Normal Course title unless the user has overridden it.
   */
  def onTitleChange(newTitle: String): JsCmd = {
    val oldTitle = state.title
    state.title = newTitle
    (
      JsCmds.JsIf(
        JE.JsOr(
          JE.JsEq(JE.ValById(state.normalCourseTitleId), oldTitle),
          JE.JsEq(JE.ValById(state.normalCourseTitleId), "")),
        JsCmds.SetValById(state.normalCourseTitleId, newTitle))
      )
  }

  override def lowPriority = {
    case PushToClient(cmd) if cmd != JsCmds.Noop =>
      partialUpdate(cmd)
  }
}