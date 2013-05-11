package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{SHtml, StatefulSnippet}
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import lib.field._
import lib.UCEditorState

class UCEditor extends StatefulSnippet {

  val state = new UCEditorState

  override def dispatch = { case _ => render }

  def render = {
    (
      ".ucdata *" #> renderFields(state.fields) andThen
      ".title .ucid *" #> state.ucId.toString
      & ".title @title" #> SHtml.ajaxText(state.title, onTitleChange(_))
    )
  }

  @inline def renderFields(fields: List[Field]) =
    fields.map(_.render).foldLeft(NodeSeq.Empty)(_ ++: _)

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
}