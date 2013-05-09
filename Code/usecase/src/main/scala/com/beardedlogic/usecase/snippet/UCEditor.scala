package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{SHtml, StatefulSnippet}
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

class UCEditor extends StatefulSnippet {
  import lib.field._

  override def dispatch = { case _ => render }

  val ucId = 1
  var title = "Untitled"
  val fields = Fields.DefaultFields.map(_.newFieldInstance)

  val normalCourseTitleId = fields.collectFirst { case f: CourseAndExceptionFields => f.courses.head }.get.stepTextId

  def render = {
    (
      ".ucdata *" #> renderFields(fields) andThen
      ".title .ucid *" #> ucId.toString
      & ".title @title" #> SHtml.ajaxText(title, onTitleChange(_))
    )
  }

  @inline def renderFields(fields: List[Field]) =
    fields.map(_.render).foldLeft(NodeSeq.Empty)(_ ++: _)

  /**
   * When the Use Case title is changed, this will update the Normal Course title unless the user has overridden it.
   */
  def onTitleChange(newTitle: String): JsCmd = {
    val oldTitle = title
    title = newTitle
    (
      JsCmds.JsIf(
        JE.JsOr(
          JE.JsEq(JE.ValById(normalCourseTitleId), oldTitle),
          JE.JsEq(JE.ValById(normalCourseTitleId), "")),
        JsCmds.SetValById(normalCourseTitleId, newTitle))
    )
  }
}