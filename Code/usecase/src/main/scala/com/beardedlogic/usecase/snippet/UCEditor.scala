package com.beardedlogic.usecase
package snippet

import scala.xml.{Text, NodeSeq}
import net.liftweb.common._
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http._
import net.liftweb.util.Helpers._
import lib._
import field.Field
import model.DAO
import msg.{JavaScript, Reactor}
import JsExt.JqExpr

object UCEditor {
  final val ParamId = "id"
}

class UCEditor extends StatefulSnippet with SnippetHelpers {
  import UCEditor._

  val state = new UseCaseCtx

  // TODO Remove this eventually right? No unsaved UCs.
  if (S.param(ParamId).isDefined)
    (for {
      idStr  <- S.param(ParamId)
      dataId <- ExternalId.parse(idStr) ~> BadResponse()
      lock   <- Locks.UseCase.forRead(dataId)
      dao    <- DAO.forTransaction
      uc     <- Box(dao.findLatestUseCaseByDataId(dataId)) ~> NotFoundResponse()
    } yield UseCaseLoader.loadCheckpoint(uc, dao, lock))
    match {
      case Full(cp)                               => state.restoreCheckpoint(cp)
      case ParamFailure(_, _, _, r: LiftResponse) => respondImmediately(r)
      case _                                      => shouldNeverHappen_!
    }

  state.init
  state.msgCentre.enabled = true

  def dispatch = {case "render" => render}

  def render = (
      ".ucdata *" #> renderFields(state.fields) andThen
        ".title .ucid *" #> state.number.toString
          & ".title @title" #> SHtml.ajaxText(state.title, onTitleChange(_))
          & ".rev *" #> revText
          & ".saveUseCase" #> SHtml.ajaxButton("Save", jsCallbackWithDao(saveUpdate))
      )

  @inline def renderFields(fields: List[Field[_]]) =
    fields.map(_.render).foldLeft(NodeSeq.Empty)(_ ++: _)

  def saveUpdate(r: Reactor, dao: DAO) {
    state.save(dao)
    r(JavaScript)(
      JqExpr(".rev") ~> JqJE.JqHtml(Text(revText))
    )
  }

  def revText = state.lastSave.map(_.uc.value.rev).getOrElse(0).toString

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
}