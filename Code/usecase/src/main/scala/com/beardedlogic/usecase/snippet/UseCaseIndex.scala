package com.beardedlogic.usecase
package snippet

import net.liftweb.common.{Full, Failure, Box}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.json.Serialization.{write => jsonWrite}
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, ClearClearable}

import lib._
import db.{Dao, UseCaseSummary, UseCaseHeaderUpdateResult}
import util.ErrorMessages
import util.HtmlTransformExt._
import util.JsExt.JsJsonTrigger
import Types._
import UseCaseHeaderUpdateResult._

object UseCaseIndex extends SnippetHelpers {

  final val TriggerAdd = JsJsonTrigger[UseCaseSummary]("uc-add")
  final val TriggerUpdate = JsJsonTrigger[UseCaseSummary]("uc-upd")

  def InitKoViewModel(vmClassName: String, model: AnyRef): CssSel = {
    val json = jsonWrite(model)
    val js = JsCmds.Run(s"var VM=new $vmClassName($json)")
    "#initVM" #> JsCmds.Script(js)
  }

  def render = daoProvider.withSession(dao =>
    ClearClearable
      & InitKoViewModel("UCIViewModel", dao.findAllUseCaseSummaries)
      & ".new_uc button" #> SHtml.ajaxButton("+ New UC", onNew _)
      & ".edit form" #> reusableAjaxForm(onUpdate)
  )

  def onNew(): JsCmd = TriggerAdd.trigger(create())

  def create(): UseCaseSummary = daoProvider.withTransaction { dao =>
    val uc = dao.createUseCaseIdentAndRev1(Defaults.Title)
    new UseCaseSummary(uc, Misc.currentTimeAsIso8601Str)
  }

  def onUpdate(): JsCmd = onUpdate(update)
  def onUpdate(x: Box[UseCaseSummary]): JsCmd = jsPossibleError(x)(TriggerUpdate.trigger)

  def update(): Box[UseCaseSummary] =
    for {
      newTitle <- S.param("title")                          ?~ ErrorMessages.BadRequest
      ucId     <- ExternalId.UseCase.parseB(S.param("eid")) ?~ ErrorMessages.BadRequest
      _        <- Locks.useCase.writeM(ucId)
      dao      <- daoProvider.forTransaction
      savedUc  <- dao.updateUseCaseHeader(ucId, _.copy(title = newTitle)) match {
                    case NewRevision(r)     => Full(r)
                    case DirectUpdate(r)    => Full(r)
                    case AlreadyUpToDate(r) => Full(r)
                    case UseCaseNotFound    => Failure("Use case not found.")
                  }
    } yield new UseCaseSummary(savedUc, Misc.currentTimeAsIso8601Str)
}
