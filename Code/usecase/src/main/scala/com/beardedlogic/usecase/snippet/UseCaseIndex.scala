package com.beardedlogic.usecase
package snippet

import net.liftweb.common.{Full, Failure, Box}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmds
import net.liftweb.json.Serialization.{write => jsonWrite}
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, ClearClearable}

import lib._
import db.DbOpResult.{StaleRevision, Success}
import model.{DAO, UseCaseSummary}
import util.{ErrorMessages, Reactor, JavaScript}
import util.HtmlTransformExt._
import util.JsExt.JsonTrigger

object UseCaseIndex extends SnippetHelpers {

  object NewUseCase extends JsonTrigger[UseCaseSummary]("new-uc")
  object UseCaseUpdated extends JsonTrigger[UseCaseSummary]("upd-uc")

  def InitKoViewModel(vmClassName: String, model: AnyRef): CssSel = {
    val json = jsonWrite(model)
    val js = JsCmds.Run(s"var VM=new $vmClassName($json)")
    "#initVM" #> JsCmds.Script(js)
  }

  def render = daoProvider.withSession(dao =>
    ClearClearable
      & InitKoViewModel("UCIViewModel", dao.findAllUseCaseSummaries)
      & ".new_uc button" #> SHtml.ajaxButton("+ New UC", jsCallbackWithDao(createNewUseCase))
      & ".edit form" #> reusableAjaxForm(jsCallback(updateUseCaseHeader(_)))
  )

  def createNewUseCase(reactor: Reactor, dao: DAO): UseCaseSummary = {
    val uc = dao.createInitialUseCase(Defaults.Title, Defaults.FieldList.get)
    val ucs = UseCaseSummary(uc.dataId, uc.valueId, uc.header.number, uc.header.title, Misc.currentTimeAsIso8601Str)
    reactor(JavaScript)(NewUseCase.trigger(ucs))
    ucs
  }

  def updateUseCaseHeader(implicit reactor: Reactor): Box[UseCaseSummary] = {

    val result: Box[UseCaseSummary] = for {
      newTitle <- S.param("title")                        ?~ ErrorMessages.BadRequest
      dataId   <- ExternalId.unapply(S.param("dataEid"))  ?~ ErrorMessages.BadRequest
      valueId  <- ExternalId.unapply(S.param("valueEid")) ?~ ErrorMessages.BadRequest
      lock     <- Locks.UseCase.forWrite(dataId)
      dao      <- daoProvider.forTransaction
      uc       <- dao.findUseCase(dataId, valueId)        ?~ "Use case not found."
      newUc     = uc.withTitle(newTitle)
      savedUc  <- dao.updateUseCaseHeader(newUc) match {
                    case Success(_, r) => Full(r)
                    case StaleRevision => Failure(ErrorMessages.StaleDataSubmitted)
                    case _             => Failure(ErrorMessages.Generic)
                  }
      savedUcs <- dao.findUseCaseSummary(savedUc)
    } yield {
      reactor(JavaScript)(UseCaseUpdated.trigger(savedUcs))
      savedUcs
    }

    reactToOptionalError(result)
    result
  }
}
