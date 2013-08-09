package com.beardedlogic.usecase
package snippet

import net.liftweb.common.{Full, Failure, Box}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmds
import net.liftweb.json.Serialization.{write => jsonWrite}
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, ClearClearable}

import lib._
import db.DbOpResult.{NothingUpdated, Success}
import model.{DAO, UseCaseSummary}
import util.{ErrorMessages, Reactor, JavaScript}
import util.HtmlTransformExt._
import util.JsExt.JsonTrigger
import Types._

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
    val uc = dao.createInitialUseCase(Defaults.Title)
    val ucs = new UseCaseSummary(uc, Misc.currentTimeAsIso8601Str)
    reactor(JavaScript)(NewUseCase.trigger(ucs))
    ucs
  }

  def updateUseCaseHeader(implicit reactor: Reactor): Box[UseCaseSummary] = {
    // TODO use case index: stop sending valueId around

    val result: Box[UseCaseSummary] = for {
      newTitle <- S.param("title")                                               ?~ ErrorMessages.BadRequest
      ucId     <- tag[UseCaseIdentIdTag](ExternalId.unapply(S.param("dataEid"))) ?~ ErrorMessages.BadRequest
      lock     <- Locks.UseCase.forWrite(ucId)
      dao      <- daoProvider.forTransaction
      savedUc  <- dao.updateUseCaseHeader(ucId, _.copy(title= newTitle)) match {
                    case Success(_, r)  => Full(r)
                    case NothingUpdated => Failure("Use case not found.")
                    case _              => Failure(ErrorMessages.Generic)
                  }
    } yield {
      val ucs = new UseCaseSummary(savedUc, Misc.currentTimeAsIso8601Str)
      reactor(JavaScript)(UseCaseUpdated.trigger(ucs))
      ucs
    }

    reactToOptionalError(result)
    result
  }
}
