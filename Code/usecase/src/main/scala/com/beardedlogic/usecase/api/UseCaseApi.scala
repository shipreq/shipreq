package com.beardedlogic.usecase
package api

import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json._
import com.beardedlogic.usecase.lib.{Locks, ExternalId}
import lib.HttpResponses.PreconditionRequiredResponse
import model._
import model.DbOpResult._
import ApiHelpers._

object UseCaseApi extends RestHelper with Logger {

  serve(List("api") prefix {
    case "usecase" :: ExternalId(valueId) :: Nil JsonPut json -> _ => updateUseCase(valueId, json)
  })

  case class UpdateUseCaseInput(title: String)

  def updateUseCase(valueId: Long, json: JValue): Either[LiftResponse, LiftResponse] =
    for {
      input <- json.parseInput[UpdateUseCaseInput]
      dao <- DAO.forSessionRight
      uc <- dao.findUseCase(valueId) ~> NotFoundResponse()
    } yield Locks.UseCase.withWriteLockAndTransaction(uc.dataId, dao) {
      val tgt = uc.copy(title = input.title)
      dao.updateUseCaseHeader(tgt) match {
        case Success(_, uc) => dao.findUseCaseSummary(uc).get.toJsonResponse
        case StaleRevision => PreconditionRequiredResponse()
      }
    }
}
