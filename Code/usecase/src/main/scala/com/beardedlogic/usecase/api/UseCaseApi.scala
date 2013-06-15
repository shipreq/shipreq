package com.beardedlogic.usecase
package api

import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json._
import net.liftweb.util.Helpers._
import lib.Misc.currentTimeAsIso8601Str
import model._
import model.DbOpResult._
import ApiHelpers._

object UseCaseApi extends RestHelper {

  serve(List("api") prefix {
    case "usecase" :: AsLong(valueId) :: Nil JsonPut json -> _ => updateUseCase(valueId, json)
  })

  case class UpdateUseCaseInput(title: String)
  def updateUseCase(valueId: Long, json: JValue): Either[LiftResponse, LiftResponse] =
    for {
      input <- json.parseInput[UpdateUseCaseInput]
      dao <- DAO.forTransaction
      uc <- dao.findUseCase(valueId) ~> NotFoundResponse()
    } yield {
      val tgt = uc.copy(title = input.title)
      dao.updateUseCaseHeader(tgt) match {
        case (_, Some(uc)) => JsonResponse(Extraction.decompose(uc.toSummary(currentTimeAsIso8601Str)))
        case (StaleRevision, _) => PreconditionRequiredResponse()
      }
    }
}
