package com.beardedlogic.usecase
package api

import net.liftweb.http._
import net.liftweb.json._
import lib.ExternalId

/**
 * @since 15/06/2013
 */
object ApiHelpers {

  /**
   * The server does not meet one of the preconditions that the requester put on the request.
   */
  case class PreconditionFailedResponse(msg: String) extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 412)
  }

  /**
   * The request was well-formed but was unable to be followed due to semantic errors.
   * (ie. prohibited fields in the data)
   */
  case class UnprocessableEntityResponse(msg: String) extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 422)
  }

  /**
   * The origin server requires the request to be conditional. Intended to prevent "the 'lost update' problem, where a
   * client GETs a resource's state, modifies it, and PUTs it back to the server, when meanwhile a third party has
   * modified the state on the server, leading to a conflict."
   */
  case class PreconditionRequiredResponse(msg: String = "That's already been updated. The data you're seeing is out-of-date. Reload and try again.") extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 428)
  }

  implicit class JsonExt(val json: JValue) extends AnyVal {
    /**
     * Parses input JSON and returns either a successfully parsed (deserialised) object, or a LiftResponse indicating
     * that the input was invalid.
     */
    def parseInput[T](implicit m: Manifest[T], formats: Formats): Either.RightProjection[LiftResponse, T] =
      (try Right(json.extract[T])
      catch {case _: net.liftweb.json.MappingException => Left(BadResponse())}
        ).right
  }

  implicit class OptionExt[T](val o: Option[T]) extends AnyVal {
    /**
     * Turns an Option into either a LiftResponse when empty, or the value with the option has one.
     */
    def ~>(responseIfEmpty: LiftResponse) = o.toRight(responseIfEmpty).right
  }

  implicit def eitherLiftResponse(e: Either[LiftResponse, LiftResponse]) = e.merge

  implicit class AnyExt(val x: Any) extends AnyVal {
    def toJson(implicit formats: Formats) = Extraction.decompose(x)
    def toJsonResponse(implicit formats: Formats) = JsonResponse(x.toJson)
  }
}