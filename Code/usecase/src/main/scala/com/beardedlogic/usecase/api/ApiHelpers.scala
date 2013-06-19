package com.beardedlogic.usecase
package api

import net.liftweb.http._
import net.liftweb.json._

/**
 * @since 15/06/2013
 */
object ApiHelpers {

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