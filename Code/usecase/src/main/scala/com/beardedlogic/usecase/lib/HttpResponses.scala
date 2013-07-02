package com.beardedlogic.usecase.lib

import net.liftweb.http.{InMemoryResponse, HeaderDefaults, LiftResponse}

object HttpResponses {

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
  case class PreconditionRequiredResponse(
    msg: String = "That's already been updated. The data you're seeing is out-of-date. Reload and try again."
    ) extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 428)
  }

  /**
   * Analogous to IllegalStateException.
   */
  case class ShouldNeverHappenResponse(msg: String = "Unexpected branch encountered.") extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 500)
  }

}