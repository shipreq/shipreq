package shipreq.webapp.server.util

import net.liftweb.http.{HeaderDefaults, InMemoryResponse, LiftResponse}

object HttpResponses {

//  /**
//   * The server does not meet one of the preconditions that the requester put on the request.
//   */
//  case class PreconditionFailedResponse(msg: String) extends LiftResponse with HeaderDefaults {
//    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 412)
//  }
//
//  /**
//   * The request was well-formed but was unable to be followed due to semantic errors.
//   * (ie. prohibited fields in the data)
//   */
//  case class UnprocessableEntityResponse(msg: String) extends LiftResponse with HeaderDefaults {
//    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 422)
//  }
//
//  /**
//   * The origin server requires the request to be conditional. Intended to prevent "the 'lost update' problem, where a
//   * client GETs a resource's state, modifies it, and PUTs it back to the server, when meanwhile a third party has
//   * modified the state on the server, leading to a conflict."
//   */
//  case class PreconditionRequiredResponse(msg: String = ErrorMessages.StaleDataSubmitted) extends LiftResponse with HeaderDefaults {
//    def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 428)
//  }

  case object ShouldNeverHappenResponse extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(ErrorMessages.ShouldNeverHappen.getBytes("UTF-8"), headers, cookies, 500)
  }

}