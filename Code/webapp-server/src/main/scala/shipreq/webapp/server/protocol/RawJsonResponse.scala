package shipreq.webapp.server.protocol

import net.liftweb.http._
import net.liftweb.http.provider.HTTPCookie

object RawJsonResponse {
  type RawJson = String

  def headers: List[(String, String)] = S.getResponseHeaders(Nil)
  def cookies: List[HTTPCookie]       = S.responseCookies

  def apply(json: RawJson, code: Int): RawJsonResponse =
    new RawJsonResponse(json, headers, cookies, code)
}

final case class RawJsonResponse(json   : RawJsonResponse.RawJson,
                                 headers: List[(String, String)],
                                 cookies: List[HTTPCookie],
                                 code   : Int) extends LiftResponse {
  def toResponse = {
    val bytes = json.getBytes("UTF-8")
    val h = ("Content-Length", bytes.length.toString) :: ("Content-Type", "application/json; charset=utf-8") :: headers
    InMemoryResponse(bytes, h, cookies, code)
  }
}
