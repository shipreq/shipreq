package shipreq.webapp.server.protocol

import net.liftweb.http._
import net.liftweb.http.provider.HTTPCookie

object BinaryResponse {
  type Binary = java.nio.ByteBuffer

  def headers: List[(String, String)] = S.getResponseHeaders(Nil)
  def cookies: List[HTTPCookie]       = S.responseCookies

  def apply(bin: Binary, code: Int = 200): BinaryResponse =
    new BinaryResponse(bin, headers, cookies, code)
}

final case class BinaryResponse(bin    : BinaryResponse.Binary,
                                headers: List[(String, String)],
                                cookies: List[HTTPCookie],
                                code   : Int) extends LiftResponse {
  def toResponse = {
    val size = bin.limit()
    val h = ("Content-Length" -> size.toString) :: ("Content-Type" -> "application/octet-stream") :: headers
    new OutputStreamResponse(_.write(bin.array, 0, size), size, h, cookies, code)
  }
}
