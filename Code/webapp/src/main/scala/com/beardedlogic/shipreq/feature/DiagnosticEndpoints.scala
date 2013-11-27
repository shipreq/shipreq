package com.beardedlogic.shipreq.feature

import net.liftweb.common._
import net.liftweb.http.InMemoryResponse
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._

/**
 * Expose URLs for diagnostic functions and purposes.
 *
 * @since 28/11/2013
 */
object DiagnosticEndpoints {

  private def endpoint(name: String) =
    Menu.i(s"diag.$name") / "diag" / name >> Hidden >> Stateless

  private val okResp = Full(InMemoryResponse(
    "OK".getBytes("UTF-8"),
    List("Content-Type" -> "text/plain", "Pragma" -> "no-cache", "Cache-Control" -> "no-cache, private, no-store"),
    Nil, 200))

  val Ping = endpoint("ping") >> EarlyResponse(() => okResp)

  val Endpoints = List(Ping)
}
