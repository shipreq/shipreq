package com.beardedlogic.shipreq.feature

import com.beardedlogic.shipreq.app.DI
import com.beardedlogic.shipreq.lib.Misc.DateTimeExt
import com.beardedlogic.shipreq.lib.SnippetHelpers
import net.liftweb.common._
import net.liftweb.http.{S, JsonResponse, InMemoryResponse}
import net.liftweb.json.Extraction
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.TimeHelpers.calcTime

/**
 * Expose URLs for diagnostic functions and purposes.
 *
 * @since 28/11/2013
 */
object DiagnosticEndpoints extends DI {

  def Endpoints = List(Ping, DbTestJson, DbTestCsv)

  private def endpoint(name: String) =
    Menu.i(s"diag.$name") / "diag" / name >> Hidden >> Stateless

  implicit def jsonFormats = SnippetHelpers.DefaultJsonFormat

  def textResponse(content: String, mimeType: String = "text/plain") =
    InMemoryResponse(
      content.getBytes("UTF-8"),
      List("Content-Type" -> s"$mimeType; charset=utf-8", "Pragma" -> "no-cache", "Cache-Control" -> "no-cache, private, no-store"),
      Nil, 200)

  // -------------------------------------------------------------------------------------------------------------------

  private val okResp = Full(textResponse("PONG"))

  val Ping = endpoint("ping") >> EarlyResponse(() => okResp)

  // -------------------------------------------------------------------------------------------------------------------

  val DbTestJson = endpoint("db") >> EarlyResponse(() => Full(JsonResponse(Extraction.decompose(dbTest))))

  val DbTestCsv = endpoint("db.csv") >> EarlyResponse(() => {
    val reps = S.param("reps").map(_.toInt).openOr(10)
    val csv = Stream.continually(dbTest.toCsv).take(reps).mkString
    Full(textResponse(csv, "text/csv"))
  })

  case class DbTestResult(timeAB: Long, timeA: Long, timeB: Long, dbClock: String) {
    def toCsv: String = "%4d,%4d,%4d, %s\n".format(timeAB, timeA, timeB, dbClock)
  }

  def dbTest(): DbTestResult = {
    val (ab, (b, dbClock)) =
      calcTime {
        daoProvider.withSession(dao =>
          calcTime {
            dao.diagSelectNow()
          }
        )
      }
    DbTestResult(ab, ab - b, b, dbClock.toIso8601Str)
  }
}
