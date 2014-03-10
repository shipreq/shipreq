package shipreq.webapp.feature

import shipreq.webapp.app.DI
import shipreq.webapp.lib.Misc.DateTimeExt
import shipreq.webapp.lib.{MailHelpers, SnippetHelpers}
import net.liftweb.common._
import net.liftweb.http.{S, BadResponse, JsonResponse, InMemoryResponse, MethodNotAllowedResponse}
import net.liftweb.json.Extraction
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.util.Props
import net.liftweb.util.TimeHelpers.calcTime

/**
 * Expose URLs for diagnostic functions and purposes.
 *
 * @since 28/11/2013
 */
object DiagnosticEndpoints extends DI {

  def Endpoints = List(Ping, DbTestJson, DbTestCsv, Email)

  private def endpoint(name: String) =
    Menu.i(s"diag.$name") / "diag" / name >> Hidden >> Stateless

  def textResponse(content: String, mimeType: String = "text/plain") =
    InMemoryResponse(
      content.getBytes("UTF-8"),
      List("Content-Type" -> s"$mimeType; charset=utf-8", "Pragma" -> "no-cache", "Cache-Control" -> "no-cache, private, no-store"),
      Nil, 200)

  implicit def jsonFormats = SnippetHelpers.DefaultJsonFormat

  def jsonResponse(value: Any) = JsonResponse(Extraction.decompose(value))

  def denyNonHttps =
    if (Props.productionMode)
      EarlyResponse(() => S.request.filter(_.request.scheme != "https").map(_ => MethodNotAllowedResponse()))
    else
      Test(_ => true)

  // -------------------------------------------------------------------------------------------------------------------

  private val pong = Full(textResponse("PONG"))

  val Ping = endpoint("ping") >> EarlyResponse(() => pong)

  // -------------------------------------------------------------------------------------------------------------------

  val DbTestJson = endpoint("db") >> EarlyResponse(() => Full(jsonResponse(dbTest)))

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
        daoProvider.withAdminDao(dao =>
          calcTime {
            dao.diagSelectNow()
          }
        )
      }
    DbTestResult(ab, ab - b, b, dbClock.toIso8601Str)
  }

  // -------------------------------------------------------------------------------------------------------------------

  case class EmailSendResult(time: Long, token: String)

  val Email = endpoint("mail") >> denyNonHttps >> EarlyResponse(() =>
    S.param("to") match {
      case Full(emailAddress) => {
        import MailHelpers._
        val token = nextFuncName
        val mail = plainTextMail(s"TEST: $token", "")
        val time = calcTime(sendMailSync(mail addressedTo emailAddress))._1
        Full(jsonResponse(EmailSendResult(time, token)))
      }
      case _ => Full(BadResponse())
    })
}
