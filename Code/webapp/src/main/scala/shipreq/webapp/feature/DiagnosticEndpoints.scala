package shipreq.webapp.feature

import java.lang.{Long => JLong}
import net.liftweb.common._
import net.liftweb.http.{S, BadResponse, JsonResponse, InMemoryResponse, MethodNotAllowedResponse, NotFoundResponse}
import net.liftweb.json.Extraction
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.util.Props
import net.liftweb.util.TimeHelpers.calcTime
import scalaz.{\/-, -\/}
import shipreq.base.util.ErrorOr
import shipreq.webapp.app.DI
import shipreq.webapp.lib.Misc.DateTimeExt
import shipreq.webapp.lib.{Misc, SnippetHelpers}
import shipreq.taskman.api.ApiOp.{QueryMsgStatus, SubmitMsg}
import shipreq.taskman.api.Msg.SendDiagEmail
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{Msg, ApiOp, MsgId}

/**
 * Expose URLs for diagnostic functions and purposes.
 *
 * @since 28/11/2013
 */
object DiagnosticEndpoints extends DI {
  type PM[T] = Menu.ParamMenuable[T]

  def Endpoints = List(Ping, DbTestJson, DbTestCsv, Email, MsgStatus)

  private def endpoint(name: String) =
    Menu.i(s"diag.$name") / "diag" / name >> Hidden >> Stateless

  def parseJLong(s: String): Box[JLong] =
    ErrorOr.safe(JLong parseLong s) match {
      case -\/(err) => Failure(err.msg)
      case \/-(id)  => Full(id)
    }

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
  // Ping

  private val pong = Full(textResponse("PONG"))

  val Ping = endpoint("ping") >> EarlyResponse(() => pong)

  // -------------------------------------------------------------------------------------------------------------------
  // DB connectivity

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
  // Email

  case class EmailSendResult(id: MsgId, time: Long, token: String)

  val Email = endpoint("mail") >> denyNonHttps >> EarlyResponse(() =>
    S.param("to") match {
      case Full(emailAddress) => {
        val token = nextFuncName
        val msg = SendDiagEmail(emailAddress.tag, token, s"Token: $token\nIssued: ${Misc.currentTimeAsIso8601Str}")
        val (time, msgId) = calcTime(taskman1(_ submitMsg msg))
        Full(jsonResponse(EmailSendResult(msgId, time, token)))
      }
      case _ => Full(BadResponse())
    })

  // -------------------------------------------------------------------------------------------------------------------
  // Taskman & msgs

  case class MsgStatusResult(id: MsgId, status: String, archived: Boolean)

  val MsgStatus: PM[JLong] =
    Menu.param[JLong]("diag.msg.status", "", parseJLong, _.toString) / "diag" / "msg" / * >>
      Hidden >> Stateless >> denyNonHttps >> EarlyResponse(() =>
        MsgStatus.currentValue match {
          case Full(l) =>
            val id = MsgId(l)
            taskman1(_ run QueryMsgStatus(id)) match {
              case Some(status) => Full(jsonResponse(MsgStatusResult(id, status.toString, status.isArchived)))
              case None         => Full(NotFoundResponse("Msg not found."))
            }
          case _ => Full(BadResponse())
        }
      )
}
