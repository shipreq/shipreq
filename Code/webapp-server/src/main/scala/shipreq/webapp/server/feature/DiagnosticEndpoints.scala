package shipreq.webapp.server.feature
/*
import java.lang.{Long => JLong}
import java.time.{Duration, Instant}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json.Extraction
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.util.Props
import scalaz.{-\/, \/-}
import shipreq.base.db.DoobieHelpers._
import shipreq.base.util.ErrorOr
import shipreq.taskman.api.Msg.SendDiagEmail
import shipreq.taskman.api.{EmailAddr, MsgId}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.lib.Misc._
import shipreq.webapp.server.lib.SnippetHelpers

/**
 * Expose URLs for diagnostic functions and purposes.
 *
 * @since 28/11/2013
 */
object DiagnosticEndpoints {
  type PM[T] = Menu.ParamMenuable[T]

  def Endpoints = List(Ping, DbTestJson, DbTestCsv, Email, MsgStatus)

  private def endpoint(name: String) =
    Menu.i(s"diag.$name") / "diag" / name >> Hidden >> Stateless

  def calcTime[T](f: => T): (Duration, T) = {
    val start  = Instant.now()
    val result = f
    val end    = Instant.now()
    (Duration.between(start, end), result)
  }

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

  val DbTestJson = endpoint("db") >> EarlyResponse(() => Full(jsonResponse(dbTest())))

  val DbTestCsv = endpoint("db.csv") >> EarlyResponse(() => {
    val reps = S.param("reps").map(_.toInt).openOr(10)
    val csv = Stream.continually(dbTest().toCsv).take(reps).mkString
    Full(textResponse(csv, "text/csv"))
  })

  case class DbTestResult(timeAB: Duration, timeA: Duration, timeB: Duration, dbClock: String) {
    def toCsv: String =
      "%4d,%4d,%4d, %s\n".format(timeAB.toMillis, timeA.toMillis, timeB.toMillis, dbClock)
  }

  def dbTest(): DbTestResult = {
    val (ab, (b, dbClock)) =
      calcTime {
        Global.db.io.trans(DbLogic.admin.diagSelectNow.measureDuration).unsafePerformIO()
      }
    DbTestResult(ab, ab minus b, b, dbClock.toStringIso8601)
  }
}
*/