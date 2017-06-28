package shipreq.webapp.server.snippet

import java.time._
import java.time.format.DateTimeFormatter
import java.util.ResourceBundle
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import scala.collection.JavaConverters._
import scala.sys.process._
import scala.util.Properties
import scala.xml.{NodeSeq, Text}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.feature.SessionStats
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.util.{CacheFn, ExpireAfter}

object AdminStats extends SnippetHelpers {

  // TODO Use env vars provided by docker
  object Build {
    val Version     = "TODO"
    val Revision    = "TODO"
  }

  sealed trait StatValue
  case class Number    (value: Long)                        extends StatValue
  case class NumberF   (value: Long)                        extends StatValue
  case class Str       (value: String)                      extends StatValue
  case class Error     (msg: String)                        extends StatValue
  case class TimePeriod(value: Duration)                    extends StatValue
  case class Table     (rows: List[(StatValue, StatValue)]) extends StatValue
  case object Unknown                                       extends StatValue

  def timePeriod(ms: Long) = TimePeriod(Duration ofMillis ms)
  def table[K,V](rs: List[(K,V)])(fk: K => StatValue)(fv: V => StatValue): StatValue =
    Table(rs.map{case (k,v) => (fk(k), fv(v))})
  def E(f: => StatValue): StatValue =
    try f catch {case t: Throwable => Error(t.getMessage)}

  val formatValue: StatValue => NodeSeq = {
    case Number(n)     => Text("%,d" format n)
    case NumberF(n)    => <pre>{"%,16d" format n}</pre>
    case Str(s)        => <pre>{s}</pre>
    case TimePeriod(p) => Text(p.toString.replaceFirst("^PT","").toLowerCase.replaceAll("(?<=[a-z])(?=\\d)"," "))
    case Unknown       => <span class="unknown">?</span>
    case Table(rows)   => <table>{rows.map{case (k,v) => <tr><th>{formatValue(k)}</th><td>{formatValue(v)}</td></tr>}}</table>
    case Error(m)      => <span class="err">{m}</span>
  }

  val cache = CacheFn(renderFn)(ExpireAfter(Duration ofMinutes 10))

  def render(in: NodeSeq): NodeSeq =
    cache.value(in)

  def renderFn = {
    val stats = allStats()
    val evalTime = Instant.now().toStringIso8601
    (
      "time [datetime]" #> evalTime andThen
      "section" #> stats.map{ case (section,subStats) =>
        ".h *" #> section &
        "tr" #> subStats.map{case (key, value) =>
          "th *" #> key &
          "td *" #> formatValue(value)
        }
      }
    )
  }

  def allStats(): List[(String, List[(String, StatValue)])] =
    Global.db.io.trans(
    for {
      userCount <- DbLogic.admin.statsCountUsers
      tableSizes <- DbLogic.admin.statsTableSizes
      indexSizes <- DbLogic.admin.statsIndexSizes
      databaseSize <- DbLogic.admin.statsDatabaseSize(Global.db.databaseName)
    } yield
      List(
        "System & Environment" -> List(
            "build.version"      -> Str(Build.Version)
          , "build.revision"     -> Str(Build.Revision)
          , "java.version"       -> Str(Properties.javaVersion)
          , "jvm.version"        -> Str(Properties.javaVmVersion)
          , "scala.version"      -> Str(Properties.versionNumberString)
          , "run.mode"           -> Str(Props.mode.toString)
          , "sys.free"           -> E(Str("free -h".!!.trim))
          , "sys.uptime"         -> E(Str("uptime".!!.trim))
        ),
        "Sessions & Logins" -> List(
          "Active Sessions"      -> Number(SessionStats.activeSessionCount.get)
          , "Logged-In Sessions" -> Number(SessionStats.loggedInUsers.size)
          , "Logged-In Users"    -> Number(SessionStats.loggedInUsers.values.asScala.toSet.size)
          , "Session Timeout"    -> (LiftRules.sessionInactivityTimeout.vend.map(timePeriod) openOr Unknown)
        ),
        "Database" -> List[(String, StatValue)](
          "Users"                        -> Number(userCount.registered)
          , "Users pending registration" -> Number(userCount.pendingRegistration)
          , "Table Sizes"                -> E(table(tableSizes)(Str)(NumberF))
          , "Index Sizes"                -> E(table(indexSizes)(Str)(NumberF))
          , "Database Size"              -> E(Number(databaseSize))
        )
      )
  ).unsafePerformIO()
}
