package shipreq.webapp.snippet.sir

import shipreq.webapp.feature.SessionStats
import shipreq.webapp.lib.{Misc, SnippetHelpers}
import shipreq.webapp.util.{ExpireAfter, CacheFn}
import java.util.ResourceBundle
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.joda.time.{DateTime, Period}
import scala.collection.JavaConversions._
import scala.sys.process._
import scala.util.Properties
import scala.xml.{NodeSeq, Text}

object Stats extends SnippetHelpers {

  object Build {
    private val props = ResourceBundle.getBundle("build")
    private def get(key: String) = props.getString("build." + key)
    val Version     = get("version")
    val Revision    = get("revision")
    val TimeStr     = get("time")
    val Time        = new DateTime(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(TimeStr))
  }

  sealed trait StatValue
  case class Number(value: Long) extends StatValue
  case class NumberF(value: Long) extends StatValue
  case class Str(value: String) extends StatValue
  case class Error(msg: String) extends StatValue
  case class TimePeriod(value: Period) extends StatValue
  case object Unknown extends StatValue
  case class Table(rows: List[(StatValue, StatValue)]) extends StatValue

  def timePeriod(ms: Long) = TimePeriod(new Period(ms))
  def table[K,V](rs: List[(K,V)])(fk: K => StatValue)(fv: V => StatValue): StatValue =
    Table(rs.map{case (k,v) => (fk(k), fv(v))})
  def E(f: => StatValue): StatValue =
    try f catch {case t: Throwable => Error(t.getMessage)}

  def formatValue(v: StatValue): NodeSeq = v match {
    case Number(n)     => Text("%,d" format n)
    case NumberF(n)    => <pre>{"%,16d" format n}</pre>
    case Str(s)        => <pre>{s}</pre>
    case TimePeriod(p) => Text(p.toString.replaceFirst("^PT","").toLowerCase.replaceAll("(?<=[a-z])(?=\\d)"," "))
    case Unknown       => <span class="unknown">?</span>
    case Table(rows)   => <table>{rows.map{case (k,v) => <tr><th>{formatValue(k)}</th><td>{formatValue(v)}</td></tr>}}</table>
    case Error(m)      => <span class="err">{m}</span>
  }

  val cache = CacheFn(renderFn)(ExpireAfter(Period seconds 10))

  def render(in: NodeSeq): NodeSeq =
    cache.value(in)

  def renderFn = {
    val stats = allStats
    val evalTime = Misc.currentTimeAsIso8601Str.value
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

  def allStats: List[(String, List[(String, StatValue)])] = daoProvider.withAdminDao(dao => {
    val userCount = dao.statsCountUsers
    List(
      "System & Environment" -> List(
          "build.version"      -> Str(Build.Version)
        , "build.revision"     -> Str(Build.Revision)
        , "build.time"         -> Str(Build.TimeStr)
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
        , "Logged-In Users"    -> Number(SessionStats.loggedInUsers.values.toSet.size)
        , "Session Timeout"    -> (LiftRules.sessionInactivityTimeout.vend.map(timePeriod) openOr Unknown)
      ),
      "Database" -> List[(String, StatValue)](
        "Users"                        -> Number(userCount.registered)
        , "Users pending registration" -> Number(userCount.pending)
        , "Table Sizes"                -> E(table(dao.statsTableSizes)(Str(_))(NumberF(_)))
        , "Index Sizes"                -> E(table(dao.statsIndexSizes)(Str(_))(NumberF(_)))
        , "Database Size"              -> E(Number(dao.statsDatabaseSize))
      )
    )
  })
}
