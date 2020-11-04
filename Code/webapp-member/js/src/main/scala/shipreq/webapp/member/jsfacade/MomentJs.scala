package shipreq.webapp.member.jsfacade

import java.time.Instant
import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@nowarn
trait Moment extends js.Object {
  val isValid            : Boolean = js.native
  def format()           : String  = js.native
  def format(fmt: String): String  = js.native
  def fromNow()          : String  = js.native
  def unix()             : Double  = js.native
  @JSName("valueOf")
  def valueOfL()         : Double  = js.native
}

@JSGlobal("moment")
@js.native
@nowarn
object Moment extends js.Any {
  def apply(): Moment = js.native
  def apply(epochMillis: Double): Moment = js.native
  def unix(epochSeconds: Double): Moment = js.native
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class MomentJs(js: Moment) {
  @inline def formatIso8601 : String  = js.format()
  @inline def formatHuman   : String  = js.format("llll")
  @inline def fromNow()     : String  = js.fromNow()
  @inline def ago()         : String  = js.fromNow()
  @inline def toEpochMilliD : Double  = js.valueOfL()
  @inline def toEpochMilli  : Long    = toEpochMilliD.toLong
  @inline def toEpochSecondD: Double  = js.unix()
  @inline def toEpochSecond : Long    = toEpochSecondD.toLong
  @inline def toInstant     : Instant = Instant.ofEpochMilli(toEpochMilli)
}

object MomentJs {

  def now(): MomentJs =
    MomentJs(Moment())

  def fromInstant(i: Instant): MomentJs =
    MomentJs(Moment(i.toEpochMilli.toDouble))
}