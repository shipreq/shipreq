package shipreq.webapp.client.base.jsfacade

import java.time.Instant
import scala.scalajs.js.annotation.JSName
import scalajs.js

@js.native
trait Moment extends js.Object {
  val isValid: Boolean = js.native
  def format(): String = js.native
  def format(fmt: String): String = js.native
  def fromNow(): String = js.native
}

@JSName("moment")
@js.native
object Moment extends js.Any {
  def apply(): Moment = js.native
  def apply(epochMillis: Double): Moment = js.native
  def unix(epochSeconds: Double): Moment = js.native
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

case class MomentJs(js: Moment) {
  @inline def formatIso8601: String = js.format()
  @inline def formatHuman  : String = js.format("llll")
  @inline def fromNow()    : String = js.fromNow()
  @inline def ago()        : String = js.fromNow()
}

object MomentJs {

  def now(): MomentJs =
    MomentJs(Moment())

  def fromInstant(i: Instant): MomentJs =
    MomentJs(Moment(i.toEpochMilli))
}