package shipreq.webapp.client.protocol

import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.{-\/, \/-, \/}
import upickle._
import shipreq.webapp.shared.protocol.Routine

object ClientProtocol {

  def parseJsObject[T: Reader](a: js.Any): Throwable \/ T =
    try
      \/-(readJs[T](json.readJs(a)))
    catch {
      case e: Throwable => -\/(e)
    }

  def jsonEffect[T: Reader](f: T => Any): js.Any => Unit =
    a => parseJsObject[T](a) match {
      case \/-(b) => f(b); ()
      case -\/(e) => handleJsonParsingError(e)
    }

  private def handleJsonParsingError(e: Throwable): Unit = () // TODO log unless release mode

  def readCluster[G <: Routine.Group : Reader](a: js.Any) = // TODO rename
    parseJsObject[G](a)

  def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, callback: r.d.O => Unit): IO[Unit] = {
    import r.d.{wi, ro}
    val i = js.encodeURIComponent(write(input))
    val success = jsonEffect[r.d.O](callback)
    // TODO failure
    IO(LiftAjax.lift_ajaxHandler(s"${r.n}=$i", success, null, "json"))
  }
}
