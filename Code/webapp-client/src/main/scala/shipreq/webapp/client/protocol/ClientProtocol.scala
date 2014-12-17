package shipreq.webapp.client.protocol

import scala.scalajs.js
import scalaz.effect.IO
import scalaz.{-\/, \/-, \/}
import upickle._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.client.lib.{FailureIO, Console}

trait ClientProtocol {
  def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, success: r.d.O => IO[Unit], f: FailureIO): IO[Unit]
}

object ClientProtocol {

  def parseJsObject[T: Reader](a: js.Any): Throwable \/ T =
    try
      \/-(readJs[T](json.readJs(a)))
    catch {
      case e: Throwable => -\/(e)
    }

  def jsonEffect[T: Reader](f: T => IO[Unit]): js.Any => Unit =
    a => parseJsObject[T](a) match {
      case \/-(b) => f(b).unsafePerformIO()
      case -\/(e) => handleJsonParsingError(a, e)
    }

  private def handleJsonParsingError(a: js.Any, e: Throwable): Unit =
    Console.error(s"Parsing failure: $e\nJS: ", a)

  def readCluster[G <: Routine.Group : Reader](a: js.Any) = // TODO rename
    parseJsObject[G](a)

  object Lift extends ClientProtocol {
    override def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, success: r.d.O => IO[Unit], f: FailureIO): IO[Unit] = {
      import r.d.{wi, ro}
      val i = js.encodeURIComponent(write(input))
      val q = s"${r.n}=$i"
      val s = jsonEffect[r.d.O](success)
      val ff = () => {
        Console.error(s"AJAX failure on ${r.n} ⇐ $input")
        f.io.unsafePerformIO()
      }
      IO(LiftAjax.lift_ajaxHandler(q, s, ff, "json"))
    }
  }
}
