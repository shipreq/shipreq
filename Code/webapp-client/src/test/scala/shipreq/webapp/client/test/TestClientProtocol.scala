package shipreq.webapp.client.test

import scalaz.std.AllInstances._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.client.lib.FailureIO
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.test.TestUtil._

import scalaz.effect.IO

trait Comm {
  type D <: Routine.Desc
  val r: Routine.Remote[D]
  val i: r.d.I
  val s: r.d.O => IO[Unit]
  val f: FailureIO
}

class TestClientProtocol extends ClientProtocol {
  var comms = Vector.empty[Comm]

  override def call[_D <: Routine.Desc](_r: Routine.Remote[_D])(_i: _r.d.I, _s: _r.d.O => IO[Unit], _f: FailureIO): IO[Unit] =
    IO {
      comms :+= new Comm {
        override type D = _D
        override val r: _r.type = _r
        override val i = _i
        override val s = _s
        override val f = _f
      }
    }

  def assertCommsSent(count: Int): Unit =
    assertEq("AJAX requests", count, comms.size)
}