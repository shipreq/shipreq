package shipreq.webapp.client.lib

import scalaz.Bind
import scalaz.effect.IO

final case class FailureIO(io: IO[Unit])

object FailureIO {

  def nop = FailureIO(IO(()))

  def lazily(f: => IO[Unit]): FailureIO = FailureIO(Bind[IO].join(IO(f)))
}