package shipreq.webapp.client.lib

import scalaz.Bind
import scalaz.effect.IO

final case class SuccessIO(io: IO[Unit])

object SuccessIO {

  val nop = SuccessIO(IO(()))

  def lazily(f: => IO[Unit]): SuccessIO = SuccessIO(Bind[IO].join(IO(f)))
}