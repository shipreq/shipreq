package shipreq.webapp.client.ui

import scalaz.StateT
import scalaz.Scalaz.Id
import scalaz.effect.IO

object Implicits {

  implicit class StateExt[S, A](val u: StateT[Id, S, A]) extends AnyVal {
    def liftIO: StateT[IO, S, A] = u.lift[IO]
  }

  implicit def autoLiftStateIntoIO[S, A](s: StateT[Id, S, A]) = s.liftIO
}
