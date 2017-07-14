package shipreq.base.util

import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config.JavaTimeConfigParsers._
import japgolly.microlibs.config._
import java.time.Duration
import scalaz.syntax.applicative._

final case class RetryCriteria(delay: Duration, maxAttempts: Option[Int]) {

  def apply(attemptsSoFar: Int): Option[Duration] =
    if (maxAttempts.forall(attemptsSoFar < _))
      Some(delay)
    else
      None
}

object RetryCriteria {

  def config: Config[RetryCriteria] =
    (Config.need[Duration]("delay") |@| Config.get[Int]("limit")) (apply _)
}