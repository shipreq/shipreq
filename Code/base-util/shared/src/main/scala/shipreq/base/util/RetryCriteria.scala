package shipreq.base.util

import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config.JavaTimeConfigParsers._
import japgolly.microlibs.config._
import java.time.Duration
import scalaz.syntax.applicative._

final case class RetryCriteria(delay: Duration, maxRetries: Option[Int]) {

  def apply(retriesSoFar: Int): Option[Duration] =
    if (maxRetries.forall(retriesSoFar < _))
      Some(delay)
    else
      None
}

object RetryCriteria {

  def config: Config[RetryCriteria] =
    (Config.need[Duration]("delay") |@| Config.get[Int]("limit")) (apply _)
}