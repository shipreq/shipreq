package shipreq.base.util

import japgolly.clearconfig._
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

  def config: ConfigDef[RetryCriteria] =
    (ConfigDef.need[Duration]("delay") |@| ConfigDef.get[Int]("limit"))(apply)
}