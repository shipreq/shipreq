package shipreq.base.util

import japgolly.clearconfig._
import java.time.Duration
import scalaz.syntax.applicative._

object RetriesJvm {

  def config: ConfigDef[Retries] =
    (ConfigDef.need[Duration]("delay") |@| ConfigDef.get[Int]("limit")) {
      case (delay, Some(limit)) => Retries.continually(delay).take(limit)
      case (delay, None)        => Retries.continually(delay)
    }

}
