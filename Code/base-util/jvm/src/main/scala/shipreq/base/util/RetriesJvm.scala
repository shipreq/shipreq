package shipreq.base.util

import cats.syntax.apply._
import japgolly.clearconfig._
import java.time.Duration

object RetriesJvm {

  def config: ConfigDef[Retries] =
    (ConfigDef.need[Duration]("delay"), ConfigDef.get[Int]("limit")).mapN {
      case (delay, Some(limit)) => Retries.continually(delay).take(limit)
      case (delay, None)        => Retries.continually(delay)
    }

}
