package shipreq.taskman.server

import org.slf4j.{MDC => SMDC}
import scalaz.effect.IO
import shipreq.base.util.log.MDC

object TaskmanLogging {

  val whoKey = "who"

  def mdc(who: String) = MDC(whoKey -> who)

  type MdcValues = String

  val readMdc: IO[MdcValues] = IO(SMDC get whoKey)

  def writeMdc(who: MdcValues): IO[Unit] = IO(SMDC.put(whoKey, who))
}

