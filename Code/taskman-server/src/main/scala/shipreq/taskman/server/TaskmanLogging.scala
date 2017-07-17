package shipreq.taskman.server

import org.slf4j.{MDC => SMDC}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.MDC

object TaskmanLogging {

  val whoKey = "who"

  def mdc(who: String) = MDC(whoKey -> who)

  type MdcValues = String

  val readMdc: Fx[MdcValues] = Fx(SMDC get whoKey)

  def writeMdc(who: MdcValues): Fx[Unit] = Fx(SMDC.put(whoKey, who))
}

