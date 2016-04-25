package shipreq.webapp.client.ww.api

import shipreq.webapp.base.data._
import boopickle._
import shipreq.webapp.base.protocol.BoopickleMacros._
import shipreq.webapp.base.protocol.BinCodecGeneric._
import shipreq.webapp.base.protocol.BinCodecData._

// Another idea could be to maintain a separate ClientData instance in the WW thread and feed it all the same updates
// that the main thread processes.

trait AbstractCmd {
  type Result
}

/*
sealed abstract class Cmd extends AbstractCmd

object Cmd {

  sealed abstract class Aux[R](implicit r: Pickler[R]) extends Cmd {
    final override type Result = R
    val resultPickler = r
  }

  case class GraphUseCaseSteps(id: UseCaseId, useCases: UseCases) extends Aux[SVG]

  implicit val pickleGraphUseCaseSteps: Pickler[GraphUseCaseSteps] = pickleCaseClass
  implicit val pickleCmd              : Pickler[Cmd              ] = pickleADT
}
*/

sealed abstract class Cmd[R](implicit r: Pickler[R]) {
  val resultPickler = r
}

object Cmd {

  case class GraphUseCaseSteps(id: UseCaseId, useCases: UseCases) extends Cmd[SVG]

  implicit val pickleGraphUseCaseSteps: Pickler[GraphUseCaseSteps] = pickleCaseClass
  implicit val pickleCmd              : Pickler[Cmd[_]           ] = pickleADT
}

/*
sealed abstract class Cmd[+R]

object Cmd {

  case class GraphUseCaseSteps(id: UseCaseId, useCases: UseCases) extends Cmd[SVG]

  type CmdG = Cmd[Any]

  implicit val pickleGraphUseCaseSteps: Pickler[GraphUseCaseSteps] = pickleCaseClass
  implicit val pickleCmd              : Pickler[CmdG              ] = _pickleADT
}
*/




case class SVG(content: String) extends AnyVal
object SVG {
  implicit val pickle: Pickler[SVG] = pickleCaseClass
}
