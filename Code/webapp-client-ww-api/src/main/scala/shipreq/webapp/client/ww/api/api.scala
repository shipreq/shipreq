package shipreq.webapp.client.ww.api

import shipreq.webapp.base.data._
import boopickle._
import shipreq.webapp.base.protocol.BoopickleMacros._
import shipreq.webapp.base.protocol.BinCodecGeneric._
import shipreq.webapp.base.protocol.BinCodecMemberData._

// Another idea could be to maintain a separate ClientData instance in the WW thread and feed it all the same updates
// that the main thread processes.

sealed abstract class Cmd[Result](implicit r: Pickler[Result]) {
  val resultPickler = r
}

object Cmd {
  case class GraphUseCaseStepFlow(id: UseCaseId, useCases: UseCases) extends Cmd[SVG]

  case class GraphAllImplications(filterDead: FilterDead,
                                  imps      : Implications.BiDir,
                                  reqs      : Requirements,
                                  reqTypes  : ReqTypes) extends Cmd[SVG]

  case class GraphReqImplications(focus     : ReqId,
                                  filterDead: FilterDead,
                                  imps      : Implications.BiDir,
                                  reqs      : Requirements,
                                  reqTypes  : ReqTypes) extends Cmd[SVG]

  implicit val pickleGraphUseCaseStepFlow: Pickler[GraphUseCaseStepFlow] = pickleCaseClass
  implicit val pickleGraphAllImplications: Pickler[GraphAllImplications] = pickleCaseClass
  implicit val pickleGraphReqImplications: Pickler[GraphReqImplications] = pickleCaseClass
  implicit val pickleCmd                 : Pickler[Cmd[_]              ] = pickleADT
}

case class SVG(content: String) extends AnyVal
object SVG {
  implicit val pickle: Pickler[SVG] = pickleCaseClass
}
