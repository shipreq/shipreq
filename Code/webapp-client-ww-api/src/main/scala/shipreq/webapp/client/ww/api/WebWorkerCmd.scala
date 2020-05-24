package shipreq.webapp.client.ww.api

import boopickle.DefaultBasic._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText

// Another idea could be to maintain a separate ClientData instance in the WW thread and feed it all the same updates
// that the main thread processes.

sealed abstract class WebWorkerCmd[Result](implicit final val resultPickler: Pickler[Result])

object WebWorkerCmd {

  final case class GraphUseCaseStepFlow(id     : UseCaseId,
                                        project: Project,
                                        ctx    : ProjectText.Context) extends WebWorkerCmd[Svg]

  final case class GraphAllImplications(filterDead: FilterDead,
                                        imps      : Implications.BiDir,
                                        reqs      : Requirements,
                                        reqTypes  : ReqTypes) extends WebWorkerCmd[Svg]

  final case class GraphReqImplications(focus     : ReqId,
                                        filterDead: FilterDead,
                                        imps      : Implications.BiDir,
                                        reqs      : Requirements,
                                        reqTypes  : ReqTypes) extends WebWorkerCmd[Svg]

  // ===================================================================================================================

  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.base.protocol.binary.v1.Rev1._

  private implicit val picklerGraphUseCaseStepFlow: Pickler[GraphUseCaseStepFlow] =
    new Pickler[GraphUseCaseStepFlow] {
      override def pickle(a: GraphUseCaseStepFlow)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.project)
        state.pickle(a.ctx)
      }
      override def unpickle(implicit state: UnpickleState): GraphUseCaseStepFlow = {
        val id      = state.unpickle[UseCaseId]
        val project = state.unpickle[Project]
        val ctx     = state.unpickle[ProjectText.Context]
        GraphUseCaseStepFlow(id, project, ctx)
      }
    }

  private implicit val picklerGraphAllImplications: Pickler[GraphAllImplications] =
    new Pickler[GraphAllImplications] {
      override def pickle(a: GraphAllImplications)(implicit state: PickleState): Unit = {
        state.pickle(a.filterDead)
        state.pickle(a.imps)
        state.pickle(a.reqs)
        state.pickle(a.reqTypes)
      }
      override def unpickle(implicit state: UnpickleState): GraphAllImplications = {
        val filterDead = state.unpickle[FilterDead]
        val imps       = state.unpickle[Implications.BiDir]
        val reqs       = state.unpickle[Requirements]
        val reqTypes   = state.unpickle[ReqTypes]
        GraphAllImplications(filterDead, imps, reqs, reqTypes)
      }
    }

  private implicit val picklerGraphReqImplications: Pickler[GraphReqImplications] =
    new Pickler[GraphReqImplications] {
      override def pickle(a: GraphReqImplications)(implicit state: PickleState): Unit = {
        state.pickle(a.focus)
        state.pickle(a.filterDead)
        state.pickle(a.imps)
        state.pickle(a.reqs)
        state.pickle(a.reqTypes)
      }
      override def unpickle(implicit state: UnpickleState): GraphReqImplications = {
        val focus      = state.unpickle[ReqId]
        val filterDead = state.unpickle[FilterDead]
        val imps       = state.unpickle[Implications.BiDir]
        val reqs       = state.unpickle[Requirements]
        val reqTypes   = state.unpickle[ReqTypes]
        GraphReqImplications(focus, filterDead, imps, reqs, reqTypes)
      }
    }

  implicit val picklerCmd: Pickler[WebWorkerCmd[_]] =
    new Pickler[WebWorkerCmd[_]] {
      private[this] final val KeyGraphAllImplications = 0
      private[this] final val KeyGraphReqImplications = 1
      private[this] final val KeyGraphUseCaseStepFlow = 2
      override def pickle(a: WebWorkerCmd[_])(implicit state: PickleState): Unit =
        a match {
          case b: GraphAllImplications => state.enc.writeByte(KeyGraphAllImplications); state.pickle(b)
          case b: GraphReqImplications => state.enc.writeByte(KeyGraphReqImplications); state.pickle(b)
          case b: GraphUseCaseStepFlow => state.enc.writeByte(KeyGraphUseCaseStepFlow); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): WebWorkerCmd[_] =
        state.dec.readByte match {
          case KeyGraphAllImplications => state.unpickle[GraphAllImplications]
          case KeyGraphReqImplications => state.unpickle[GraphReqImplications]
          case KeyGraphUseCaseStepFlow => state.unpickle[GraphUseCaseStepFlow]
        }
    }
}
