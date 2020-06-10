package shipreq.webapp.client.ww.api

import boopickle.ConstPickler
import boopickle.DefaultBasic._
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.text.ProjectText

// Another idea could be to maintain a separate ClientData instance in the WW thread and feed it all the same updates
// that the main thread processes.

sealed abstract class WebWorkerCmd[Result](implicit final val resultPickler: Pickler[Result])

object WebWorkerCmd {

  // Using instead of Unit so that we can define an implicit Pickler here and have it be universally in scope
  case object NoResult

  final case class SetProject(projectAndOrd: ProjectAndOrd) extends WebWorkerCmd[NoResult.type]

  final case class UpdateProject(events: VerifiedEvent.NonEmptySeq) extends WebWorkerCmd[NoResult.type]

  type Ord = Option[EventOrd.Latest]

  final case class GraphUseCaseStepFlow(ord: Ord,
                                        id : UseCaseId,
                                        ctx: ProjectText.Context) extends WebWorkerCmd[ErrorMsg \/ Svg]

  final case class GraphReqImplications(ord       : Ord,
                                        focus     : ReqId,
                                        filterDead: FilterDead) extends WebWorkerCmd[ErrorMsg \/ Svg]

  final case class GraphAllImplications(ord       : Ord,
                                        filterDead: FilterDead,
                                        scope     : Option[Set[ReqId]],
                                        config    : ImpGraphConfig) extends WebWorkerCmd[ErrorMsg \/ Svg]

  // ===================================================================================================================

  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.base.protocol.binary.v1.Rev1._
  import shipreq.webapp.base.protocol.binary.v1.Rev1.SavedViewPicklers._

  implicit val picklerSvg: Pickler[Svg] =
    transformPickler(Svg.apply)(_.content)

  implicit val picklerNoResult: Pickler[NoResult.type] =
    ConstPickler(NoResult)

  private implicit val picklerOrd: Pickler[Ord] =
    transformPickler[Ord, Int](
      i => Option.when(i > 0)(EventOrd.Latest(i)))(
      _.fold(0)(_.value))

  implicit val picklerSetProject: Pickler[SetProject] =
    transformPickler(SetProject.apply)(_.projectAndOrd)

  implicit val picklerUpdateProject: Pickler[UpdateProject] =
    transformPickler(UpdateProject.apply)(_.events)

  implicit val picklerErrorMsgOrSvg: Pickler[ErrorMsg \/ Svg] =
    pickleDisj

  private implicit val picklerGraphUseCaseStepFlow: Pickler[GraphUseCaseStepFlow] =
    new Pickler[GraphUseCaseStepFlow] {
      override def pickle(a: GraphUseCaseStepFlow)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.id)
        state.pickle(a.ctx)
      }
      override def unpickle(implicit state: UnpickleState): GraphUseCaseStepFlow = {
        val ord = state.unpickle[Ord]
        val id  = state.unpickle[UseCaseId]
        val ctx = state.unpickle[ProjectText.Context]
        GraphUseCaseStepFlow(ord, id, ctx)
      }
    }

  implicit val picklerGraphReqImplications: Pickler[GraphReqImplications] =
    new Pickler[GraphReqImplications] {
      override def pickle(a: GraphReqImplications)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.focus)
        state.pickle(a.filterDead)
      }
      override def unpickle(implicit state: UnpickleState): GraphReqImplications = {
        val ord        = state.unpickle[Ord]
        val focus      = state.unpickle[ReqId]
        val filterDead = state.unpickle[FilterDead]
        GraphReqImplications(ord, focus, filterDead)
      }
    }

  implicit val picklerGraphAllImplications: Pickler[GraphAllImplications] =
    new Pickler[GraphAllImplications] {
      override def pickle(a: GraphAllImplications)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.filterDead)
        state.pickle(a.scope)
        state.pickle(a.config)
      }
      override def unpickle(implicit state: UnpickleState): GraphAllImplications = {
        val ord        = state.unpickle[Ord]
        val filterDead = state.unpickle[FilterDead]
        val scope      = state.unpickle[Option[Set[ReqId]]]
        val config     = state.unpickle[ImpGraphConfig]
        GraphAllImplications(ord, filterDead, scope, config)
      }
    }

  implicit val picklerCmd: Pickler[WebWorkerCmd[_]] =
    new Pickler[WebWorkerCmd[_]] {
      private[this] final val KeySetProject           = 0
      private[this] final val KeyUpdateProject        = 1
      private[this] final val KeyGraphAllImplications = 2
      private[this] final val KeyGraphReqImplications = 3
      private[this] final val KeyGraphUseCaseStepFlow = 4
      override def pickle(a: WebWorkerCmd[_])(implicit state: PickleState): Unit =
        a match {
          case b: SetProject           => state.enc.writeByte(KeySetProject          ); state.pickle(b)
          case b: UpdateProject        => state.enc.writeByte(KeyUpdateProject       ); state.pickle(b)
          case b: GraphAllImplications => state.enc.writeByte(KeyGraphAllImplications); state.pickle(b)
          case b: GraphReqImplications => state.enc.writeByte(KeyGraphReqImplications); state.pickle(b)
          case b: GraphUseCaseStepFlow => state.enc.writeByte(KeyGraphUseCaseStepFlow); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): WebWorkerCmd[_] =
        state.dec.readByte match {
          case KeySetProject           => state.unpickle[SetProject]
          case KeyUpdateProject        => state.unpickle[UpdateProject]
          case KeyGraphAllImplications => state.unpickle[GraphAllImplications]
          case KeyGraphReqImplications => state.unpickle[GraphReqImplications]
          case KeyGraphUseCaseStepFlow => state.unpickle[GraphUseCaseStepFlow]
        }
    }
}
