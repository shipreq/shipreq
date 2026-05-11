package shipreq.webapp.client.ww.api

import boopickle.ConstPickler
import boopickle.DefaultBasic._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.{ProjectCreator, ProjectId, UserId}
import shipreq.webapp.base.protocol.Version
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.storage.ClientSideStorage
import shipreq.webapp.member.project.text.ProjectText

// Another idea could be to maintain a separate ClientData instance in the WW thread and feed it all the same updates
// that the main thread processes.

sealed abstract class WebWorkerCmd[Result](implicit final val resultPickler: Pickler[Result])

object WebWorkerCmd {

  // Using instead of Unit so that we can define an implicit Pickler here and have it be universally in scope
  case object NoResult

  final case class Init(am    : AssetManifest,
                        cssCtx: ClientSideStorage.Context,
                        encKey: ClientSideProjectEncryptionKey) extends WebWorkerCmd[NoResult.type]

  final case class UpdateProject(data: Project \/ VerifiedEvent.Seq) extends WebWorkerCmd[NoResult.type]

  case object ClearAndDisableCache extends WebWorkerCmd[NoResult.type]

  type Ord = Option[EventOrd.Latest]

  final case class GraphUseCaseFlow(ord: Ord,
                                    id : UseCaseId,
                                    ctx: ProjectText.Context) extends WebWorkerCmd[ErrorMsg \/ Svg]

  final case class GraphReqImplications(ord       : Ord,
                                        focus     : ReqId,
                                        filterDead: FilterDead,
                                        colours   : Option[ImpGraphConfig.Colours]) extends WebWorkerCmd[ErrorMsg \/ Svg]

  final case class GraphAllImplications(ord       : Ord,
                                        filterDead: FilterDead,
                                        scope     : Option[Set[ReqId]],
                                        config    : ImpGraphConfig) extends WebWorkerCmd[ErrorMsg \/ Svg]

  final case class GraphInline(dot: String) extends WebWorkerCmd[ErrorMsg \/ Svg]

  // ===================================================================================================================

  val protocolVer = Version.fromInts(2, 0) // Bump this when any of following imports change
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData1._
  import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev1.SavedViewPicklers._
  import shipreq.webapp.member.project.protocol.binary.v2.Rev0._

  implicit val picklerSvg: Pickler[Svg] =
    transformPickler(Svg.apply)(_.content)

  implicit val picklerNoResult: Pickler[NoResult.type] =
    ConstPickler(NoResult)

  private implicit val picklerOrd: Pickler[Ord] =
    transformPickler[Ord, Int](
      i => Option.when(i > 0)(EventOrd.Latest(i)))(
      _.fold(0)(_.value))

  implicit val picklerUpdateProject: Pickler[UpdateProject] =
    transformPickler(UpdateProject.apply)(_.data)

  implicit val picklerErrorMsgOrSvg: Pickler[ErrorMsg \/ Svg] =
    pickleDisj

  private implicit val picklerCssCtx: Pickler[ClientSideStorage.Context] =
    new Pickler[ClientSideStorage.Context] {
      override def pickle(a: ClientSideStorage.Context)(implicit state: PickleState): Unit = {
        state.pickle(a.userId)
        state.pickle(a.projectId)
        state.pickle(a.creator)
      }
      override def unpickle(implicit state: UnpickleState): ClientSideStorage.Context = {
        val uid = state.unpickle[UserId.Public]
        val pid = state.unpickle[ProjectId.Public]
        val c   = state.unpickle[ProjectCreator]
        ClientSideStorage.Context(uid, pid, c)
      }
    }

  private implicit val picklerInit: Pickler[Init] =
    new Pickler[Init] {
      override def pickle(a: Init)(implicit state: PickleState): Unit = {
        state.pickle(a.am)
        state.pickle(a.cssCtx)
        state.pickle(a.encKey)
      }
      override def unpickle(implicit state: UnpickleState): Init = {
        val am     = state.unpickle[AssetManifest]
        val cssCtx = state.unpickle[ClientSideStorage.Context]
        val encKey = state.unpickle[ClientSideProjectEncryptionKey]
        Init(am, cssCtx, encKey)
      }
    }

  private implicit val picklerGraphUseCaseFlow: Pickler[GraphUseCaseFlow] =
    new Pickler[GraphUseCaseFlow] {
      override def pickle(a: GraphUseCaseFlow)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.id)
        state.pickle(a.ctx)
      }
      override def unpickle(implicit state: UnpickleState): GraphUseCaseFlow = {
        val ord = state.unpickle[Ord]
        val id  = state.unpickle[UseCaseId]
        val ctx = state.unpickle[ProjectText.Context]
        GraphUseCaseFlow(ord, id, ctx)
      }
    }

  implicit val picklerGraphReqImplications: Pickler[GraphReqImplications] =
    new Pickler[GraphReqImplications] {
      override def pickle(a: GraphReqImplications)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.focus)
        state.pickle(a.filterDead)
        state.pickle(a.colours)
      }
      override def unpickle(implicit state: UnpickleState): GraphReqImplications = {
        val ord        = state.unpickle[Ord]
        val focus      = state.unpickle[ReqId]
        val filterDead = state.unpickle[FilterDead]
        val colours    = state.unpickle[Option[ImpGraphConfig.Colours]]
        GraphReqImplications(ord, focus, filterDead, colours)
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

  implicit val picklerGraphInline: Pickler[GraphInline] =
    transformPickler(GraphInline.apply)(_.dot)

  implicit val picklerCmd: Pickler[WebWorkerCmd[_]] =
    new Pickler[WebWorkerCmd[_]] {
      private[this] final val KeyInit                 = 0
      private[this] final val KeyUpdateProject        = 1
      private[this] final val KeyGraphAllImplications = 2
      private[this] final val KeyGraphReqImplications = 3
      private[this] final val KeyGraphUseCaseFlow     = 4
      private[this] final val KeyGraphInline          = 5
      private[this] final val KeyClearAndDisableCache = 6
      override def pickle(a: WebWorkerCmd[_])(implicit state: PickleState): Unit =
        a match {
          case b: Init                 => state.enc.writeByte(KeyInit                ); state.pickle(b)
          case b: UpdateProject        => state.enc.writeByte(KeyUpdateProject       ); state.pickle(b)
          case b: GraphAllImplications => state.enc.writeByte(KeyGraphAllImplications); state.pickle(b)
          case b: GraphReqImplications => state.enc.writeByte(KeyGraphReqImplications); state.pickle(b)
          case b: GraphUseCaseFlow     => state.enc.writeByte(KeyGraphUseCaseFlow    ); state.pickle(b)
          case b: GraphInline          => state.enc.writeByte(KeyGraphInline         ); state.pickle(b)
          case ClearAndDisableCache    => state.enc.writeByte(KeyClearAndDisableCache)
        }
      override def unpickle(implicit state: UnpickleState): WebWorkerCmd[_] =
        state.dec.readByte match {
          case KeyInit                 => state.unpickle[Init]
          case KeyUpdateProject        => state.unpickle[UpdateProject]
          case KeyGraphAllImplications => state.unpickle[GraphAllImplications]
          case KeyGraphReqImplications => state.unpickle[GraphReqImplications]
          case KeyGraphUseCaseFlow     => state.unpickle[GraphUseCaseFlow]
          case KeyGraphInline          => state.unpickle[GraphInline]
          case KeyClearAndDisableCache => ClearAndDisableCache
        }
    }
}
