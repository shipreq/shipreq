package shipreq.webapp.member.project.protocol.websocket

import cats.Eq
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.cats_ext.CatsMacros.deriveEq
import japgolly.microlibs.utils.StaticLookupFn
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.protocol.websocket.WebSocketShared
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  final case class WebSocket(projectId: ProjectId.Public, creator: ProjectCreator) extends Protocol.WebSocket.ClientReqServerPush[SafePickler] {
    override val  url    = Urls.ProjectSpaWebSocket.url(projectId, creator)
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = WsReqRes
    override val  req    = WsReqRes.AndReq.protocol
    override val  push   = WebSocket.pushProtocol
  }

  object WebSocket {

    type Push = StateUpdate

    private[WebSocket] val pushProtocol: Protocol.Of[SafePickler, Push] =
      Protocol(Codecs.Push.safePickler)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitAppData(projectData    : Project \/ VerifiedEvent.Seq,
                               projectMetaData: ProjectMetaData,
                               supp           : Supplimentary)

  final case class Supplimentary(rolodex: Rolodex) {
    def ++(s: Supplimentary): Supplimentary =
      Supplimentary(rolodex ++ s.rolodex)
  }

  object Supplimentary {
    val empty: Supplimentary =
      apply(Rolodex.empty)

    implicit def univEq: UnivEq[Supplimentary] = UnivEq.derive
  }

  final case class StateUpdate(events: VerifiedEvent.Seq, supp: Supplimentary) {
    def ++(x: StateUpdate): StateUpdate =
      StateUpdate(events ++ x.events, supp ++ x.supp)
  }

  object StateUpdate {
    val empty: StateUpdate =
      apply(VerifiedEvent.Seq.empty, Supplimentary.empty)

    implicit def eqStateUpdate(implicit e: Eq[VerifiedEvent.Seq]): Eq[StateUpdate] = deriveEq
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // Bump when *any* codec changes happen
  private val wsrrVersion = Version.fromInts(2, 0)

  // When any of the following change, bump wsrrVersion
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import CreateContentCmd.CodecsV4._
  import ManualIssueCmd.CodecsV4._
  import SavedViewCmd.CodecsV4._
  import UpdateAccessCmd.CodecsV1._
  import UpdateConfigCmd.CodecsV2._
  import UpdateContentCmd.CodecsV4._

  private object Codecs {

    private val picklerSupplimentary_v10: Pickler[Supplimentary] = {
      // Bump the above version when any of following changes
      import shipreq.webapp.member.project.protocol.binary.v2.Rev0._
      picklerRolodex.xmap(Supplimentary.apply)(_.rolodex)
    }

    private val picklerStateUpdate_v10: Pickler[StateUpdate] = {
      // Bump the above version when any of following changes
      import shipreq.webapp.member.project.protocol.binary.v2.Rev0._
      @inline implicit def picklerSupp = picklerSupplimentary_v10

      new Pickler[StateUpdate] {
        override def pickle(a: StateUpdate)(implicit state: PickleState): Unit = {
          state.pickle(a.events)
          state.pickle(a.supp)
        }
        override def unpickle(implicit state: UnpickleState): StateUpdate = {
          val events = state.unpickle[VerifiedEvent.Seq]
          val supp   = state.unpickle[Supplimentary]
          StateUpdate(events, supp)
        }
      }
    }

    val safePicklerWsReqResAndReq: SafePickler[WsReqRes.AndReq] = {
      import WsReqRes._

      val pickler: Pickler[AndReq] =
        new Pickler[AndReq] {

          override def pickle(obj: AndReq)(implicit state: PickleState): Unit = {
            state.pickle(obj.reqRes.key)
            state.pickle(obj.req)(obj.reqRes.protocolReq.codec)
          }

          override def unpickle(implicit state: UnpickleState): AndReq = {
            val key = state.unpickle[Int]
            byKey(key) match {
              case Some(p) =>
                val req = state.unpickle(p.protocolReq.codec)
                p.AndReq(req)
              case None =>
                val msg = s"Can't unpickle request: unknown key ($key)"
                throw new RuntimeException(msg)
            }
          }
        }

      pickler
        .asVersion(wsrrVersion)
        .withMagicNumbers(0x1DB44559, 0x53562938)
    }

    object Requests {
      // When any of the following change (import or impls), bump wsrrVersion
      import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData1._
      import shipreq.webapp.member.project.protocol.binary.v1.PostEvents._

      implicit val picklerOptionEventOrdLatest: Pickler[Option[EventOrd.Latest]] =
        transformPickler[Option[EventOrd.Latest], Int](
          i => Option.when(i > 0)(EventOrd.Latest(i)))(
          _.fold(0)(_.value))

      implicit val picklerNonEmptySetEventOrd: Pickler[NonEmptySet[EventOrd]] =
        pickleNES

      implicit val picklerFieldMandatorinessModReq: Pickler[(CustomFieldId, Mandatory)] =
        Tuple2Pickler

      implicit val picklerReqTypeImplicationModReq: Pickler[(CustomReqTypeId, Mandatory)] =
        Tuple2Pickler
    }

    object Responses {
      protected val responseVersion = Version.fromInts(2, 0) // Bump this when any of following imports change
      import shipreq.webapp.member.project.protocol.binary.v2.Rev0._
      @inline private implicit def picklerSupp = picklerSupplimentary_v10
      @inline private implicit def picklerStateUpdate = picklerStateUpdate_v10

      private implicit val picklerInitAppData: Pickler[InitAppData] =
        new Pickler[InitAppData] {
          override def pickle(a: InitAppData)(implicit state: PickleState): Unit = {
            state.pickle(a.projectData)
            state.pickle(a.projectMetaData)
            state.pickle(a.supp)
          }
          override def unpickle(implicit state: UnpickleState): InitAppData = {
            val project         = state.unpickle[Project \/ VerifiedEvent.Seq]
            val projectMetaData = state.unpickle[ProjectMetaData]
            val supp            = state.unpickle[Supplimentary]
            InitAppData(project, projectMetaData, supp)
          }
        }

      private implicit val picklerInitAppRes: Pickler[ErrorMsg \/ InitAppData] =
        pickleDisj

      private implicit val picklerEventResult: Pickler[WsReqRes.EventResult] =
        pickleDisj

      // These SafePicklers below are for responses.
      // We're ditching the magic header because there's not much point; they can trust us.
      // We're keeping a magic footer just in case.

      implicit val safePicklerUnit: SafePickler[Unit] =
        unitPickler.asV1(0) // no magic numbers because no data

      implicit val safePicklerInitAppRes: SafePickler[ErrorMsg \/ InitAppData] =
        picklerInitAppRes
          .asVersion(responseVersion)
          .withMagicNumberFooter(0x8819303B)

      implicit val safePicklerEventResult: SafePickler[WsReqRes.EventResult] =
        picklerEventResult
          .asVersion(responseVersion)
          .withMagicNumberFooter(0x86DA8677)

      implicit val safePicklerStateUpdate: SafePickler[StateUpdate] =
        picklerStateUpdate
          .asVersion(responseVersion)
          .withMagicNumberFooter(0x8473B8AD)
    }

    object Push {
      protected val version = Version.fromInts(2, 0) // Bump this when any of following imports change
      @inline private implicit def picklerStateUpdate = picklerStateUpdate_v10

      private def pickler: Pickler[WebSocket.Push] =
        picklerStateUpdate

      val safePickler: SafePickler[WebSocket.Push] =
        pickler
          .asVersion(version)
          .withMagicNumberFooter(0x06F60C06)
    }
  }

  import Codecs.Requests._
  import Codecs.Responses._

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait WsReqRes extends Protocol.RequestResponse[SafePickler] { self =>
    protected[ProjectSpaProtocols] val key: Int
    protected[ProjectSpaProtocols] val protocolReq: Protocol.Of[Pickler, RequestType]
    val protocolRes: Protocol.Of[SafePickler, ResponseType]
    final val name = getClass.getName.replaceFirst("""^.*\$(.+?)\$?$""", "$1")

    final type AndReq = WsReqRes.AndReq { val reqRes: self.type }
    final def AndReq(r: RequestType): AndReq =
      new WsReqRes.AndReq {
        override val reqRes: self.type = self
        override val req = r
      }

    override final type PreparedRequestType = WsReqRes.AndReq

    override final def prepareSend(r: RequestType): PreparedSend = {
      val req = AndReq(r)
      Protocol.RequestResponse.PreparedSend(req, protocolRes)
    }

    def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]): G[this.type]
  }

  object WsReqRes {
    sealed abstract class Base[Req: Pickler, Res: SafePickler](override final protected[ProjectSpaProtocols] val key: Int) extends WsReqRes {
      override final type RequestType = Req
      override final type ResponseType = Res
      override final protected[ProjectSpaProtocols] val protocolReq = Protocol(implicitly)
      override final val protocolRes = Protocol(implicitly)
    }

    type EventResult = ErrorMsg \/ StateUpdate

    case object InitApp extends Base[Option[EventOrd.Latest], ErrorMsg \/ InitAppData](0) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onInitApp(r)
    }

    case object Reconnect extends Base[Option[EventOrd.Latest], StateUpdate](1) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onReconnect(r)
    }

    case object Sync extends Base[NonEmptySet[EventOrd], Unit](2) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onSync(r)
    }

    case object UpdateConfig extends Base[UpdateConfigCmd, EventResult](3) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateConfig(r)
    }

    case object CreateContent extends Base[CreateContentCmd, EventResult](4) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onCreateContent(r)
    }

    case object UpdateContent extends Base[UpdateContentCmd, EventResult](5) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateContent(r)
    }

    case object ProjectNameSet extends Base[String, EventResult](6) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onProjectNameSet(r)
    }

    case object UpdateSavedViews extends Base[SavedViewCmd, EventResult](7) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateSavedViews(r)
    }

    case object UpdateManualIssues extends Base[ManualIssueCmd, EventResult](8) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateManualIssues(r)
    }

    /** Deprecated in v1.1 */
    case object FieldMandatorinessMod extends Base[Unit, Unit](9) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onFieldMandatorinessMod(r)
    }

    case object ReqTypeImplicationMod extends Base[(CustomReqTypeId, Mandatory), EventResult](10) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onReqTypeImplicationMod(r)
    }

    case object AccessUpdate extends Base[UpdateAccessCmd, EventResult](11) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onAccessUpdate(r)
    }

    implicit def univEq: UnivEq[WsReqRes] = UnivEq.derive
    val values = AdtMacros.adtValues[WsReqRes]
    val byKey = StaticLookupFn.useArrayBy(values.whole)(_.key).toOption

    final case class Fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](
        onInitApp              : F[InitApp              .type] => G[InitApp              .type],
        onReconnect            : F[Reconnect            .type] => G[Reconnect            .type],
        onSync                 : F[Sync                 .type] => G[Sync                 .type],
        onUpdateConfig         : F[UpdateConfig         .type] => G[UpdateConfig         .type],
        onCreateContent        : F[CreateContent        .type] => G[CreateContent        .type],
        onUpdateContent        : F[UpdateContent        .type] => G[UpdateContent        .type],
        onProjectNameSet       : F[ProjectNameSet       .type] => G[ProjectNameSet       .type],
        onUpdateSavedViews     : F[UpdateSavedViews     .type] => G[UpdateSavedViews     .type],
        onUpdateManualIssues   : F[UpdateManualIssues   .type] => G[UpdateManualIssues   .type],
        onFieldMandatorinessMod: F[FieldMandatorinessMod.type] => G[FieldMandatorinessMod.type],
        onReqTypeImplicationMod: F[ReqTypeImplicationMod.type] => G[ReqTypeImplicationMod.type],
        onAccessUpdate         : F[AccessUpdate         .type] => G[AccessUpdate         .type],
        ) { self =>
      @inline def apply(r: WsReqRes)(f: F[r.type]) = r.fold(this)(f)
      def compose[H[_ <: WsReqRes]](h: Fold[G, H]): Fold[F, H] =
        Fold(
          onInitApp               = f => h.onInitApp              (self.onInitApp              (f)),
          onReconnect             = f => h.onReconnect            (self.onReconnect            (f)),
          onSync                  = f => h.onSync                 (self.onSync                 (f)),
          onUpdateConfig          = f => h.onUpdateConfig         (self.onUpdateConfig         (f)),
          onCreateContent         = f => h.onCreateContent        (self.onCreateContent        (f)),
          onUpdateContent         = f => h.onUpdateContent        (self.onUpdateContent        (f)),
          onProjectNameSet        = f => h.onProjectNameSet       (self.onProjectNameSet       (f)),
          onUpdateSavedViews      = f => h.onUpdateSavedViews     (self.onUpdateSavedViews     (f)),
          onUpdateManualIssues    = f => h.onUpdateManualIssues   (self.onUpdateManualIssues   (f)),
          onFieldMandatorinessMod = f => h.onFieldMandatorinessMod(self.onFieldMandatorinessMod(f)),
          onReqTypeImplicationMod = f => h.onReqTypeImplicationMod(self.onReqTypeImplicationMod(f)),
          onAccessUpdate          = f => h.onAccessUpdate         (self.onAccessUpdate         (f)),
        )
    }

    trait AndReq {
      val reqRes: WsReqRes
      val req: reqRes.RequestType
      override def toString = s"$reqRes.AndReq($req)"
      override def hashCode() = reqRes.## * 31 + req.##
      override def equals(obj: Any) = obj match {
        case x: AndReq => (reqRes == x.reqRes) && (req == x.req)
        case _         => false
      }
    }

    object AndReq {
      private[ProjectSpaProtocols] val protocol: Protocol.Of[SafePickler, AndReq] =
        Protocol(Codecs.safePicklerWsReqResAndReq)
    }
  }
}
