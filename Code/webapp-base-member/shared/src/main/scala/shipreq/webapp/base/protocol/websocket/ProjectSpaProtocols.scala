package shipreq.webapp.base.protocol.websocket

import boopickle.DefaultBasic._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.utils.StaticLookupFn
import japgolly.univeq.UnivEq
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  final case class WebSocket(projectId: ProjectId.Public) extends Protocol.WebSocket.ClientReqServerPush[SafePickler] {
    override val  url    = Urls.ProjectSpaWebSocket.url(projectId)
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = WsReqRes
    override val  req    = WsReqRes.AndReq.protocol
    override val  push   = WebSocket.pushProtocol
  }

  object WebSocket {
    type Push = VerifiedEvent.NonEmptySeq

    private[WebSocket] val pushProtocol: Protocol.Of[SafePickler, Push] =
      Protocol(Codecs.safePicklerVerifiedEventNonEmptySeq)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // When any of these change, bump the minor version of safePicklerWsReqResAndReq
  import CreateContentCmd.CodecsV1._
  import ManualIssueCmd.CodecsV1._
  import SavedViewCmd.CodecsV1._
  import UpdateConfigCmd.CodecsV1._
  import UpdateContentCmd.CodecsV1._

  private object Codecs {
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
        .asV1(2)
        .withMagicNumbers(0x1DB44559, 0x53562938)
    }

    // When any of these change, bump responseVersion
    import boopickle.DefaultBasic.unitPickler
    import shipreq.webapp.base.protocol.binary.v1.BaseData._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._
    import shipreq.webapp.base.protocol.binary.v1.PostEvents._
    import shipreq.webapp.base.protocol.binary.v1.Rev2._

    protected final val responseVersion = 2

    implicit val picklerInitAppData: Pickler[InitAppData] =
      new Pickler[InitAppData] {
        override def pickle(a: InitAppData)(implicit state: PickleState): Unit = {
          state.pickle(a.project)
          state.pickle(a.projectMetaData)
        }
        override def unpickle(implicit state: UnpickleState): InitAppData = {
          val project         = state.unpickle[ProjectAndOrd]
          val projectMetaData = state.unpickle[ProjectMetaData]
          InitAppData(project, projectMetaData)
        }
      }

    implicit val picklerInitAppRes: Pickler[ErrorMsg \/ InitAppData] =
      pickleDisj

    implicit val picklerOptionEventOrdLatest: Pickler[Option[EventOrd.Latest]] =
      optionPickler

    implicit val picklerNonEmptySetEventOrd: Pickler[NonEmptySet[EventOrd]] =
      pickleNES

    implicit val picklerFieldMandatorinessModReq: Pickler[(CustomFieldId, Mandatory)] =
      Tuple2Pickler

    implicit val picklerReqTypeImplicationModReq: Pickler[(CustomReqTypeId, Mandatory)] =
      Tuple2Pickler

    implicit val picklerEventResult: Pickler[WsReqRes.EventResult] =
      pickleDisj

    // These SafePicklers below are for responses.
    // We're ditching the magic header because there's not much point; they can trust us.
    // We're keeping a magic footer just in case.

    implicit val safePicklerUnit: SafePickler[Unit] =
      unitPickler.asV1(0) // no magic numbers because no data

    implicit val safePicklerInitAppRes: SafePickler[ErrorMsg \/ InitAppData] =
      picklerInitAppRes
        .asV1(responseVersion)
        .withMagicNumberFooter(0x8819303B)

    implicit val safePicklerEventResult: SafePickler[WsReqRes.EventResult] =
      picklerEventResult
        .asV1(responseVersion)
        .withMagicNumberFooter(0x86DA8677)

    implicit val safePicklerVerifiedEventSeq: SafePickler[VerifiedEvent.Seq] =
      picklerVerifiedEventSeq
        .asV1(responseVersion)
        .withMagicNumberFooter(0x85651C09)

    val safePicklerVerifiedEventNonEmptySeq: SafePickler[VerifiedEvent.NonEmptySeq] =
      picklerVerifiedEventNonEmptySeq
        .asV1(responseVersion)
        .withMagicNumberFooter(0x06F60C06)
  }

  import Codecs._

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitAppData(project        : ProjectAndOrd,
                               projectMetaData: ProjectMetaData)

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

    type EventResult = ErrorMsg \/ VerifiedEvent.Seq

    case object InitApp extends Base[Unit, ErrorMsg \/ InitAppData](0) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onInitApp(r)
    }

    case object Reconnect extends Base[Option[EventOrd.Latest], VerifiedEvent.Seq](1) {
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
