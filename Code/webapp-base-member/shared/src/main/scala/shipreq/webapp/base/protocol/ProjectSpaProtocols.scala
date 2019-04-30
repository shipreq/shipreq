package shipreq.webapp.base.protocol

import boopickle.{PickleState, Pickler, UnpickleState}
import japgolly.microlibs.adt_macros.AdtMacros
import scalaz.\/
import shipreq.base.util.{ErrorMsg, StaticLookupFn}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.user._
import shipreq.webapp.base.Urls
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecMemberData._
import BinCodecUser._
import BinCodecEvents._

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  final case class InitPageData(username: Username,
                                projectId: ProjectId.Public,
                                projectName: Project.Name)

  implicit val picklerInitPageData = pickleCaseClass[InitPageData]

  final val EntryPointName = "P"
  val EntryPoint = ClientSideProc[InitPageData](EntryPointName)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class WebSocket(projectId: ProjectId.Public) extends Protocol.WebSocket.ClientReqServerPush[Pickler] {
    override val  url    = Urls.ProjectSpaWebSocket.url(projectId)
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = WsReqRes
    override val  req    = WsReqRes.AndReq.protocol
    override val  push   = WebSocket.pushProtocol
  }

  object WebSocket {
    type Push = VerifiedEvent.NonEmptySeq
    private[WebSocket] val pushProtocol = Protocol[Pickler, Push](implicitly)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitAppData(project        : ProjectAndOrd,
                               projectMetaData: ProjectMetaData)

  implicit val picklerInitAppData = pickleCaseClass[InitAppData]

  sealed trait WsReqRes extends Protocol.RequestResponse[Pickler] { self =>
    protected val protocolReq: Protocol.Of[Pickler, RequestType]
    val protocolRes: Protocol.Of[Pickler, ResponseType]
    protected val key: Int

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
    sealed abstract class Base[Req: Pickler, Res: Pickler](override final protected val key: Int) extends WsReqRes {
      override final type RequestType = Req
      override final type ResponseType = Res
      override final protected val protocolReq = Protocol(implicitly)
      override final val protocolRes = Protocol(implicitly)
    }

    type EventResult = ErrorMsg \/ VerifiedEvent.Seq

    case object InitApp extends Base[Unit, ErrorMsg \/ InitAppData](0) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onInitApp(r)
    }

    case object CreateContent extends Base[CreateContentCmd, EventResult](1) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onCreateContent(r)
    }

    case object UpdateContent extends Base[UpdateContentCmd, EventResult](2) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateContent(r)
    }

    case object ProjectNameSet extends Base[String, EventResult](3) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onProjectNameSet(r)
    }

    case object UpdateSavedViews extends Base[SavedViewCmd, EventResult](4) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onUpdateSavedViews(r)
    }

    case object FieldMandatorinessMod extends Base[(CustomFieldId, Mandatory), EventResult](5) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onFieldMandatorinessMod(r)
    }

    case object ReqTypeImplicationMod extends Base[(CustomReqTypeId, ImplicationRequired), EventResult](6) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onReqTypeImplicationMod(r)
    }

    case object CustomIssueTypeCrud extends Base[CrudAction[CustomIssueTypeId, (HashRefKey, Option[String])], EventResult](7) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onCustomIssueTypeCrud(r)
    }

    case object CustomReqTypeCrud extends Base[CrudAction[CustomReqTypeId, (ReqType.Mnemonic, String, ImplicationRequired)], EventResult](8) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onCustomReqTypeCrud(r)
    }

    case object FieldMod extends Base[FieldCrud.CfgAction, EventResult](9) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onFieldMod(r)
    }

    case object TagMod extends Base[TagCrud.Action, EventResult](10) {
      override def fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](f: WsReqRes.Fold[F, G])(r: F[this.type]) = f.onTagMod(r)
    }

    val values = AdtMacros.adtValues[WsReqRes]
    val byKey = StaticLookupFn.arrayBy(values.whole)(_.key)

    final case class Fold[F[_ <: WsReqRes], G[_ <: WsReqRes]](
        onInitApp              : F[InitApp              .type] => G[InitApp              .type],
        onCreateContent        : F[CreateContent        .type] => G[CreateContent        .type],
        onUpdateContent        : F[UpdateContent        .type] => G[UpdateContent        .type],
        onProjectNameSet       : F[ProjectNameSet       .type] => G[ProjectNameSet       .type],
        onUpdateSavedViews     : F[UpdateSavedViews     .type] => G[UpdateSavedViews     .type],
        onFieldMandatorinessMod: F[FieldMandatorinessMod.type] => G[FieldMandatorinessMod.type],
        onReqTypeImplicationMod: F[ReqTypeImplicationMod.type] => G[ReqTypeImplicationMod.type],
        onCustomIssueTypeCrud  : F[CustomIssueTypeCrud  .type] => G[CustomIssueTypeCrud  .type],
        onCustomReqTypeCrud    : F[CustomReqTypeCrud    .type] => G[CustomReqTypeCrud    .type],
        onFieldMod             : F[FieldMod             .type] => G[FieldMod             .type],
        onTagMod               : F[TagMod               .type] => G[TagMod               .type],
        ) { self =>
      def compose[H[_ <: WsReqRes]](h: Fold[G, H]): Fold[F, H] =
        Fold(
          onInitApp               = f => h.onInitApp              (self.onInitApp              (f)),
          onCreateContent         = f => h.onCreateContent        (self.onCreateContent        (f)),
          onUpdateContent         = f => h.onUpdateContent        (self.onUpdateContent        (f)),
          onProjectNameSet        = f => h.onProjectNameSet       (self.onProjectNameSet       (f)),
          onUpdateSavedViews      = f => h.onUpdateSavedViews     (self.onUpdateSavedViews     (f)),
          onFieldMandatorinessMod = f => h.onFieldMandatorinessMod(self.onFieldMandatorinessMod(f)),
          onReqTypeImplicationMod = f => h.onReqTypeImplicationMod(self.onReqTypeImplicationMod(f)),
          onCustomIssueTypeCrud   = f => h.onCustomIssueTypeCrud  (self.onCustomIssueTypeCrud  (f)),
          onCustomReqTypeCrud     = f => h.onCustomReqTypeCrud    (self.onCustomReqTypeCrud    (f)),
          onFieldMod              = f => h.onFieldMod             (self.onFieldMod             (f)),
          onTagMod                = f => h.onTagMod               (self.onTagMod               (f)),
        )
    }

    trait AndReq {
      val reqRes: WsReqRes
      val req: reqRes.RequestType
      override def toString = s"$reqRes.AndReq($req)"
    }

    object AndReq {
      val protocol: Protocol.Of[Pickler, AndReq] = Protocol(
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
      )
    }
  }
}
