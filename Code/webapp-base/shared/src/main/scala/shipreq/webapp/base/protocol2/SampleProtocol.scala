package shipreq.webapp.base.protocol2

import boopickle.DefaultBasic._
import japgolly.microlibs.adt_macros.AdtMacros
import java.nio.ByteBuffer
import shipreq.webapp.base.Urls

object SampleProtocol {

  sealed trait ReqRes extends Protocol.RequestResponse[Pickler] { self =>
    protected[SampleProtocol] val protocolReq: Protocol.Of[Pickler, RequestType]
    protected[SampleProtocol] val protocolRes: Protocol.Of[Pickler, ResponseType]
    protected[SampleProtocol] val key: Int

    override final type PreparedRequestType = ReqRes.AndReq

    final type AndReq = ReqRes.AndReq { val reqRes: self.type }
    final def AndReq(r: RequestType): AndReq =
      new ReqRes.AndReq {
        override val reqRes: self.type = self
        override val req = r
      }

    override final def prepareSend(r: RequestType): PreparedSend = {
      val req = AndReq(r)
      val protocolAndReq = ReqRes.AndReq.protocol.andValue(req)
      Protocol.RequestResponse.PreparedSend(protocolAndReq, protocolRes)
    }

    def fold[F[_ <: ReqRes], G[_ <: ReqRes]](f: ReqRes.Fold[F, G])(r: F[this.type]): G[this.type]
  }

  object ReqRes {
    sealed abstract class Help[Req: Pickler, Res: Pickler](override final protected[SampleProtocol] val key: Int) extends ReqRes {
      override final type RequestType = Req
      override final type ResponseType = Res
      override final protected[SampleProtocol] val protocolReq = Protocol(implicitly)
      override final protected[SampleProtocol] val protocolRes = Protocol(implicitly)
    }

    case object IsEven extends Help[Long, Boolean](1) {
      override def fold[F[_ <: ReqRes], G[_ <: ReqRes]](f: ReqRes.Fold[F, G])(r: F[this.type]) = f.isEven(r)
    }
    case object Xs extends Help[Int, String](2) {
      override def fold[F[_ <: ReqRes], G[_ <: ReqRes]](f: ReqRes.Fold[F, G])(r: F[this.type]) = f.xs(r)
    }

    // =================================================================================================================

    val values = AdtMacros.adtValues[ReqRes]
    val byKey: Int => ReqRes = values.whole.map(r => r.key -> r).toMap.apply // TODO assert uniq & use Array

    // =================================================================================================================

    final case class Fold[F[_ <: ReqRes], G[_ <: ReqRes]](
        isEven: F[IsEven.type] => G[IsEven.type],
        xs    : F[Xs    .type] => G[Xs    .type]) { self =>
      def compose[H[_ <: ReqRes]](h: Fold[G, H]): Fold[F, H] =
        Fold(
          isEven = f => h.isEven(self.isEven(f)),
          xs     = f => h.xs    (self.xs    (f)))
    }

    trait AndReq {
      val reqRes: ReqRes
      val req: reqRes.RequestType
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
            val p = byKey(key)
            val req = state.unpickle(p.protocolReq.codec)
            p.AndReq(req)
          }
        }
      )
    }
  }

  object WS extends Protocol.WebSocket.ClientReqServerPush[Pickler] {
    override type ReqId        = Int
    override type Req          = ReqRes.AndReq
    override type ReqRes       = SampleProtocol.ReqRes
    override type Push         = String
    override val  url          = Urls.projectSpaWebSocket
    override val  protocolReq  = ReqRes.AndReq.protocol
    override val  protocolPush = Protocol[Pickler, Push](implicitly)
  }

  type F[R <: ReqRes] = R#RequestType
  type G[R <: ReqRes] = R#ResponseType
  val respondFold = ReqRes.Fold[F, G](
    isEven = r => (r&1)==0,
    xs     = r => "x" * r,
  )
  def respond(r: ReqRes.AndReq): ByteBuffer = {
    val res = r.reqRes.fold(respondFold)(r.req)
    val bb = Pickle.intoBytes(res)(implicitly, r.reqRes.protocolRes.codec)
    bb
  }

}
