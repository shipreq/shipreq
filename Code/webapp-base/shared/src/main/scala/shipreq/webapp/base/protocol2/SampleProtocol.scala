package shipreq.webapp.base.protocol2

import boopickle.DefaultBasic._
import japgolly.microlibs.adt_macros.AdtMacros
import java.nio.ByteBuffer
import scalaz.~>

object SampleProtocol {

  sealed trait ReqRes extends Protocol.RequestResponse[Pickler] { self =>
    protected[SampleProtocol] val protocolReq: Protocol.Of[Pickler, RequestType]
    protected[SampleProtocol] val protocolRes: Protocol.Of[Pickler, ResponseType]
    protected[SampleProtocol] val key: Int

//    val request : Protocol[Pickler]
//    val response: Protocol[Pickler]
//    val key: Int
//    override type RequestType = Int
//    override type ResponseType = Boolean

    override final type PreparedRequestType = ReqRes.AndReq

    final type AndReq = ReqRes.AndReq { val reqRes: self.type }
    final def AndReq(r: RequestType): AndReq = ???

    override final def prepareSend(r: RequestType): PreparedSend = {
      val req = AndReq(r)
      val protocolAndReq = ReqRes.AndReq.protocol.andValue(req)
      Protocol.RequestResponse.PreparedSend(protocolAndReq, protocolRes)
    }

//    def foldReq[A](f: ReqRes.AndReq.Fold[A], r: RequestType): A
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
//      override def foldReq[A](f: ReqRes.AndReq.Fold[A], r: RequestType): A = f.isEven(r)
      override def fold[F[_ <: ReqRes], G[_ <: ReqRes]](f: ReqRes.Fold[F, G])(r: F[this.type]) = f.isEven(r)
    }
    case object Xs extends Help[Int, String](2) {
//      override def foldReq[A](f: ReqRes.AndReq.Fold[A], r: RequestType): A = f.xs(r)
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
//      def map[H[_]](h: G ~> H): Fold[F, H] =
//        Fold(
//          isEven = f => h(self.isEven(f)),
//          xs     = f => h(self.xs    (f)))
    }
    object Fold {
//      def uniform[F[+_ <: ReqRes], G[+_ <: ReqRes]](f: F[_ <: ReqRes] => G[_ <: ReqRes]): Fold[F, G] =
//        apply[F, G](f, f)
    }

    trait AndReq {
      val reqRes: ReqRes
      val req: reqRes.RequestType

//      final def apply[A](f: AndReq.Fold[A]): A = reqRes.foldReq(f, req)
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

//      final case class Fold[A](isEven: IsEven.RequestType => A, xs: Xs.RequestType => A)
    }
  }

  type Push = String
  val push = Protocol[Pickler, Push](implicitly)
  val WS = Protocol.WebSocket.ClientReqServerPush[Pickler, Int, ReqRes.AndReq, ReqRes, Push](push)


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
