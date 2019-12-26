package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import japgolly.univeq.UnivEq
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._

object WebSocketShared {

  final case class ReqId(value: Int)

  object ReqId {
    implicit def univEq: UnivEq[ReqId] = UnivEq.derive
    implicit val pickler: Pickler[ReqId] = intPickler.xmap(apply)(_.value)
  }

  final case class CloseCode(value: Int) extends AnyVal

  object CloseCode {
    /** Runtime exception occurred */
    val UnhandledException = apply(4500)

    /** Error sending response */
    val RespondException = apply(4501)
  }

  // ===================================================================================================================
  // Client to Server

  type ClientToServer[Req] = (ReqId, Req)

  def protocolCS[Req](implicit sp: SafePickler[Req]): Protocol.Of[SafePickler, ClientToServer[Req]] =
    Protocol(sp.map(Tuple2Pickler(ReqId.pickler, _)))

  // ===================================================================================================================
  // Server to Client

  type ServerToClient[Push] = Push \/ (ReqId, Protocol.AndValue[SafePickler])

  def protocolSC[Push](responseUnpickler: ReqId => Protocol[SafePickler])
                      (implicit pushCodec: SafePickler[Push]): Protocol.Of[SafePickler, ServerToClient[Push]] = {

    val pickler: Pickler[ServerToClient[Push]] =
      new Pickler[ServerToClient[Push]] {

        override def pickle(obj: ServerToClient[Push])(implicit state: PickleState): Unit =
          obj match {
            case \/-((i, pv)) =>
              state.enc.writeLong(i.value.toLong << 1)
              pv.codec.embeddedWrite(pv.value)
            case -\/(push) =>
              state.enc.writeLong(1)
              pushCodec.embeddedWrite(push)
          }

        override def unpickle(implicit state: UnpickleState): ServerToClient[Push] = {
          val header = state.dec.readLong
          if (header == 1)
            -\/(pushCodec.embeddedRead)
          else {
            val reqId = ReqId((header >> 1).toInt)
            val protocol = responseUnpickler(reqId)
            val pav: Protocol.AndValue[SafePickler] =
              if (protocol eq null)
                null
              else {
                val v = protocol.codec.embeddedRead
                protocol.andValue(v)
              }
            \/-((reqId, pav))
          }
        }
      }

    Protocol(pickler.asV10)
  }
}
