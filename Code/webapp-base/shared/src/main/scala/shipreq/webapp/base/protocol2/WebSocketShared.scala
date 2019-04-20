package shipreq.webapp.base.protocol2

import boopickle.{PickleState, Pickler, UnpickleState}
import japgolly.univeq.UnivEq
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.protocol.BinCodecGeneric.{Tuple2Pickler, intPickler}

object WebSocketShared {

  final case class ReqId(value: Int)

  object ReqId {
    implicit def univEq: UnivEq[ReqId] = UnivEq.derive
    implicit val pickler: Pickler[ReqId] = intPickler.xmap(apply)(_.value)
  }

  // ===================================================================================================================
  // Client to Server

  type ClientToServer[Req] = (ReqId, Req)

  def protocolCS[Req: Pickler]: Pickler[ClientToServer[Req]] =
    Tuple2Pickler

  // ===================================================================================================================
  // Server to Client

  type ServerToClient[Push] = Push \/ (ReqId, Protocol.AndValue[Pickler])

  def protocolSC[Push: Pickler](responseUnpickler: ReqId => Protocol[Pickler]): Pickler[ServerToClient[Push]] =
    new Pickler[ServerToClient[Push]] {

      override def pickle(obj: ServerToClient[Push])(implicit state: PickleState): Unit =
        obj match {
          case \/-((i, pv)) =>
            state.enc.writeLong(i.value.toLong << 1)
            pv.codec.pickle(pv.value)
          case -\/(push) =>
            state.enc.writeLong(1)
            state.pickle(push)
        }

      override def unpickle(implicit state: UnpickleState): ServerToClient[Push] = {
        val header = state.dec.readLong
        if (header == 1)
          -\/(state.unpickle[Push])
        else {
          val reqId = ReqId((header >> 1).toInt)
          val protocol = responseUnpickler(reqId)
          val pav: Protocol.AndValue[Pickler] =
            if (protocol eq null)
              null
            else {
              val v = state.unpickle(protocol.codec)
              protocol.andValue(v)
            }
          \/-((reqId, pav))
        }
      }
    }
}
