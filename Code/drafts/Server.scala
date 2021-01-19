package shipreq.base.test.drafts

import japgolly.microlibs.utils.Ref
import scala.scalajs.js
import Yjs2.Update
import Network.{Msg, Node}

final case class Event(ord: Int, value: String)

final class Server(protected val network: Network) extends Node {
  override val id = "s"
  network.register(this)

  var connected = Set.empty[Ref[Node]]
  var drafts = Option.empty[DraftStream[Any]]
  var events = new js.Array[Event]

  def connect(nodes: Client*): Unit = {
    for (n <- nodes) {
      connected += Ref(n)
      n.recvRemoteStateOnConnect(drafts)
      events.lastOption.foreach(n.recvEvent)
    }
  }

  def addEvent(e: Event): Unit = {
    assert(
      events.isEmpty || events.last.ord == e.ord - 1,
      s"Event $e doesn't follow ${events.last}")
    events.push(e)
  }

  override def recv(from: Node, msg: Msg): Unit =
    msg match {

//      case Msg.NewDelta(baseOrd, d) =>
//        drafts match {
//          case Some(s) if s.baseOrd != baseOrd =>
//            send(Msg.RejectDueToBaseMismatch(s.baseOrd, s.deltas), from)
//
//          case _ =>
//            // debug(s"Storing new delta (${d.value.length} bytes)")
//            val newStore: RemoteDraftStore =
//              drafts match {
//                case Some(s) => s :+ d
//                case None    => RemoteDraftStore(baseOrd, NonEmptyVector.one(d))
//              }
//            drafts = Some(newStore)
//            for (n <- connected - Ref(from)) {
//              send(msg, n.value)
//            }
//        }

      case _ =>
        throw new RuntimeException("Server doesn't support network msg: " + msg)
    }
}
