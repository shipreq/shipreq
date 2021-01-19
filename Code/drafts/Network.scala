package shipreq.base.test.drafts

import scala.scalajs.js
import shipreq.base.test.BaseTestUtil._
import Yjs2.Update

object Network {

  trait Node {
    val id: String
    protected val network: Network
    def recv(from: Node, msg: Msg): Unit

    protected final def debug(msg: => String): Unit = {
      println(s"[$id] $msg")
    }

    final def send(msg: Msg, to: Node): Unit =
      network.send(this, msg, to)

    final def recvAll(): Unit =
      network.flushTo(this)
  }

  sealed trait Msg
  object Msg {
    //    final case class InitDeltas(deltas: NonEmptyVector[Delta]) extends Msg
    final case class NewDelta(baseOrd: Int, delta: Update) extends Msg
    final case class RejectDueToBaseMismatch(baseOrd: Int, deltas: NonEmptyVector[Update]) extends Msg
  }
}

final class Network {
  import Network._
  
  private val nodes = collection.mutable.Map.empty[String, Node]
  private val pendingByTarget = collection.mutable.Map.empty[String, js.Array[(Node, Msg)]]
  private var inFlight = 0
  private var nodeArray = Array.empty[Node]
  private val rng = new java.util.Random

  def register(n: Node): Unit = {
    nodes.update(n.id, n)
    pendingByTarget.update(n.id, new js.Array)
    nodeArray = nodes.values.toArray
  }

  def send(from: Node, msg: Msg, to: Node): Unit = {
    val pipe = pendingByTarget.get(to.id).getOrThrow(s"Node ${to.id} not registered.")
    pipe.push((from, msg))
    inFlight += 1
  }

  def recvNext(n: Node): Boolean = {
    val pipe = pendingByTarget(n.id)
    if (pipe.isEmpty)
      false
    else {
      val (from, msg) = pipe.shift()
      inFlight -= 1
      n.recv(from, msg)
      true
    }
  }

  def flushTo(n: Node): Unit =
    while(recvNext(n)) {}

  private def shuffleNodeArray(): Unit = {
    val a = nodeArray

    def swap(i1: Int, i2: Int): Unit = {
      val tmp = a(i1)
      a(i1) = a(i2)
      a(i2) = tmp
    }

    for (n <- a.length to 2 by -1) {
      val k = rng.nextInt(n)
      swap(n - 1, k)
    }
  }

  def flushAll(): Unit = {
    while (inFlight != 0) {
      shuffleNodeArray()
      var i = nodeArray.length
      while (i > 0) {
        i -= 1
        val n = nodeArray(i)
        if (recvNext(n))
          i = 0
      }
    }
  }

}

