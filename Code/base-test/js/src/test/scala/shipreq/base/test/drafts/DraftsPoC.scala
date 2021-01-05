package shipreq.base.test.drafts

import japgolly.microlibs.utils.Ref
import scala.scalajs.js
import shipreq.base.test.BaseTestUtil._
import utest.{assert => _, _}
import shipreq.webapp.member.jsfacade.Yjs

object DraftsPoC extends TestSuite {

  type Delta = Yjs.Update

  // Drafts are saved (per user) locally and remotely
  // start, edit, commit, abort, recv, send, resume

  // ===================================================================================================================

  sealed trait NetworkNode {
    val id: String
    protected val network: Network
    def recv(from: NetworkNode, msg: Msg): Unit

    protected final def debug(msg: => String): Unit = {
      println(s"[$id] $msg")
    }

    final def send(msg: Msg, to: NetworkNode): Unit =
      network.send(this, msg, to)

    final def recvAll(): Unit =
      network.flushTo(this)
  }

  sealed trait Msg
  object Msg {
    final case class InitDeltas(deltas: NonEmptyVector[Delta]) extends Msg
    final case class NewDelta(delta: Delta) extends Msg
  }

  final class Network {
    private val nodes = collection.mutable.Map.empty[String, NetworkNode]
    private val pendingByTarget = collection.mutable.Map.empty[String, js.Array[(NetworkNode, Msg)]]
    private var inFlight = 0
    private var nodeArray = Array.empty[NetworkNode]
    private val rng = new java.util.Random

    def register(n: NetworkNode): Unit = {
      nodes.update(n.id, n)
      pendingByTarget.update(n.id, new js.Array)
      nodeArray = nodes.values.toArray
    }

    def send(from: NetworkNode, msg: Msg, to: NetworkNode): Unit = {
      val pipe = pendingByTarget.get(to.id).getOrThrow(s"Node ${to.id} not registered.")
      pipe.push((from, msg))
      inFlight += 1
    }

    def recvNext(n: NetworkNode): Boolean = {
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

    def flushTo(n: NetworkNode): Unit =
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

  // ===================================================================================================================

  final case class Saved(ord: Int, value: String)

  final class Client(protected val network: Network, val id: String) extends NetworkNode {
    network.register(this)

//    var saved = Option.empty[Saved]

    private var editState = Option.empty[Yjs.Doc]
    private var remote = Option.empty[Yjs.Doc]

    def startEditing(): Unit = {
      assert(editState.isEmpty, "Already editing")
      editState = Some(new Yjs.Doc)
    }

    def editValue(): Option[String] =
      editState.map(_.getText().toText())

    private def doc(): Yjs.Doc =
      editState.getOrThrow("Editor closed")

    private def text(): Yjs.YText =
      doc().getText()

    def length: Int =
      text().length

    def insert(index: Int, content: String): Unit =
      text().insert(index, content)

    def delete(index: Int, length: Int): Unit =
      text().delete(index, length)

    def append(content: String): Unit =
      insert(length, content)

    def replace(from: String, to: String): Unit = {
      val s = text().toText()
      val i = s.indexOf(from)
      assert(i >= 0, s"'$from' not found in '$s'")
      delete(i, from.length)
      insert(i, to)
    }

    private def addRemoteDelta(delta: Delta): Unit = {
      val r = remote.getOrElse {
        val r = new Yjs.Doc
        remote = Some(r)
        r
      }
      Yjs.applyUpdate(r, delta)
      editState match {
        case Some(e) => Yjs.applyUpdate(e, delta)
        case None    =>
          val e = new Yjs.Doc
          Yjs.applyUpdate(e, Yjs.encodeStateAsUpdate(r))
          editState = Some(e)
      }
    }

    override def recv(from: NetworkNode, msg: Msg): Unit =
      msg match {

        case Msg.InitDeltas(ds) =>
          for (d <- ds)
            addRemoteDelta(d)

        case Msg.NewDelta(d) =>
          addRemoteDelta(d)

        case _ =>
          throw new RuntimeException(s"Client $id doesn't support network msg: $msg")
      }

    def -->(s: Server): Unit = {
      val e = doc()
      val d: Delta =
        remote match {
          case None    => Yjs.encodeStateAsUpdate(e)
          case Some(r) => Yjs.encodeStateAsUpdate(e, Yjs.encodeStateVector(r))
        }
      send(Msg.NewDelta(d), s)
    }
  }

  // ===================================================================================================================

  final class Server(protected val network: Network) extends NetworkNode {
    override val id = "s"
    network.register(this)

    var store = Option.empty[RemoteStorage]
    var connected = Set.empty[Ref[NetworkNode]]

    def connect(nodes: NetworkNode*): Unit = {
      for (n <- nodes) {
        connected += Ref(n)
        for (s <- store) {
          send(Msg.InitDeltas(s.deltas), n)
        }
      }
    }

    def +=(d: Delta): Unit = {
//      debug(s"Storing new delta (${d.length} bytes)")
      val newStore: RemoteStorage =
        store match {
          case Some(s) => s :+ d
          case None    => RemoteStorage(NonEmptyVector.one(d))
        }
      store = Some(newStore)
    }

    override def recv(from: NetworkNode, msg: Msg): Unit =
      msg match {

        case Msg.NewDelta(d) =>
          this += d
          for (n <- (connected - Ref(from))) {
            send(msg, n.value)
          }

        case _ =>
          throw new RuntimeException("Server doesn't support network msg: " + msg)
      }
  }

  final case class RemoteStorage(deltas: NonEmptyVector[Delta]) {
    def :+(d: Delta): RemoteStorage =
      copy(deltas :+ d)
  }

  // ===================================================================================================================

  override def tests = Tests {

    "basic" - {

      "sync" - {
        val n = new Network
        val s = new Server(n)
        val a = new Client(n, "a")
        val b = new Client(n, "b")
        s.connect(a, b)

        a.startEditing()
        a.append("this is my draft")
        a --> s
        n.flushAll()
        assertEq(b.editValue(), Some("this is my draft"))

        val c = new Client(n, "c")
        s.connect(c)
        n.flushAll()
        assertEq(c.editValue(), Some("this is my draft"))

        b.replace("t", "T")
        c.replace("my", "our")
        assertEq(b.editValue(), Some("This is my draft"))
        assertEq(c.editValue(), Some("this is our draft"))
        b --> s
        c --> s
        n.flushAll()

        assertEq(a.editValue(), Some("This is our draft"))
        assertEq(b.editValue(), Some("This is our draft"))
        assertEq(c.editValue(), Some("This is our draft"))
      }
    }

  }
}
