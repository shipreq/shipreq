package shipreq.base.test.drafts

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.member.jsfacade.Yjs
import Yjs2._
import Network.{Msg, Node}
import Client._
import Yjs.Doc

object Client {
  final case class DocWithBase(baseOrd: Int, doc: Doc)

  def initDoc(event: Option[Event]): Doc = {
    val d = new Doc
    for (e <- event) {
      val initDelta = newUpdate(clientId = e.ord)(_.getText().insert(0, e.value))
      d += initDelta
      val sv = d.toStateVector
      d.getMap("svs").set(e.ord.toString, sv.value)
    }
    d
  }

  final class MutableDraftStream {
//    private var s = Option.empty[DraftStream]

//    def init(event: Option[Event]): Doc = {
//      assert(s.isEmpty, "DraftStream already exists")
//      val doc = Client.initDoc(event)
//      val evOrd = event.fold(0)(_.ord)
//      val baseOrd = evOrd
//      DraftStream.Row(delta = doc.)
//      doc
//    }
  }
}

final class Client(protected val network: Network, val id: String) extends Node {

  network.register(this)

  //    var saved = Option.empty[Saved]
//  private val mapName = "m"

  private var forceEditorOpen = false
  private var editing = false
  var latestEvent = Option.empty[Event]
  var editState = Option.empty[DocWithBase]
  var remote = Option.empty[DocWithBase]

  private def doc(): Doc =
    editState.getOrThrow("Editor closed").doc

  private def text(): Yjs.YText =
    doc().getText()

//  private def map(): Yjs.YMap =
//    doc().getMap(mapName)

  def editValue(): Option[String] =
    Option.when(editing)(text().strValue())

  def length: Int =
    text().length

  def insert(index: Int, content: String): Unit =
    text().insert(index, content)

  def delete(index: Int, length: Int): Unit =
    text().delete(index, length)

  def append(content: String): Unit =
    insert(length, content)

  def replaceFirst(from: String, to: String): Unit =
    text().replaceFirst(from, to)

  def startEditing(): Unit = {
    assert(!editing, "Already editing")
    editing = true
    forceEditorOpen = true
    if (editState.isEmpty) {
      val baseOrd = latestEvent.fold(0)(_.ord)
      val d = Client.initDoc(latestEvent)
      editState = Some(DocWithBase(baseOrd, d))
    }
  }

  def abort(): Unit = {
    assert(editing, "Editor closed")
    // TODO ignores initial value
    val d = doc()
    d.itemsFilter(_ => false)
    if (d.itemsForall(_.deleted)) {
      editing = false
    }
    forceEditorOpen = false
  }

  def recvEvent(e: Event): Unit =
    if (latestEvent.forall(e.ord > _.ord)) {
      latestEvent = Some(e)
    }

  override def recv(from: Node, msg: Msg): Unit =
    msg match {

      //        case Msg.InitDeltas(ds) =>
      //          for (d <- ds)
      //            addRemoteDelta(d)

//      case Msg.NewDelta(baseOrd, d) =>
//        addRemoteDelta(d)

      case _ =>
        throw new RuntimeException(s"Client $id doesn't support network msg: $msg")
    }

  def -->(svr: Server): Unit = {
    for (s <- editState) {
      val d = deltaAgainstRemote(s.doc)
      // TODO emptiness check
      send(Msg.NewDelta(s.baseOrd, d), svr)
    }
  }

  def deltaAgainstRemote(doc: Doc): Update =
    remote match {
      case Some(r) => doc.toUpdate(r.doc)
      case None    => doc.toUpdate
    }

  def recvRemoteStateOnConnect(state: Option[DraftStream[Any]]): Unit = {
//    for (s <- state) {
//
//    }
  }

  private def replaceRemote(baseOrd: Int, deltas: NonEmptyVector[Update]): Unit = {
    val old = remote
  }

  //  def deltaAgainstRemote(): Option[Update] =
  //    editState.map(deltaAgainstRemote(_))
  //
  //  private def modEditNE(f: Doc => Unit): Unit =
  //    editState match {
  //      case Some(e) => f(e)
  //      case None    =>
  //        val e = new Doc
  //        editState = Some(e)
  //        f(e)
  //    }
  //
  //  private def addRemoteDelta(delta: Update): Unit = {
  //    val r = remote.getOrElse {
  //      val r = new Doc
  //      remote = Some(r)
  //      r
  //    }
  //    r += delta
  //    modEditNE(_ += delta)
  //    afterExternalUpdate()
  //  }
  //
  //  def setRemoteState(deltas: Vector[Update]): Unit = {
  //
  //    if (deltas.isEmpty) {
  //      assert(editState.isEmpty) // TODO handle later
  //      remote = None
  //
  //    } else {
  //
  //      // TODO: Something like this will be necessary when drafts steams diverge
  //      // 1. x = e - r
  //      // 2. e' = r' + x
  //      //        val x = deltaAgainstRemote()
  //
  //      val r = new Doc
  //      r ++= deltas
  //      remote = Some(r)
  //      modEditNE(_ ++= deltas)
  //    }
  //
  //    afterExternalUpdate()
  //  }
  //
  //  private def afterExternalUpdate(): Unit = {
  //    if (!forceEditorOpen) {
  //      for (d <- editState) {
  //        editing = !d.itemsForall(_.deleted)
  //      }
  //    }
  //  }
}
