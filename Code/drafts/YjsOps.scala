package shipreq.base.test.drafts

import shipreq.webapp.member.jsfacade.Yjs

object Yjs2 {
  import Yjs.{Doc, Item, YText}

  final case class Update(value: Yjs.Update) extends AnyVal
  final case class StateVector(value: Yjs.StateVector) extends AnyVal

  implicit final class YjsDocExt(private val self: Doc) extends AnyVal {

    @inline def +=(update: Update): Unit =
      Yjs.applyUpdate(self, update.value)

    @inline def ++=(updates: IterableOnce[Update]): Unit =
      updates.iterator.foreach(+=)

    @inline def +=(doc: Doc): Unit =
      Yjs.applyUpdate(self, doc.toUpdate(self).value)

    @inline def toUpdate: Update =
      Update(Yjs.encodeStateAsUpdate(self))

    @inline def toUpdate(target: StateVector): Update =
      Update(Yjs.encodeStateAsUpdate(self, target.value))

    @inline def toUpdate(target: Doc): Update =
      toUpdate(target.toStateVector)

    @inline def toUpdate(target: Option[Doc]): Update =
      target match {
        case Some(t) => toUpdate(t)
        case None    => toUpdate
      }

    @inline def toUpdateSV(target: Option[StateVector]): Update =
      target match {
        case Some(t) => toUpdate(t)
        case None    => toUpdate
      }

    @inline def toStateVector: StateVector =
      StateVector(Yjs.encodeStateVector(self))

    def itemsForall(f: Item => Boolean): Boolean =
      self.store.clients.values.forall(_.forall(f))

    def itemsExists(f: Item => Boolean): Boolean =
      self.store.clients.values.exists(_.exists(f))

    def itemsFilter(f: Item => Boolean): Unit =
      self.transact(t => {
        for {
          items <- self.store.clients.values
          item <- items
        } {
          if (!item.deleted && !f(item))
            item.delete(t)
        }
      })
  }

  implicit final class YjsTextExt(private val self: YText) extends AnyVal {
    def append(s: String) = self.insert(self.length, s)
    def prepend(s: String) = self.insert(0, s)
    def deleteLast(length: Int) = self.delete(self.length - length, length)
    def replaceFirst(from: String, to: String): Unit = {
      val s = self.strValue()
      val i = s.indexOf(from)
      assert(i >= 0, s"'$from' not found in '$s'")
      self.doc.transact(_ => {
        self.delete(i, from.length)
        self.insert(i, to)
      })
    }
  }

  def newUpdate(clientId: Int)(f: Doc => Unit): Update = {
    val d = new Doc
    d.clientID = clientId
    f(d)
    d.toUpdate
  }
}
