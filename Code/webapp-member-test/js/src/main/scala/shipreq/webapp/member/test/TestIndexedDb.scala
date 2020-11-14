package shipreq.webapp.member.test

import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.window
import shipreq.base.test.Node
import shipreq.webapp.member.protocol.indexeddb.IndexedDb

object TestIndexedDb {
  import IndexedDb._

  def apply(): IndexedDb = {
    Node.loadFakeIndexedDb()
    IndexedDb(window.indexedDB)
  }

  private implicit def throwableStrings(s: String): Throwable =
    new RuntimeException(s)

  def unusedOpenCallbacks: OpenCallbacks =
    OpenCallbacks(
      upgradeNeeded = v => AsyncCallback.throwException(s"IndexedDb.open.upgradeNeeded called: $v"),
      blocked = Callback.throwException("IndexedDb.open.blocked called")
    )

  object UnsafeTypes {
    implicit def autoIndexedDbDatabaseName(s: String): DatabaseName = DatabaseName(s)
    implicit def autoIndexedDbKeyFromString(s: String): Key = Key(s)
    implicit def autoIndexedDbKeyFromInt(i: Int): Key = Key(i)
  }
}
