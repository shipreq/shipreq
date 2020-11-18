package shipreq.webapp.member.test

import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.window
import shipreq.base.test.Node
import shipreq.webapp.member.protocol.indexeddb._

object TestIndexedDb {
  import IndexedDb._

  /** Unfortunately, this isn't isolated but has process-scoped shared-state via the global window.indexedDB. */
  def instance(): IndexedDb = {
    Node.loadFakeIndexedDb()
    IndexedDb(window.indexedDB)
  }

  private var prevDbIndex = 0

  def fresh = {
    prevDbIndex += 1
    val name = DatabaseName("testdb_" + prevDbIndex)
    instance().open(name)
  }

  def apply(name: String, stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    apply(DatabaseName(name), stores: _*)

  def apply(name: DatabaseName, stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    instance().open(name)(createStoresOnOpen(stores: _*))

  def apply(stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    fresh(createStoresOnOpen(stores: _*))

  private implicit def throwableStrings(s: String): Throwable =
    new RuntimeException(s)

  def unusedOpenCallbacks: OpenCallbacks =
    OpenCallbacks(
      upgradeNeeded = v => Callback.throwException(s"IndexedDb.open.upgradeNeeded called: $v"),
      blocked = Callback.throwException("IndexedDb.open.blocked called")
    )

  def createStoresOnOpen(stores: ObjectStoreDef[_, _]*): OpenCallbacks =
    unusedOpenCallbacks.copy(
      upgradeNeeded = e => Callback.traverse(stores)(e.db.createObjectStore(_))
    )

  object UnsafeTypes {
    implicit def autoIndexedDbDatabaseName(s: String): DatabaseName = DatabaseName(s)
  }
}
