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
    IndexedDb(window.indexedDB.get)
  }

  private var prevDbIndex = 0

  def freshDbName() = {
    prevDbIndex += 1
    DatabaseName("testdb_" + prevDbIndex)
  }

  def fresh = {
    val name = freshDbName()
    instance().open(name)
  }

  def apply(name: String, stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    apply(DatabaseName(name), stores: _*)

  def apply(name: DatabaseName, stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    instance().open(name)(createStoresOnOpen(stores: _*))

  def apply(stores: ObjectStoreDef[_, _]*): AsyncCallback[IndexedDb.Database] =
    fresh(createStoresOnOpen(stores: _*))

  def unusedOpenCallbacks: OpenCallbacks =
    OpenCallbacks(
      upgradeNeeded = _ => Callback.empty,
      versionChange = _ => Callback.empty,
      closed        = Callback.empty,
    )

  def createStoresOnOpen(stores: ObjectStoreDef[_, _]*): OpenCallbacks =
    unusedOpenCallbacks.copy(
      upgradeNeeded = e => Callback.traverse(stores)(e.db.createObjectStore(_))
    )

  object UnsafeTypes {
    implicit def autoIndexedDbDatabaseName(s: String): DatabaseName = DatabaseName(s)
  }
}
