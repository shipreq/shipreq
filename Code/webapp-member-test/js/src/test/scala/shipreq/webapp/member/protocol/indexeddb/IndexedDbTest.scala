package shipreq.webapp.member.protocol.indexeddb

import utest._
import shipreq.webapp.member.test.TestIndexedDb
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node.asyncTest

object IndexedDbTest extends TestSuite {
  import IndexedDb._
  import TestIndexedDb.UnsafeTypes._

  override def tests = Tests {

    "basic" - asyncTest {
      val store = ObjectStoreDef("test")(IndexedDbCodec.String)

      val openCallbacks =
        TestIndexedDb.unusedOpenCallbacks.copy(
          upgradeNeeded = _.db.createObjectStore(store).void.asAsyncCallback
        )

      for {
        db      <- TestIndexedDb().open("blah")(openCallbacks)

        rw      <- db.transactionRW(store).asAsyncCallback
        storeRW <- rw.objectStore(store).asAsyncCallback
        _       <- storeRW.add(1, "hello")
        _       <- rw.completion

        ro      <- db.transactionRO(store).asAsyncCallback
        storeRO <- ro.objectStore(store).asAsyncCallback
        v1      <- storeRO.get(1)
        v2      <- storeRO.get(2)
      } yield {
        assertEq(v1, Some("hello"))
        assertEq(v2, None)
      }
    }
  }
}
