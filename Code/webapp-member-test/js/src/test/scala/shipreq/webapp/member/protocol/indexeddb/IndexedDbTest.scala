package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.member.test.TestEncryption
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.member.test.TestIndexedDb
import utest._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.protocol.binary.Compression
import shipreq.webapp.member.test.project._

object IndexedDbTest extends TestSuite {
  import IndexedDb._
  import TestIndexedDb.UnsafeTypes._

  override def tests = Tests {

    "basic" - asyncTest {
      val store = ObjectStoreDef("test")(IndexedDbCodec.string)

      val openCallbacks =
        TestIndexedDb.unusedOpenCallbacks.copy(
          upgradeNeeded = _.db.createObjectStore(store).void.asAsyncCallback
        )

      for {
        db      <- TestIndexedDb().open("IndexedDbTest_basic")(openCallbacks)

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

    "stack" - asyncTest {
      import shipreq.webapp.member.project.protocol.binary.v1.Latest.picklerProject
      import SampleProject8.project
      import SafePickler.ConstructionHelperImplicits._
      import TestEncryption.UnsafeTypes._

      implicit val safePicklerProject: SafePickler[Project] =
        picklerProject.asV1(0).withMagicNumbers(0x89827590, 0x8858F858)

      val zip = Compression(3, addHeaders = false)

      val dbName = "IndexedDbTest_stack"
      val storeName = "s"
      val key = 123

      def openCallbacks(store: ObjectStoreDef[_]) =
        TestIndexedDb.unusedOpenCallbacks.copy(
          upgradeNeeded = _.db.createObjectStore(store).void.asAsyncCallback
        )

      for {
        enc1     <- TestEncryption("a" * 32)
//        enc2     <- TestEncryption("b" * 32)
//        codec1    <- AsyncCallback.delay(IndexedDbCodec.default[Project](zip, enc1))
        codec1    <- AsyncCallback.delay(IndexedDbCodec.binary.encrypt(enc1).pickle[Project])
//        codec2    = IndexedDbCodec.default[Project](zip, enc2)
        store1    <- AsyncCallback.delay(ObjectStoreDef(storeName)(codec1))
//        store2    = ObjectStoreDef(storeName)(codec2)
        db1      <- TestIndexedDb().open(dbName)(openCallbacks(store1))
//        db2      <- TestIndexedDb().open(dbName)(openCallbacks(store2))

        rw       <- db1.transactionRW(store1).asAsyncCallback.logResult(implicitly[sourcecode.Line].value.toString)
        storeRW  <- rw.objectStore(store1).asAsyncCallback.logResult(implicitly[sourcecode.Line].value.toString)
        _        <- storeRW.add(key, project).logResult(implicitly[sourcecode.Line].value.toString)
        _        <- rw.completion.logResult(implicitly[sourcecode.Line].value.toString)

        ro1      <- db1.transactionRO(store1).asAsyncCallback.logResult(implicitly[sourcecode.Line].value.toString)
        storeRO1 <- ro1.objectStore(store1).asAsyncCallback.logResult(implicitly[sourcecode.Line].value.toString)
        p1       <- storeRO1.get(key).logResult(implicitly[sourcecode.Line].value.toString)
        _        <- ro1.completion.logResult(implicitly[sourcecode.Line].value.toString)

//        ro2      <- db2.transactionRO(store2).asAsyncCallback.logAround("y")
//        storeRO2 <- ro2.objectStore(store2).asAsyncCallback.logAround("z")
//        p2       <- storeRO2.get(key).attempt
        
      } yield {
        assertEq(p1, Some(project))
      }
    }
  }
}
