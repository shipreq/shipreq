package shipreq.webapp.member.protocol.indexeddb

import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.protocol.binary.Compression
import shipreq.webapp.member.test.project._
import shipreq.webapp.member.test.{TestEncryption, TestIndexedDb}
import utest._

object IndexedDbTest extends TestSuite {
  import TestIndexedDb.UnsafeTypes._

  override def tests = Tests {

    "basicSync" - asyncTest {
      val store = ObjectStoreDef.Sync("test", IndexedDbCodec.string)
      for {
        db   <- TestIndexedDb(store)
        _    <- db.add(store)(1, "hello")
        get1 <- db.get(store)(1)
        get2 <- db.get(store)(2)
      } yield {
        assertEq(get1, Some("hello"))
        assertEq(get2, None)
      }
    }

    "basicAsync" - asyncTest {
      val store = ObjectStoreDef.Async("test", IndexedDbCodec.string.async)
      for {
        db   <- TestIndexedDb(store)
        _    <- db.add(store)(1, "hello")
        get1 <- db.get(store)(1)
        get2 <- db.get(store)(2)
      } yield {
        assertEq(get1, Some("hello"))
        assertEq(get2, None)
      }
    }

    "stack" - asyncTest {
      import shipreq.webapp.member.project.protocol.binary.v1.Latest.picklerProject
      import SampleProject8.{project => project1}
      import SampleProject5.{project => project2}
      import SafePickler.ConstructionHelperImplicits._
      import TestEncryption.UnsafeTypes._

      implicit val safePicklerProject: SafePickler[Project] =
        picklerProject.asV1(0).withMagicNumbers(0x89827590, 0x8858F858)

      val zip3 = Compression(3, addHeaders = false)
      val zip9 = Compression(9, addHeaders = false)

      val dbName = "IndexedDbTest_stack"
      val storeName = "s"
      val key1 = 123
      val key2 = 321

      for {
        enc1   <- TestEncryption("a" * 32)
        enc2   <- TestEncryption("b" * 32)
        store13 = ObjectStoreDef.Async(storeName, IndexedDbCodec.default[Project](zip3, enc1))
        store19 = ObjectStoreDef.Async(storeName, IndexedDbCodec.default[Project](zip9, enc1))
        store2  = ObjectStoreDef.Async(storeName, IndexedDbCodec.default[Project](zip3, enc2))
        db13   <- TestIndexedDb(dbName, store13)
        db19   <- TestIndexedDb(dbName, store19)
        db2    <- TestIndexedDb(dbName, store2)
        _      <- db13.add(store13)(key1, project1)
        _      <- db19.add(store19)(key2, project2)
        p13    <- db13.get(store13)(key1)
        p19    <- db19.get(store19)(key1)
        p23    <- db13.get(store13)(key2)
        p29    <- db19.get(store19)(key2)
        px     <- db2.get(store2)(key1).attempt
      } yield {

        // tests that protocol stack works (i.e. pickle ↔ zip ↔ enc)
        // tests that compression level changes don't prevent deserialisation
        assertEq(p13, Some(project1))
        assertEq(p19, Some(project1))
        assertEq(p23, Some(project2))
        assertEq(p29, Some(project2))

        // test different encryption key
        assert(px.isLeft)
        px
      }
    }

  }
}
