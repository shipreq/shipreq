package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react._
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.protocol.binary.{BinaryFormat, Compression}
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project._
import shipreq.webapp.member.test.{TestEncryption, TestIndexedDb}
import utest._

object IndexedDbTest extends TestSuite {

  override def tests = Tests {

    "basicSync" - asyncTest {
      val store = ObjectStoreDef.Sync("test", KeyCodec.int, ValueCodec.string)
      for {
        db    <- TestIndexedDb(store)
        _     <- db.add(store)(1, "hello")
        _     <- db.add(store)(3, "three")
        get1  <- db.get(store)(1)
        get2  <- db.get(store)(2)
        get3  <- db.get(store)(3)
        keys1 <- db.getAllKeys(store)
        vals1 <- db.getAllValues(store)
        _     <- db.delete(store)(1)
        keys2 <- db.getAllKeys(store)
        vals2 <- db.getAllValues(store)
      } yield {
        assertEq(get1, Some("hello"))
        assertEq(get2, None)
        assertEq(get3, Some("three"))
        assertSeqIgnoreOrder(keys1)(1, 3)
        assertSeqIgnoreOrder(keys2)(3)
        assertSeqIgnoreOrder(vals1)("hello", "three")
        assertSeqIgnoreOrder(vals2)("three")
      }
    }

    "basicAsync" - asyncTest {
      val store = ObjectStoreDef.Async("test", KeyCodec.int, ValueCodec.string.async)
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

    "pickleCompressEncrypt" - asyncTest {
      import shipreq.webapp.member.project.protocol.binary.Latest.picklerProject
      import SampleProject8.{project => project1}
      import SampleProject5.{project => project2}
      import SafePickler.ConstructionHelperImplicits._
      import TestEncryption.UnsafeTypes._
      import ValueCodec.Async.binary

      implicit val safePicklerProject: SafePickler[Project] =
        picklerProject.asV1(0).withMagicNumbers(0x89827590, 0x8858F858)

      val zip3 = Compression(3, addHeaders = false)
      val zip9 = Compression(9, addHeaders = false)

      val dbName = "IndexedDbTest_stack"
      val storeName = "s"
      val kc = KeyCodec.int
      val key1 = 123
      val key2 = 321

      for {
        enc1   <- TestEncryption("a" * 32)
        enc2   <- TestEncryption("b" * 32)
        fmt13   = BinaryFormat.pickleCompressEncrypt[Project](zip3, enc1)
        fmt19   = BinaryFormat.pickleCompressEncrypt[Project](zip9, enc1)
        fmt2    = BinaryFormat.pickleCompressEncrypt[Project](zip3, enc2)
        store13 = ObjectStoreDef.Async(storeName, kc, binary.xmapBinaryFormat(fmt13))
        store19 = ObjectStoreDef.Async(storeName, kc, binary.xmapBinaryFormat(fmt19))
        store2  = ObjectStoreDef.Async(storeName, kc, binary.xmapBinaryFormat(fmt2))
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

    "put" - asyncTest {
      val store = ObjectStoreDef.Sync("test", KeyCodec.int, ValueCodec.string)
      for {
        db    <- TestIndexedDb(store)
        _     <- db.add(store)(1, "x1")
        add2  <- db.add(store)(1, "x2").attempt
        _     <- db.put(store)(2, "y1")
        _     <- db.put(store)(2, "y2")
        get1  <- db.get(store)(1)
        get2  <- db.get(store)(2)
      } yield {
        assertEq(get1, Some("x1"))
        assertEq(get2, Some("y2"))
        assert(add2.isLeft)
        add2
      }
    }

    "closeOnUpgrade" - asyncTest {
      val name = TestIndexedDb.freshDbName()
      val tdb = TestIndexedDb.instance()
      val c = TestIndexedDb.unusedOpenCallbacks
      val store = ObjectStoreDef.Sync("test", KeyCodec.int, ValueCodec.string)

      for {
        verChg <- AsyncCallback.barrier.asAsyncCallback
        closed <- AsyncCallback.barrier.asAsyncCallback

        db1    <- tdb.open(name, 1)(c.copy(
                    upgradeNeeded = _.createObjectStore(1, store),
                    versionChange = _ => Callback.log("db1 verChg") >> verChg.complete,
                    closed        = Callback.log("db1 closing") >> closed.complete))

        _      <- db1.add(store)(1, "omg")

        db2    <- tdb.open(name, 2)(c.copy(
                    upgradeNeeded = _ => Callback.log("db2 upgrading")))

        _      <- verChg.await
        _      <- closed.await
        v1     <- db1.get(store)(1).attempt
        v2     <- db2.get(store)(1)
      } yield {
        assert(v1.isLeft)
        assertEq(v2, Some("omg"))
        v1
      }
    }

  }
}
