package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import scalaz.Equal
import shipreq.base.test.Node.asyncTest
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.{ProjectId, UserId}
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.ProjectLibrary
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

abstract class ClientSideStorageLaws extends TestSuite {

  protected final implicit val equalProjectLibrary: Equal[ProjectLibrary] =
    Equal.equalBy(l => (l.latest, l.futureEvents))

  protected def createInstance(ctx: Context): AsyncCallback[ClientSideStorage.ReadWrite]

  private final object Internals {

    def userId(i: Int): UserId.Public =
      Obfuscated("user-" + i)

    def projectId(i: Int): ProjectId.Public =
      Obfuscated("project-" + i)

    private val padding = "_" * 32

    def encKey(s: String): ClientSideProjectEncryptionKey = {
      assert(s.length <= 32)
      val s2 = (s + padding).take(32)
      ClientSideProjectEncryptionKey(BinaryData.fromStringBytes(s2))
    }

    val u1p1 = Context(userId(1), projectId(1), encKey("u1p1"))
    val u2p1 = Context(userId(2), projectId(1), encKey("u2p1"))
    val u1p2 = Context(userId(1), projectId(2), encKey("u1p2"))

    val newInstance = createInstance(u1p1)

    val pl2_9 = newProjectLibrary(2, 9)
    val pl3   = newProjectLibrary(3)
    val pl3_6 = newProjectLibrary(3, 6)
    val pl4   = newProjectLibrary(4)

    assert(pl3.futureEvents.isEmpty)
    assert(pl3_6.futureEvents.nonEmpty)
  }

  // ===================================================================================================================

  override final def tests = Tests {
    import Internals._

    "empty" - asyncTest {
      for {
        s   <- newInstance
        pl  <- s.getProjectLibrary
        ord <- s.getProjectLibraryOrd
      } yield {
        assertEq(pl, None)
        assertEq(ord, None)
      }
    }

    "saveGet" - asyncTest {
      for {
        s   <- newInstance
        _   <- s.saveProjectLibrary(pl3_6)
        pl  <- s.getProjectLibrary
        ord <- s.getProjectLibraryOrd
      } yield {
        assertEq(pl, Some(pl3)) // notice future events are not saved
        assertEq(ord, Some(EventOrd.Latest(3)))
      }
    }

    "saveOlder" - asyncTest {
      for {
        s   <- newInstance
        _   <- s.saveProjectLibrary(pl3_6)
        _   <- s.saveProjectLibrary(pl2_9)
        pl  <- s.getProjectLibrary
        ord <- s.getProjectLibraryOrd
      } yield {
        assertEq(pl, Some(pl3))
        assertEq(ord, Some(EventOrd.Latest(3)))
      }
    }

    "saveNewer" - asyncTest {
      for {
        s   <- newInstance
        _   <- s.saveProjectLibrary(pl2_9)
        _   <- s.saveProjectLibrary(pl3_6)
        pl  <- s.getProjectLibrary
        ord <- s.getProjectLibraryOrd
      } yield {
        assertEq(pl, Some(pl3))
        assertEq(ord, Some(EventOrd.Latest(3)))
      }
    }

    "isolation" - asyncTest {
      for {
        a   <- createInstance(u1p1)
        b   <- createInstance(u2p1)
        c   <- createInstance(u1p2)
        _   <- a.saveProjectLibrary(pl2_9)
        _   <- b.saveProjectLibrary(pl3)
        _   <- c.saveProjectLibrary(pl4)
        x   <- a.getProjectLibraryOrd
        y   <- b.getProjectLibraryOrd
        z   <- c.getProjectLibraryOrd
      } yield {
        assertEq(x, Some(EventOrd.Latest(2)))
        assertEq(y, Some(EventOrd.Latest(3)))
        assertEq(z, Some(EventOrd.Latest(4)))
      }
    }

  }
}