package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import scalaz.Equal
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.{CacheJs, ProjectLibrary}
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

abstract class ClientSideStorageLaws extends TestSuite {
  import TestData._

  protected final implicit val equalProjectLibrary: Equal[ProjectLibrary] =
    Equal.equalBy(l => (l.latest, l.futureEvents))

  protected def createInstance: (ClientSideStorage.Context, ClientSideProjectEncryptionKey) => AsyncCallback[ClientSideStorage.ReadWrite]

  private final object Internals {

    implicit val cache = CacheJs()

    val newInstance = createInstance(u1p1, key_u1p1)

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
        a   <- createInstance(u1p1, key_u1p1)
        b   <- createInstance(u2p1, key_u2p1)
        c   <- createInstance(u1p2, key_u1p2)
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