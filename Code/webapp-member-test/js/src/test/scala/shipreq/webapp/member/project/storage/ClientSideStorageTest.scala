package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.TestClientSideStorage
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

object ClientSideStorageTest extends TestSuite {
  import AsyncCallback.delay

  override def tests = Tests {

    "dynamic" - asyncTest {
      val s1 = TestClientSideStorage()
      val s2 = TestClientSideStorage()
      val s = ClientSideStorage.ReadWrite.Dynamic(Creator1)(s1, s2)

      def states() = (s1.ordAsInt(), s2.ordAsInt())

      for {
        _ <- s.saveProjectLibrary(newProjectLibrary(2, 4))
        _ <- s.getProjectLibraryOrd.map(assertEq(_, Some(EventOrd.Latest(2))))
        _ <- delay(assertEq(states(), (2, 0)))

        _ <- delay(s1.available = false)
        _ <- s.getProjectLibraryOrd.map(assertEq(_, None))
        _ <- s.saveProjectLibrary(newProjectLibrary(3, 5))
        _ <- s.getProjectLibraryOrd.map(assertEq(_, Some(EventOrd.Latest(3))))
        _ <- delay(assertEq(states(), (2, 3)))
      } yield ()
    }

  }
}
