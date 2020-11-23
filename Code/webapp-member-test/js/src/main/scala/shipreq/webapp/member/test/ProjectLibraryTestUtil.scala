package shipreq.webapp.member.test

import japgolly.microlibs.utils.Memo
import java.time.Instant
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{Event, EventOrd, ProjectEvents, VerifiedEvent}
import shipreq.webapp.member.project.library.{CacheJs, ProjectLibrary}
import utest._

object ProjectLibraryTestUtil {

  private val now = Instant.now().minusSeconds(999999)

  private val plCache = CacheJs()

  private val newVerifiedEvent: Int => VerifiedEvent =
    Memo.int { i =>
      VerifiedEvent(
        EventOrd(i),
        Event.ProjectNameSet(i.toString),
        now.plusSeconds(i),
      )
    }

  def newVerifiedEvents(range: Range): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ range.iterator.map(newVerifiedEvent)

  val newProjectEvents: Int => ProjectEvents =
    Memo.int { ord =>
      ProjectEvents(newVerifiedEvents(1 to ord))
    }

  val newProject: Int => Project =
    Memo.int { ord =>
      if (ord == 0)
        Project.empty
      else
        Project.empty.copy(
          name = ord.toString,
          history = newProjectEvents(ord))
    }

  def newProjectLibrary(latest: Int, futureEvents: Int*): ProjectLibrary = {
    val fes = VerifiedEvent.Seq.empty ++ futureEvents.iterator.map { i =>
      assert(i > (latest + 1))
      newVerifiedEvent(i)
    }
    val p = newProject(latest)
    ProjectLibrary.init(p, plCache).addEvents(fes)
  }


}
