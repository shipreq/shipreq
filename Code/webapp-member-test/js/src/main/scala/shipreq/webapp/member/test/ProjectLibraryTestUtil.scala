package shipreq.webapp.member.test

import japgolly.microlibs.utils.Memo
import java.time.Instant
import scala.scalajs.js
import shipreq.webapp.base.util.LruMemo
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{Event, EventOrd, ProjectEvents, VerifiedEvent}
import shipreq.webapp.member.project.library._
import utest._

object ProjectLibraryTestUtil {

  private val now = Instant.now().minusSeconds(999999)

  val newVerifiedEvent: Int => VerifiedEvent =
    Memo.int { i =>
      VerifiedEvent(
        EventOrd(i),
        Event.ProjectNameSet(i.toString),
        now.plusSeconds(i),
      )
    }

  def newVerifiedEvents(ords: Int*): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ ords.map(newVerifiedEvent)

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

  def newProjectLibrary(latest: Int, futureEvents: Int*)(implicit cache: Cache = CacheJs()): ProjectLibrary = {
    val fes = VerifiedEvent.Seq.empty ++ futureEvents.iterator.map { i =>
      assert(i > (latest + 1))
      newVerifiedEvent(i)
    }
    val p = newProject(latest)
    ProjectLibrary.init(p, cache).addEvents(fes)
  }

  def newCache(milestoneEvery: Int               = CacheJs.MilestonesEvery,
               milestones    : js.Array[Project] = null,
               lruSize       : Int               = 20,
              ): Cache = {
    new Cache {
      override def apply(ord: EventOrd): Option[Project] =
        None

      override def storePotentialMilestone(p: Project): Unit =
        ()

      override def iterator() =
        Iterator.empty

      override protected def updateNE(projects: Iterable[Project]): Cache = {
        val latest = projects.maxBy(_.ordAsInt)

        val newCache =
          CacheJsTestAccess.nonEmpty(
            latest         = latest,
            milestoneEvery = milestoneEvery,
            milestones     = Option(milestones).getOrElse(newMilestones()),
            lru            = LruMemo.ExternalFn.byUnivEq(lruSize),
          )

        projects.foreach(newCache.storePotentialMilestone)

        newCache
      }
    }
  }

  @inline def newMilestones(): js.Array[Project] =
    new js.Array
}
