package shipreq.webapp.member.project.library

import scala.scalajs.js
import scala.scalajs.js.annotation._
import shipreq.webapp.base.util.LruMemo
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.EventOrd

object CacheJs {

  def apply(): Cache =
    Empty

  /** Retain projects as milestones every n events.
    *
    * See https://shipreq.com/project/d6My#/reqs/DE-3
    */
  final val MilestonesEvery = 4000

  private object Empty extends Cache {
    override def apply(ord: EventOrd): Option[Project] =
      None

    override def storePotentialMilestone(p: Project): Unit =
      ()

    override def iterator() =
      Iterator.empty

    override protected def updateNE(projects: Iterable[Project]): Cache = {
      val latest = projects.maxBy(_.ordAsInt)

      val newCache =
        new NonEmpty(
          latest         = latest,
          milestoneEvery = MilestonesEvery,
          milestones     = new js.Array,
          lru            = LruMemo.ExternalFn.byUnivEq(20),
        )

      projects.foreach(newCache.storePotentialMilestone)

      newCache
    }
  }

  private[library] final class NonEmpty(
        private[library] val latest        : Project,
                             milestoneEvery: Int,
                             milestones    : js.Array[Project],
        private[library] val lru           : LruMemo.ExternalFn[Int, Project]) extends Cache {

    @inline private def isMilestone(i: Int): Boolean =
      (i % milestoneEvery) == 0

    @inline private def ordToMilestoneIdx(o: Int): Int =
      o / milestoneEvery - 1

//    @inline private def milestoneIdxToOrd(i: Int): Int =
//      (i + 1) * retainEvery

    @inline private def getMilestone(idx: Int): js.UndefOr[Project] =
      milestones.asInstanceOf[ArrayExt[Project]].get(idx)

    override def apply(ord: EventOrd): Option[Project] = Some {
      need(ord.value)
    }

    private def need(tgt: Int): Project =
      if (tgt == 0)
        Project.empty
      else if (isMilestone(tgt)) {
        val mi = ordToMilestoneIdx(tgt)
        val result = getMilestone(mi)
        result.getOrElse {
          buildAndUpdateMilestones(tgt)
        }
      } else
        lru.getOrElsePut(tgt) {
          buildAndUpdateMilestones(tgt)
        }

    private def closest(tgt: Int): Int = {
      var c = 0

      // Scan LRU keys
      lru.foreachKey { o =>
        if (o > c && o <= tgt)
          c = o
      }

      // Scan milestones
      if (c != tgt) {
        var i = 0
        var o = milestoneEvery
        while (o <= tgt) {
          if (o > c && getMilestone(i).isDefined)
            c = o
          i += 1
          o += milestoneEvery
        }
      }

      c
    }

    private def buildAndUpdateMilestones(tgt: Int): Project = {
      @tailrec
      def go(prev: Int, p1: Project, nextMilestone: Int): Project = {
        val to     = tgt min nextMilestone
        val events = latest.history.events.slice(prev, to)
        val p2     = p1.updateOrThrow(events)

        if (to == nextMilestone)
          storePotentialMilestone(p2)

        if (to == tgt)
          p2
        else
          go(to, p2, nextMilestone + milestoneEvery)
      }

      val startOrd      = closest(tgt)
      val startProject  = need(startOrd)
      val nextMilestone = ((startOrd / milestoneEvery) + 1) * milestoneEvery
      go(startOrd, startProject, nextMilestone)
    }

    override def storePotentialMilestone(p: Project): Unit = {
      val o = p.ordAsInt
      if (isMilestone(o))
        milestones.update(ordToMilestoneIdx(o), p)
    }

    override def iterator(): Iterator[Project] = {
      def milestoneIterator() =
        milestones.indices
          .iterator
          .map(getMilestone)
          .filter(_.isDefined)
          .map(_.get)

      val projects = new js.Array[Project]
      lru.foreachValueIgnoreAccess(projects.push(_))
      projects.push(latest)

      milestoneIterator() ++ projects.iterator
    }

    override protected def updateNE(projects: Iterable[Project]): Cache = {
      val newLatest = projects.maxBy(_.ordAsInt)

      val newCache =
        if (newLatest > latest)
          new NonEmpty(
            latest         = newLatest,
            milestoneEvery = milestoneEvery,
            milestones     = milestones, // could shallow copy via .jsSlice() but latestOrd check means can reuse safely
            lru            = lru.duplicate(),
          )
        else
          this

      projects.foreach(newCache.storePotentialMilestone)

      newCache
    }
  }

  @js.native
  @nowarn("cat=unused")
  private[library] trait ArrayExt[A] extends js.Object {
    @JSBracketAccess
    def get(index: Int): js.UndefOr[A] = js.native
  }

}