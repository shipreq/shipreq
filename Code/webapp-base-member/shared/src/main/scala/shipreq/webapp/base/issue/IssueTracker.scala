package shipreq.webapp.base.issue

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{Event, EventSeqSummary}

final class IssueTracker(val issues : Issues,
                         val project: Project,
                         detectors  : Vector[IssueDetector],
                         tstate     : IssueTracker.TrackerState,
                         dstates    : Map[IssueDetector, IssueTracker.DetectorState],
                        ) {

  def update(events: TraversableOnce[Event], newProject: Project): IssueTracker =
    IssueTracker.update(EventSeqSummary(events), newProject, detectors, tstate, dstates)
}

object IssueTracker {

  def apply(project: Project): IssueTracker =
    apply(project, IssueDetectors.all.whole)

  def apply(project: Project, detectors: Vector[IssueDetector]): IssueTracker = {
    val tstateM = new MutableTrackerState(1)
    val dstatesM = detectors.iterator.map(d => d -> new MutableDetectorState(Vector.empty)).toMap

    // Run and prepare detectors
    for (d <- detectors) {
      val dstate = dstatesM(d)

      val action = IssueDetector.Action(
        add                 = i => dstate.issues += tstateM.assignId(i),
        foreachDirtyLiveReq = tstateM.dirtyFns.addForeachLiveReq)

      val init = IssueDetector.Init(action, project)

      d.init(init)
    }

    // Scan requirements
    for (f <- tstateM.dirtyFns.foreachLiveReq())
      project.liveReqIterator().foreach(f)

    buildTracker(project, detectors, tstateM, dstatesM)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private def update(ess        : EventSeqSummary,
                     newProject : Project,
                     detectors  : Vector[IssueDetector],
                     tstate     : IssueTracker.TrackerState,
                     dstates    : Map[IssueDetector, IssueTracker.DetectorState]): IssueTracker = {

    val essp     = ess.withProject(newProject)
    val reqs     = newProject.content.reqs
    val reqTypes = newProject.config.reqTypes

    @inline def isReqDead (id: ReqId) = reqs.need(id).live(reqTypes) is Dead
    @inline def isReqDirty(id: ReqId) = essp.reqs.contains(id)
    def isReqDeadOrDirty  (id: ReqId) = isReqDirty(id) || isReqDead(id)

    val autoInvalidate: Issue => Boolean = {
      case i: Issue.ConflictingTags       => isReqDeadOrDirty(i.reqId)
      case _: Issue.EmptyCodeGroup
         | _: Issue.UninhabitableTagField => false
    }

    val tstateM             = tstate.resume()
    val dstatesM            = dstates.mapValuesNow(_.resume())
    val withAllContentDirty = new MutableDirtyFns

    // Run and prepare detectors
    for (d <- detectors) {
      val dstate        = dstatesM(d)
      val dirtyFns      = new MutableDirtyFns
      var invalidateAll = false

      val action = IssueDetector.Action(
        add                 = i => dstate.issues += tstateM.assignId(i),
        foreachDirtyLiveReq = dirtyFns.addForeachLiveReq)

      val init = IssueDetector.Init(action, newProject)

      val increment = IssueDetector.Increment(
        init          = init,
        eventSummary  = ess,
        invalidateAll = () => invalidateAll = true)

      d.increment(increment)

      if (invalidateAll) {
        dstate.invalidateAll()
        withAllContentDirty += dirtyFns
      } else {
        dstate.invalidate(autoInvalidate)
        tstateM.dirtyFns += dirtyFns
      }
    }

    // Scan requirements
    (withAllContentDirty.foreachLiveReq(), tstateM.dirtyFns.foreachLiveReq()) match {
      case (Some(all), Some(dirty)) =>
        val dirtyReqs = essp.reqs
        for (req <- newProject.liveReqIterator()) {
          all(req)
          if (dirtyReqs.contains(req.id))
            dirty(req)
        }

      case (None, Some(f)) =>
        for (id <- essp.reqs) {
          val req = reqs.need(id)
          if (req.live(reqTypes) is Live)
            f(req)
        }

      case (Some(f), None) =>
        newProject.liveReqIterator().foreach(f)

      case (None, None) =>
        ()
    }

    buildTracker(newProject, detectors, tstateM, dstatesM)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private def buildTracker(newProject: Project,
                           detectors : Vector[IssueDetector],
                           tstateM   : MutableTrackerState,
                           dstatesM  : Map[IssueDetector, MutableDetectorState]): IssueTracker = {
    val tstateR = tstateM.result()
    val dstatesR = dstatesM.mapValuesNow(_.result())
    val issues = Issues.fromDetectorMap(dstatesR)(_.issues)
    new IssueTracker(issues, newProject, detectors, tstateR, dstatesR)
  }

  private def fuse[A](fs: TraversableOnce[A => Unit]): Option[A => Unit] =
    Option.unless(fs.isEmpty)(fs.reduce((x, y) => i => { x(i); y(i) }))

  private final class MutableTrackerState(firstId: Int) {
    private var _nextId = firstId

    val dirtyFns = new MutableDirtyFns

    def nextId(): IssueId = {
      val id = IssueId(_nextId)
      _nextId += 1
      id
    }

    def assignId(i: Issue): IssueWithId =
      IssueWithId(nextId(), i)

    def result(): TrackerState = {
      val n = _nextId
      TrackerState(() => new MutableTrackerState(n))
    }
  }

  private final class MutableDirtyFns {
    private[MutableDirtyFns] var fnsForeachLiveReq = List.empty[() => Req => Unit]

    def nonEmpty = fnsForeachLiveReq.nonEmpty

    val addForeachLiveReq: (() => Req => Unit) => Unit =
      fnsForeachLiveReq ::= _

    def foreachLiveReq(): Option[Req => Unit] =
      fuse(fnsForeachLiveReq.iterator.map(_()))

    def +=(d: MutableDirtyFns): Unit =
      fnsForeachLiveReq = fnsForeachLiveReq.reverse_:::(d.fnsForeachLiveReq)
  }

  private final case class TrackerState(resume: () => MutableTrackerState)

  private final class MutableDetectorState(old: Vector[IssueWithId]) {
    val issues = Vector.newBuilder[IssueWithId]
    private var retain = old

    def invalidate(f: Issue => Boolean): Unit =
      retain = retain.filter(i => !f(i.issue))

    def invalidateAll(): Unit =
      retain = Vector.empty

    def result(): DetectorState = {
      val i = (issues ++= retain).result()
      DetectorState(i, () => new MutableDetectorState(i))
    }
  }

  private final case class DetectorState(issues: Vector[IssueWithId],
                                         resume: () => MutableDetectorState)
}
