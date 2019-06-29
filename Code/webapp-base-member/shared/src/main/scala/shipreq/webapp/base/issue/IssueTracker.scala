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
        foreachDirtyLiveReq = tstateM.dirtyFns.liveReq.add,
        foreachDirtyLiveRcg = tstateM.dirtyFns.liveRcg.add)

      val init = IssueDetector.Init(action, project)

      d.init(init)
    }

    // Scan dirty requirements
    for (ff <- tstateM.dirtyFns.liveReq.foreach) {
      val it = project.liveReqIterator()
      if (it.nonEmpty)
        it.foreach(ff())
    }

    // Scan dirty req code groups
    for (ff <- tstateM.dirtyFns.liveRcg.foreach) {
      val it = project.content.reqCodes.liveGroups
      if (it.nonEmpty)
        it.foreach(ff())
    }

    buildTracker(project, detectors, tstateM, dstatesM)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private def update(ess        : EventSeqSummary,
                     newProject : Project,
                     detectors  : Vector[IssueDetector],
                     tstate     : IssueTracker.TrackerState,
                     dstates    : Map[IssueDetector, IssueTracker.DetectorState]): IssueTracker = {

    val essp         = ess.withProject(newProject)
    val reqs         = newProject.content.reqs
    val liveGroupIds = newProject.content.reqCodes.liveGroupIds
    val reqTypes     = newProject.config.reqTypes

    @inline def isReqDead (id: ReqId) = reqs.need(id).live(reqTypes) is Dead
    @inline def isReqDirty(id: ReqId) = essp.reqs.contains(id)
    def isReqDeadOrDirty  (id: ReqId) = isReqDirty(id) || isReqDead(id)

    @inline def isRcgDead (id: ReqCodeGroupId) = !liveGroupIds.contains(id)
    @inline def isRcgDirty(id: ReqCodeGroupId) = essp.reqCodeGroups.all.contains(id)
    def isRcgDeadOrDirty  (id: ReqCodeGroupId) = isRcgDirty(id) || isRcgDead(id)

    val autoInvalidate: Issue => Boolean = {
      case i: Issue.BlankTitle            => isReqDeadOrDirty(i.reqId) // wider than necessary
      case i: Issue.ConflictingTags       => isReqDeadOrDirty(i.reqId)
      case i: Issue.DeadIssueTagInRcg     => isRcgDeadOrDirty(i.rcgId)
      case i: Issue.DeadIssueTagInReq     => isReqDeadOrDirty(i.reqId)
      case i: Issue.DeadRefInRcg          => isRcgDeadOrDirty(i.rcgId)
      case i: Issue.DeadRefInReq          => isReqDeadOrDirty(i.reqId)
      case i: Issue.DeadTag               => isReqDeadOrDirty(i.reqId)
      case i: Issue.EmptyCodeGroup        => isRcgDeadOrDirty(i.rcgId)
      case i: Issue.IssueTagInRcg         => isRcgDeadOrDirty(i.rcgId)
      case i: Issue.IssueTagInReq         => isReqDeadOrDirty(i.reqId)
      case _: Issue.UninhabitableTagField => false
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
        foreachDirtyLiveReq = dirtyFns.liveReq.add,
        foreachDirtyLiveRcg = dirtyFns.liveRcg.add)

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
    incrementalScan[ReqId, Req](
      allFns       = withAllContentDirty.liveReq,
      dirtyFns     = tstateM.dirtyFns.liveReq,
      liveIterator = () => newProject.liveReqIterator(),
      dirtyIdSet   = () => essp.reqs,
      id           = _.id,
      foreachLive  = f => id => {
        val req = reqs.need(id)
        if (req.live(reqTypes) is Live)
          f(req)
      },
    )

    // Scan req code groups
    incrementalScan[ReqCodeGroupId, LiveCodeGroup](
      allFns       = withAllContentDirty.liveRcg,
      dirtyFns     = tstateM.dirtyFns.liveRcg,
      liveIterator = () => newProject.content.reqCodes.liveGroups.iterator,
      dirtyIdSet   = () => essp.reqCodeGroups.all,
      id           = _.id,
      foreachLive  = f => newProject.content.reqCodes.liveGroup(_).foreach(f)
    )

    buildTracker(newProject, detectors, tstateM, dstatesM)
  }

  private def incrementalScan[Id, A](allFns      : MutableDirtyFnsA[A],
                                     dirtyFns    : MutableDirtyFnsA[A],
                                     liveIterator: () => Iterator[A],
                                     dirtyIdSet  : () => Set[Id],
                                     id          : A => Id,
                                     foreachLive : (A => Unit) => Id => Unit): Unit =
    (allFns.foreach, dirtyFns.foreach) match {

      case (Some(allF), Some(dirtyF)) =>
        val it = liveIterator()
        if (it.nonEmpty) {
          val all = allF()
          val dirtyAs = dirtyIdSet()
          if (dirtyAs.isEmpty) {
            it.foreach(all)
          } else {
            val dirty = dirtyF()
            for (a <- it) {
              all(a)
              if (dirtyAs.contains(id(a)))
                dirty(a)
            }
          }
        }

      case (None, Some(ff)) =>
        val dirtyAs = dirtyIdSet()
        if (dirtyAs.nonEmpty) {
          val f = ff()
          dirtyAs.foreach(foreachLive(f))
        }

      case (Some(ff), None) =>
        val it = liveIterator()
        if (it.nonEmpty) {
          it.foreach(ff())
        }

      case (None, None) =>
        ()
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

  private def fuse[A](fs: TraversableOnce[() => A => Unit]): Option[() => A => Unit] =
    Option.unless(fs.isEmpty)(() => fs.toIterator.map(_()).reduce((x, y) => a => { x(a); y(a) }))

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

  private final class MutableDirtyFnsA[A] {
    private var fns               : List[() => A => Unit]     = Nil
    def nonEmpty                  : Boolean                   = fns.nonEmpty
    val add                       : (() => A => Unit) => Unit = fns ::= _
    def foreach                   : Option[() => A => Unit]   = fuse(fns)
    def +=(f: MutableDirtyFnsA[A]): Unit                      = fns = fns.reverse_:::(f.fns)
  }

  private final class MutableDirtyFns {
    val liveReq = new MutableDirtyFnsA[Req]
    val liveRcg = new MutableDirtyFnsA[LiveCodeGroup]

    def nonEmpty = liveReq.nonEmpty || liveRcg.nonEmpty

    def +=(d: MutableDirtyFns): Unit = {
      liveReq += d.liveReq
      liveRcg += d.liveRcg
    }
  }

  private final case class TrackerState(resume: () => MutableTrackerState)

  private final class MutableDetectorState(old: Vector[IssueWithId]) {
    val issues = Vector.newBuilder[IssueWithId]
    private var retain = old

    val invalidate: (Issue => Boolean) => Unit =
      f => retain = retain.filter(i => !f(i.issue))

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
