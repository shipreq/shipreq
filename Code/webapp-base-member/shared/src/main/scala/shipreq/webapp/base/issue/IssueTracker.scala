package shipreq.webapp.base.issue

import shipreq.webapp.base.data._

final class IssueTracker(val issues : Issues,
                         val project: Project) {

  /** This used to be incremental */
  def update(newProject: Project): IssueTracker =
    IssueTracker(newProject)
}

object IssueTracker {

  def apply(project: Project): IssueTracker =
    apply(project, IssueDetectors.all.whole)

  def apply(project: Project, detectors: Vector[IssueDetector]): IssueTracker = {
    val tstateM = new MutableTrackerState(1)

    val ctx = IssueDetector.Ctx(
      project        = project,
      add            = i => tstateM.issues += tstateM.assignId(i),
      foreachLiveReq = tstateM.dirtyFns.liveReq.add,
      foreachLiveRcg = tstateM.dirtyFns.liveRcg.add,
      foreachLiveUcs = tstateM.dirtyFns.liveUcs.add)

    // Run and prepare detectors
    for (d <- detectors)
      d.detect(ctx)

    // Scan project
    tstateM.dirtyFns.liveReq.foreach(project.liveReqIterator())
    tstateM.dirtyFns.liveRcg.foreach(project.content.reqCodes.liveGroups)
    tstateM.dirtyFns.liveUcs.foreach(project.content.reqs.useCases.liveStepIterator())

    buildTracker(project, detectors, tstateM)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private def buildTracker(newProject: Project,
                           detectors : Vector[IssueDetector],
                           tstateM   : MutableTrackerState): IssueTracker = {
    val issues = Issues(tstateM.issues.result())
    new IssueTracker(issues, newProject)
  }

  private def fuseReduce[A](fs: TraversableOnce[() => A => Unit]): A => Unit =
    fs.toIterator.map(_()).reduce((x, y) => a => { x(a); y(a) })

  private final class MutableTrackerState(firstId: Int) {
    private var _nextId = firstId

    def nextId(): IssueId = {
      val id = IssueId(_nextId)
      _nextId += 1
      id
    }

    def assignId(i: Issue): IssueWithId =
      IssueWithId(nextId(), i)

    val dirtyFns = new MutableDirtyFns

    val issues = Vector.newBuilder[IssueWithId]
  }

  private final class MutableDirtyFnsA[A] {
    private var fns: List[() => A => Unit]     = Nil
    val add        : (() => A => Unit) => Unit = fns ::= _

    def foreach(as: => TraversableOnce[A]): Unit =
      if (fns.nonEmpty) {
        val i = as
        if (i.nonEmpty) {
          val f = fuseReduce(fns)
          i.foreach(f)
        }
      }
  }

  private final class MutableDirtyFns {
    val liveReq = new MutableDirtyFnsA[Req]
    val liveRcg = new MutableDirtyFnsA[LiveCodeGroup]
    val liveUcs = new MutableDirtyFnsA[UseCaseStep.Focus]
  }
}
