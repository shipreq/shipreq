package shipreq.webapp.base.data.derivation

import shipreq.webapp.base.text.Atom._
import nyaya.util.Multimap
import shipreq.base.util.Direction
import shipreq.webapp.base.data._

final class ReverseDependencies(atomScan: AtomScan, useCases: UseCases) {

  // This is super minimal at the moment. All I need is reverse-lookup for use case steps.

  private var _useCaseStepIdRefs = Multimap.empty[UseCaseStepId, Set, Location]

  // Scan: refs in reqs
  for {
    live       <- Live
    (src, lds) <- atomScan.contentRefsInReqs.raw
    ld         <- lds(live)
  } ld.value match {

    case tgt: ContentRef # UseCaseStepRef =>
      val loc = Location.Req(src, ld.loc)
      _useCaseStepIdRefs = _useCaseStepIdRefs.add(tgt.value, loc)

    case _ =>
  }

  // Scan: refs in RCGs
  for {
    live       <- Live
    (src, lds) <- atomScan.contentRefsInRcgs.raw
    ld         <- lds(live)
  } ld.value match {

    case tgt: ContentRef # UseCaseStepRef =>
      val loc = Location.ReqCodeGroup(src, ld.loc)
      _useCaseStepIdRefs = _useCaseStepIdRefs.add(tgt.value, loc)

    case _ =>
  }

  // Scan: use case step flow
  // Don't use a for-comprehension here
  // https://github.com/scala/bug/issues/11951
  Direction.foreach { dir =>
    useCases.stepFlow(dir).iterator.foreach { case (src, tgts) =>
      val loc = Location.Req(useCases.stepIndex(src).useCaseId, Location.Text.UseCaseStep(src))
      tgts.foreach { tgt =>
        _useCaseStepIdRefs = _useCaseStepIdRefs.add(tgt, loc)
      }
    }
  }

  // ===================================================================================================================

  def useCaseStepId(id: UseCaseStepId): Set[Location] =
    _useCaseStepIdRefs(id)

}
