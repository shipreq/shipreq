package shipreq.webapp.base.protocol.binary.v1

/** This is a convenience for usages that don't need to care about versioning (eg. benchmarks, WW, tests).
  * It reduces the amount of busy-work required when bumping versions by allowing you to modify just this one
  * object rather than all uses that don't care and just need the latest.
  */
object Latest {

  @inline implicit def picklerEvent                    = Rev4.picklerEvent
  @inline implicit def picklerProject                  = Rev4.picklerProject
  @inline implicit def picklerProjectAndOrd            = Rev4.picklerProjectAndOrd
  @inline implicit def picklerVerifiedEvent            = Rev4.picklerVerifiedEvent
  @inline implicit def picklerVerifiedEventSeq         = Rev4.picklerVerifiedEventSeq
  @inline implicit def picklerVerifiedEventNonEmptySeq = Rev4.picklerVerifiedEventNonEmptySeq

  val AtomPicklers      = Rev3.AtomPicklers
  val SavedViewPicklers = Rev4.SavedViewPicklers
}
