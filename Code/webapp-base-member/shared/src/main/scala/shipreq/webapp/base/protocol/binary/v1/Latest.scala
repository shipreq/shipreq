package shipreq.webapp.base.protocol.binary.v1

/** This is a convenience for usages that don't need to care about versioning (eg. benchmarks, WW, tests).
  * It reduces the amount of busy-work required when bumping versions by allowing you to modify just this one
  * object rather than all uses that don't care and just need the latest.
  */
object Latest {

  @inline implicit def picklerEvent                    = Rev2.picklerEvent
  @inline implicit def picklerProject                  = Rev2.picklerProject
  @inline implicit def picklerProjectAndOrd            = Rev2.picklerProjectAndOrd
  @inline implicit def picklerVerifiedEvent            = Rev2.picklerVerifiedEvent
  @inline implicit def picklerVerifiedEventSeq         = Rev2.picklerVerifiedEventSeq
  @inline implicit def picklerVerifiedEventNonEmptySeq = Rev2.picklerVerifiedEventNonEmptySeq

  val AtomPicklers      = Rev2.AtomPicklers
  val SavedViewPicklers = Rev1.SavedViewPicklers
}
