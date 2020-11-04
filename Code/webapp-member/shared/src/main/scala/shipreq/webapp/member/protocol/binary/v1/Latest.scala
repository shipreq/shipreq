package shipreq.webapp.member.protocol.binary.v1

/** This is a convenience for usages that don't need to care about versioning (eg. benchmarks, WW, tests).
  * It reduces the amount of busy-work required when bumping versions by allowing you to modify just this one
  * object rather than all uses that don't care and just need the latest.
  */
object Latest {
  import shipreq.webapp.member.protocol.binary.v1.{Rev7 => L}

  @inline implicit def picklerEvent                    = L.picklerEvent
  @inline implicit def picklerProject                  = L.picklerProject
  @inline implicit def picklerProjectAndOrd            = L.picklerProjectAndOrd
  @inline implicit def picklerVerifiedEvent            = L.picklerVerifiedEvent
  @inline implicit def picklerVerifiedEventSeq         = L.picklerVerifiedEventSeq
  @inline implicit def picklerVerifiedEventNonEmptySeq = L.picklerVerifiedEventNonEmptySeq
  @inline implicit def pickleValidFilter               = L.pickleValidFilter

  val AtomPicklers      = Rev6.AtomPicklers
  val SavedViewPicklers = L.SavedViewPicklers
}
