package shipreq.webapp.base.protocol.json.v1

/** This is a convenience for usages that don't need to care about versioning (eg. benchmarks, WW, tests).
  * It reduces the amount of busy-work required when bumping versions by allowing you to modify just this one
  * object rather than all uses that don't care and just need the latest.
  */
object Latest {
  import shipreq.webapp.base.protocol.json.v1.{Rev5 => L}

  @inline implicit def decoderEvent         = L.decoderEvent
  @inline implicit def encoderEvent         = L.encoderEvent
  @inline implicit def decoderVerifiedEvent = L.decoderVerifiedEvent
  @inline implicit def encoderVerifiedEvent = L.encoderVerifiedEvent
  @inline implicit def codecValidFilter     = L.codecValidFilter

  val AtomCodecs      = Rev5.AtomCodecs
  val SavedViewCodecs = L.SavedViewCodecs
}
