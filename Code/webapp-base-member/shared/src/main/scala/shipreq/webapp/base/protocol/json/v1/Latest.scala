package shipreq.webapp.base.protocol.json.v1

/** This is a convenience for usages that don't need to care about versioning (eg. benchmarks, WW, tests).
  * It reduces the amount of busy-work required when bumping versions by allowing you to modify just this one
  * object rather than all uses that don't care and just need the latest.
  */
object Latest {

  @inline implicit def decoderEvent         = Rev4.decoderEvent
  @inline implicit def encoderEvent         = Rev4.encoderEvent
  @inline implicit def decoderVerifiedEvent = Rev4.decoderVerifiedEvent
  @inline implicit def encoderVerifiedEvent = Rev4.encoderVerifiedEvent

  val AtomCodecs      = Rev3.AtomCodecs
  val SavedViewCodecs = Rev4.SavedViewCodecs

}
