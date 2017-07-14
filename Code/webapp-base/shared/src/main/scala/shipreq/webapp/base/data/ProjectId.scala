package shipreq.webapp.base.data

import shipreq.base.util.TaggedTypes.TaggedLong

final case class ProjectId(value: Long) extends TaggedLong // not AnyVal, it gets boxed

object ProjectId {

  /** The real ProjectId is never directly exposed to users. Publicly it has a different ID. */
  type Public = Obfuscated[ProjectId]
}
