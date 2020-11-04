package shipreq.webapp.base.data

import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.base.util.Obfuscated

final case class ProjectId(value: Long) extends TaggedLong // not AnyVal, it gets boxed

object ProjectId {

  /** The real ProjectId is never directly exposed to users. Publicly it has a different ID. */
  type Public = Obfuscated[ProjectId]
}
