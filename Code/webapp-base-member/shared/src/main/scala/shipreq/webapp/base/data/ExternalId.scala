package shipreq.webapp.base.data

import japgolly.univeq.UnivEq

// The rest of this is in webapp-server.
// Exposure to JS defeats the purpose (i.e. obfuscation).

final case class ExternalId[T](value: String) extends AnyVal

object ExternalId {
  implicit def univEq[A]: UnivEq[ExternalId[A]] =
    UnivEq.force
}

