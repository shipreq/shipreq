package shipreq.webapp.base.data

// The rest of this is in webapp-server.
// Exposure to JS defeats the purpose (i.e. obfuscation).

final case class ExternalId[T](value: String) extends AnyVal

