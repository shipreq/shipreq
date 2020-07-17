package shipreq.webapp.base.data

// The means to obfuscate rest of this is in webapp-server.
// Its exposure to JS is a security risk.
final case class Obfuscated[A](value: String) extends AnyVal {

  def subst[B]: Obfuscated[B] =
    Obfuscated(value)
}

object Obfuscated {
  implicit def univEq[A]: UnivEq[Obfuscated[A]] =
    UnivEq.force
}
