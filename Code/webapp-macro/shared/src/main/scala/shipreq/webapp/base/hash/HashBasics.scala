package shipreq.webapp.base.hash

import japgolly.univeq.UnivEq

final case class HashSchemeId(index: Int) extends AnyVal {
  def asChar: Char =
    ('a' + index).toChar
}

object HashSchemeId {
  implicit def univEq: UnivEq[HashSchemeId] =
    UnivEq.derive

  def fromChar(char: Char): HashSchemeId =
    apply(char - 'a')
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class HashScopeVer(value: Int) extends AnyVal {
  @inline def inc: HashScopeVer =
    HashScopeVer(value + 1)

  @inline def <=(x: HashScopeVer): Boolean =
    value <= x.value
}

object HashScopeVer {
  val init: HashScopeVer =
    apply(1)
}

