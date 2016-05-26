package shipreq.webapp.client.base.ui.semantic

import japgolly.univeq.UnivEq

sealed abstract class Size(c: ClassName) extends HasClass(c)
object Size {
  case object Mini    extends Size("mini")
  case object Tiny    extends Size("tiny")
  case object Small   extends Size("small")
  case object Default extends Size(NoClass)
  case object Large   extends Size("large")
  case object Big     extends Size("big")
  case object Huge    extends Size("huge")
  case object Massive extends Size("massive")

  implicit def univEq: UnivEq[Size] = UnivEq.derive
}

