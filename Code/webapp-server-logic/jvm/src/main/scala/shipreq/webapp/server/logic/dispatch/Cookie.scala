package shipreq.webapp.server.logic.dispatch

import japgolly.univeq.UnivEq

final case class Cookie(name       : Cookie.Name,
                        value      : String,
                        maxAgeInSec: Option[Int],
                        httpOnly   : Option[Boolean],
                        secure     : Option[Boolean])

object Cookie {
  final case class Name(value: String)

  type LookupFn = Name => Option[String]

  final case class Update(add: List[Cookie], remove: List[Cookie.Name])

  object Update {
    val empty = apply(Nil, Nil)
    def add(c: Cookie) = apply(c :: Nil, Nil)
  }

  implicit def univEqName  : UnivEq[Name]   = UnivEq.derive
  implicit def univEqCookie: UnivEq[Cookie] = UnivEq.derive
  implicit def univEqUpdate: UnivEq[Update] = UnivEq.derive
}
