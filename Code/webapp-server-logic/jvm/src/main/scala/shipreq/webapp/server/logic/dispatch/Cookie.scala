package shipreq.webapp.server.logic.dispatch

final case class Cookie(name       : Cookie.Name,
                        value      : String,
                        maxAgeInSec: Option[Int],
                        httpOnly   : Option[Boolean],
                        secure     : Option[Boolean])

object Cookie {
  final case class Name(value: String)

  type LookupFn = Name => Option[String]

  object LookupFn {

    val empty: LookupFn =
      _ => None

    def overHeader(header: String): Cookie.LookupFn =
      if (header eq null)
        empty
      else
        name => {
          val k = name.value + "="

          val startIdx: Int =
            if (header.startsWith(k))
              0
            else {
              val i = header.indexOf("; " + k, k.length)
              if (i > 0) i + 2 else -1
            }

          if (startIdx < 0)
            None
          else
            Some(header.substring(startIdx + k.length).takeWhile(_ != ';'))
        }
  }

  final case class Update(add: List[Cookie], remove: List[Cookie.Name])

  object Update {
    val empty = apply(Nil, Nil)
    def add(c: Cookie) = apply(c :: Nil, Nil)
  }

  implicit def univEqName  : UnivEq[Name]   = UnivEq.derive
  implicit def univEqCookie: UnivEq[Cookie] = UnivEq.derive
  implicit def univEqUpdate: UnivEq[Update] = UnivEq.derive
}
