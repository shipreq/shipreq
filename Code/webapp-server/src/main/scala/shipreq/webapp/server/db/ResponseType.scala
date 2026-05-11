package shipreq.webapp.server.db

import japgolly.microlibs.adt_macros.AdtMacros

/** @since DB migration v4.4 */
sealed abstract class ResponseType(final val dbValue: String, final val idx: Int)
object ResponseType {
  case object `1xx` extends ResponseType("1xx", 0)
  case object `2xx` extends ResponseType("2xx", 1)
  case object `3xx` extends ResponseType("3xx", 2)
  case object `4xx` extends ResponseType("4xx", 3)
  case object `5xx` extends ResponseType("5xx", 4)
  case object Other extends ResponseType("other", 5)

  def apply(code: Int): ResponseType =
    if (code >= 200) {
      if (code < 300)
        `2xx`
      else if (code < 400)
        `3xx`
      else if (code < 500)
        `4xx`
      else if (code < 600)
        `5xx`
      else
        Other
    } else {
      if (code >= 100)
        `1xx`
      else
        Other
    }

  implicit def univEq: UnivEq[ResponseType] = UnivEq.derive

  val values = AdtMacros.adtValues[ResponseType]

  assert(values.whole.indices.toSet == values.whole.map(_.idx).toSet)
}
