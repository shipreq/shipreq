package shipreq.webapp.base.filter

import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data.ReqType.Mnemonic

/**
 * Parsed filter text. May be invalid.
 */
sealed trait FilterSpec

object FilterSpec {

  sealed trait ReqsSpec
  object ReqsSpec {
    case class WholeType(reqtype: Mnemonic)                     extends ReqsSpec
    case class SomeOfType(reqtype: Mnemonic, numbers: Set[Int]) extends ReqsSpec
  }
  type Reqs = NonEmptyVector[ReqsSpec]

  // TODO Use NonEmptySets instead of NonEmptyVectors

  case class SimpleText(text: String)                       extends FilterSpec
  case class QuotedText(text: String, quoteChar: Char)      extends FilterSpec
  case class Regex     (text: String)                       extends FilterSpec
  case class ReqType   (value: Mnemonic)                    extends FilterSpec
  case class HashRef   (text: String)                       extends FilterSpec
  case class Implies   (reqs: Reqs)                         extends FilterSpec
  case class ImpliedBy (reqs: Reqs)                         extends FilterSpec
  case class Presence  (attr: String)                       extends FilterSpec
  case class Lack      (attr: String)                       extends FilterSpec
  case class AllOf     (clause: NonEmptyVector[FilterSpec]) extends FilterSpec
  case class AnyOf     (clause: NonEmptyVector[FilterSpec]) extends FilterSpec
  case class Not       (expr: FilterSpec)                   extends FilterSpec
}
