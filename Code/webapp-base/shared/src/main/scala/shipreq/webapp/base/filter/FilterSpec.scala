package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import shipreq.base.util.ConciseIntSetFormat
import shipreq.webapp.base.data.HashRefKey
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.text.Grammar

/**
 * Parsed filter text. May be invalid.
 */
sealed trait FilterSpec

object FilterSpec {

  sealed trait ReqsSpec
  case class WholeType (reqtype: Mnemonic)                            extends ReqsSpec
  case class SomeOfType(reqtype: Mnemonic, numbers: NonEmptySet[Int]) extends ReqsSpec

  type Reqs = NonEmptyVector[ReqsSpec]

  case class SimpleText(text: String)                      extends FilterSpec
  case class QuotedText(text: String, quoteChar: Char)     extends FilterSpec
  case class Regex     (text: String)                      extends FilterSpec
  case class ReqType   (value: Mnemonic)                   extends FilterSpec
  case class HashRef   (text: HashRefKey)                  extends FilterSpec
  case class Implies   (reqs: Reqs)                        extends FilterSpec
  case class ImpliedBy (reqs: Reqs)                        extends FilterSpec
  case class Presence  (attr: String)                      extends FilterSpec
  case class Lack      (attr: String)                      extends FilterSpec
  case class AllOf     (inner: NonEmptyVector[FilterSpec]) extends FilterSpec
  case class AnyOf     (inner: NonEmptyVector[FilterSpec]) extends FilterSpec
  case class Not       (expr: FilterSpec)                  extends FilterSpec

  // -------------------------------------------------------------------------------------------------------------------

  def toText(fs: FilterSpec): String = {
    import shipreq.base.util.SafeStringOps._

    def fmtClause(clause: NonEmptyVector[FilterSpec]): String =
      clause.reduceMapLeft1(fmtExpr)(_ ~ ' ' ~ _)

    def fmtReqs(r: Reqs): String =
      r.reduceMapLeft1(fmtReqsSpec)(_ ~ ',' ~ _)

    def fmtReqsSpec(r: ReqsSpec): String =
      r match {
        case WholeType (mn)     => mn.value
        case SomeOfType(mn, ns) =>
          val n =
            if (ns.tail.isEmpty)
              ns.head.toString
            else
              '{' ~ ConciseIntSetFormat.short(ns) ~ '}'
          mn.value ~ '-' ~ n
      }

    def fmtExpr(fs: FilterSpec): String =
      fs match {
        case SimpleText(text)        => text
        case QuotedText(text, qChar) => qChar ~ text ~ qChar
        case Regex     (text)        => '/' ~ text.replace("/", "\\/") ~ '/'
        case ReqType   (value)       => value
        case HashRef   (text)        => Grammar.hashRefKey.prefix ~ text
        case Implies   (reqs)        => "implies:" ~ fmtReqs(reqs)
        case ImpliedBy (reqs)        => "impliedBy:" ~ fmtReqs(reqs)
        case Presence  (attr)        => "has:" ~ attr
        case Lack      (attr)        => "no:" ~ attr
        case AllOf     (inner)       => '(' ~ fmtClause(inner) ~ ')'
        case AnyOf     (inner)       => '{' ~ fmtClause(inner) ~ '}'
        case Not       (expr)        => '-' ~ fmtExpr(expr)
      }

    fs match {
      case AllOf(inner) => fmtClause(inner)
      case _            => fmtExpr(fs)
    }
  }
}
