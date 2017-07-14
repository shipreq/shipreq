package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.traverse1._
import shipreq.base.util.{ConciseIntSetFormat, Min2Set}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{HashRefKey, Project}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.text.Grammar

/**
 * Parsed filter text. May be invalid.
 */
sealed trait PotentialFilter

object PotentialFilter {

  sealed trait ReqsSpec
  case class WholeType (reqtype: Mnemonic)                            extends ReqsSpec
  case class SomeOfType(reqtype: Mnemonic, numbers: NonEmptySet[Int]) extends ReqsSpec

  type Reqs = NonEmptyVector[ReqsSpec]

  case class SimpleText(text: String)                      extends PotentialFilter
  case class QuotedText(text: String, quoteChar: Char)     extends PotentialFilter
  case class Regex     (text: String)                      extends PotentialFilter
  case class ReqType   (value: Mnemonic)                   extends PotentialFilter
  case class HashRef   (text: HashRefKey)                  extends PotentialFilter
  case class Implies   (reqs: Reqs)                        extends PotentialFilter
  case class ImpliedBy (reqs: Reqs)                        extends PotentialFilter
  case class Presence  (attr: String)                      extends PotentialFilter
  case class Lack      (attr: String)                      extends PotentialFilter
  case class AllOf     (inner: NonEmptyVector[PotentialFilter]) extends PotentialFilter
  case class AnyOf     (inner: NonEmptyVector[PotentialFilter]) extends PotentialFilter
  case class Not       (expr: PotentialFilter)                  extends PotentialFilter

  // -------------------------------------------------------------------------------------------------------------------

  def toText(pf: PotentialFilter): String = {
    import shipreq.base.util.SafeStringOps._

    def fmtClause(clause: NonEmptyVector[PotentialFilter]): String =
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

    def fmtExpr(fs: PotentialFilter): String =
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

    pf match {
      case AllOf(inner) => fmtClause(inner)
      case _            => fmtExpr(pf)
    }
  }

  final case class Validator(run: PotentialFilter => String \/ ValidFilter)

  def validator(p: Project): Validator = Validator {
    type R = String \/ ValidFilter
    @inline implicit def autoR(a: ValidFilter): R = \/-(a)
    @inline def error(msg: String) = -\/(msg)

    val reqTypesByMnemonic = p.config.reqTypes.allByMnemonic

    def byAttr(f: ValidFilter.Attr => ValidFilter, n: String): R =
      ValidFilter.Attr(n) match {
        case Some(a) => f(a)
        case None    => error(s"Unknown attribute: '$n'. Known: ${ValidFilter.Attr.availableText}.") // English
      }

    def lookupReqType(mn: data.ReqType.Mnemonic): String \/ data.ReqType =
      reqTypesByMnemonic.get(mn) match {
        case Some(rt) => \/-(rt)
        case None     => error(s"Unknown type: '${mn.value}'") // English
      }

    def lookupReqsByType(mn: data.ReqType.Mnemonic): String \/ Vector[data.ReqId] =
      lookupReqType(mn).map(rt => p.reqs.pubids.value(rt.reqTypeId))

    val lookupReqs: ReqsSpec => String \/ ValidFilter.Reqs = {
      case SomeOfType(mn, nums) =>
        lookupReqsByType(mn).map(vec =>
          nums.foldLeft[ValidFilter.Reqs](Set.empty)((q, num) =>
            if (num > vec.length) q else q + vec(num - 1)))
      case WholeType(mn) =>
        lookupReqsByType(mn).map(_.toSet)
    }

    def byReqs(f: ValidFilter.Reqs => ValidFilter, reqs: Reqs): R =
      reqs.traverse(lookupReqs).map(sets =>
        f(sets.reduce(_ ++ _)))

    def composite(f: Min2Set[ValidFilter] => ValidFilter, specs: NonEmptyVector[PotentialFilter]): R =
      specs.traverse(translate).map( v =>
        Min2Set.maybe1(v.toNES)(identity)(f))

    def translate(pf: PotentialFilter): R =
      pf match {
        case SimpleText(text)    => ValidFilter.Text(text)
        case QuotedText(text, _) => ValidFilter.Text(text)
        case Presence(attr)      => byAttr(ValidFilter.Presence, attr)
        case Lack(attr)          => byAttr(ValidFilter.Lack, attr)
        case ReqType(mn)         => lookupReqType(mn).map(rt => ValidFilter.ReqType(rt.reqTypeId))
        case Implies(reqs)       => byReqs(ValidFilter.ImpliesAnyOf, reqs)
        case ImpliedBy(reqs)     => byReqs(ValidFilter.ImpliedByAnyOf, reqs)
        case AllOf(inner)        => composite(ValidFilter.AllOf, inner)
        case AnyOf(inner)        => composite(ValidFilter.AnyOf, inner)
        case Not(expr)           => translate(expr) map ValidFilter.Not
        case Regex(regex)        => ValidFilter.textPattern(regex)

        case HashRef(text) =>
          p.config.hashRefLookup(text) match {
            case Some(-\/(t)) => ValidFilter.Tag(t.id)
            case Some(\/-(i)) => ValidFilter.CustomIssue(i.id)
            case None         => error(s"Unknown tag or issue: '$text'") // English
          }
      }

    translate
  }

}
