package shipreq.webapp.base.filter

import java.util.regex.Pattern // PatternSyntaxException not available in Scala.JS
import scalaz.{-\/, \/-, \/}
import shipreq.base.util.{UnivEq, NonEmptyVector}
import shipreq.webapp.base.data
import shipreq.webapp.base.filter.{FilterSpec => S}

/**
 * A valid filter, ready to be applied to data.
 */
sealed trait FilterAst

object FilterAst {

  type Reqs = Set[data.ReqId] // If empty, then it's instant fail for the filter.

  sealed abstract class Attr(val name: String, val additionalNames: String*)
  object Attr {
    case object AnyIssue extends Attr("issue", "issues")
    case object AnyTag   extends Attr("tag", "tags")

    val values: NonEmptyVector[Attr] =
      NonEmptyVector(AnyTag, AnyIssue)

    def availableText: String =
      values.whole.map(_.name).mkString(", ")

    val names: Map[String, Attr] =
      values.foldLeft(Map.empty[String, Attr])((m, a) =>
        a.additionalNames.foldLeft(m.updated(a.name, a))(_.updated(_, a)))

    def apply(n: String): Option[Attr] =
      names.get(n.toLowerCase)
  }

  case class Presence      (attr: Attr)                                       extends FilterAst
  case class Lack          (attr: Attr)                                       extends FilterAst
  case class ReqType       (id: data.ReqTypeId)                               extends FilterAst
  case class Tag           (id: data.ApplicableTagId)                         extends FilterAst
  case class CustomIssue   (id: data.CustomIssueTypeId)                       extends FilterAst
  case class Text          (substring: String)                                extends FilterAst
  case class ImpliesAnyOf  (reqs: Reqs)                                       extends FilterAst
  case class ImpliedByAnyOf(reqs: Reqs)                                       extends FilterAst
  case class AllOf         (head: FilterAst, tail: NonEmptyVector[FilterAst]) extends FilterAst
  case class AnyOf         (head: FilterAst, tail: NonEmptyVector[FilterAst]) extends FilterAst
  case class Not           (expr: FilterAst)                                  extends FilterAst

  case class TextPattern(pattern: Pattern) extends FilterAst {
    override def hashCode = pattern.pattern.##
    override def equals(o: Any) = o match {
      case TextPattern(q) => (pattern.pattern == q.pattern) && (pattern.flags == q.flags)
      case _              => false
    }
  }

  implicit def equality: UnivEq[FilterAst] = UnivEq.force

  // -------------------------------------------------------------------------------------------------------------------

  def apply(p: data.Project, filterSpec: FilterSpec): String \/ FilterAst = {
    type R = String \/ FilterAst
    @inline implicit def autoR(a: FilterAst): R = \/-(a)
    @inline def error(msg: String) = -\/(msg)

    val reqTypesByMnemonic = p.reqTypesByMnemonic

    def byAttr(f: Attr => FilterAst, n: String): R =
      Attr(n) match {
        case Some(a) => f(a)
        case None    => error(s"Unknown attribute: '$n'. Known: ${Attr.availableText}.") // English
      }

    def lookupReqType(mn: data.ReqType.Mnemonic): String \/ data.ReqType =
      reqTypesByMnemonic.get(mn) match {
        case Some(rt) => \/-(rt)
        case None     => error(s"Unknown type: '${mn.value}'") // English
      }

    def lookupReqsByType(mn: data.ReqType.Mnemonic): String \/ Vector[data.ReqId] =
      lookupReqType(mn).map(rt => p.reqs.data.pubids.value(rt.reqTypeId))

    val lookupReqs: S.ReqsSpec => String \/ Reqs = {
      case S.SomeOfType(mn, nums) =>
        lookupReqsByType(mn).map(vec =>
          nums.foldLeft[Reqs](Set.empty)((q, num) =>
            if (num > vec.length) q else q + vec(num - 1)))
      case S.WholeType(mn) =>
        lookupReqsByType(mn).map(_.toSet)
    }

    def byReqs(f: Reqs => FilterAst, reqs: S.Reqs): R =
      reqs.traverseD(lookupReqs).map(sets =>
        f(sets.reduce(_ ++ _)))

    def composite(f: (FilterAst, NonEmptyVector[FilterAst]) => FilterAst, specs: NonEmptyVector[FilterSpec]): R =
      specs.traverseD(translate).map(asts =>
        NonEmptyVector.maybe(asts.tail, asts.head)(f(asts.head, _)))

    def translate(spec: FilterSpec): R =
      spec match {
        case S.SimpleText(text)    => Text(text)
        case S.QuotedText(text, _) => Text(text)
        case S.Presence(attr)      => byAttr(Presence, attr)
        case S.Lack(attr)          => byAttr(Lack, attr)
        case S.ReqType(mn)         => lookupReqType(mn).map(rt => ReqType(rt.reqTypeId))
        case S.Implies(reqs)       => byReqs(ImpliesAnyOf, reqs)
        case S.ImpliedBy(reqs)     => byReqs(ImpliedByAnyOf, reqs)
        case S.AllOf(clause)       => composite(AllOf, clause)
        case S.AnyOf(clause)       => composite(AnyOf, clause)
        case S.Not(expr)           => translate(expr) map Not

        case S.Regex(regex) =>
          try TextPattern(Pattern compile regex) catch {
            // case e: PatternSyntaxException => error(e.getDescription)
            case e: Throwable => error(e.getMessage)
          }

        case S.HashRef(text) =>
          p.hashRefLookup(text) match {
            case Some(-\/(t)) => Tag(t.id)
            case Some(\/-(i)) => CustomIssue(i.id)
            case None         => error(s"Unknown tag or issue: '$text'") // English
          }
      }

    translate(filterSpec)
  }

  /*
  def toSpec(p: data.Project, f: FilterAst): String \/ FilterSpec = {
    type R = String \/ FilterSpec
    implicit def mustToOpt(m: Must[FilterSpec]): R = m.fold(-\/.apply, \/-.apply)

    def byReqs(f: S.Reqs => FilterSpec, reqs: Reqs): R = {
      val a: Must[Set[data.Req]] = p.reqs.data.reqsM(reqs)
      a.map(NonEmptySet.maybe(_, -\/("Empty <reqs>"): R)(rs =>
        rs.toStream.map(
      ))
    }

    def translateN(asts: NonEmptyVector[FilterAst]): String \/ NonEmptyVector[FilterSpec] =
      asts.traverseD(translate)

    def translate(f: FilterAst): R = {
      case Presence(a)          => S.Presence(a.name)
      case Lack(a)              => S.Lack(a.name)
      case ReqType(id)          => p.reqType        (id).map(r => S.ReqType(r.mnemonic))
      case Tag(id)              => p.atag           (id).map(t => S.HashRef(t.key))
      case CustomIssue(id)      => p.customIssueType(id).map(i => S.HashRef(i.key))
      case TextPattern(pat)     => S.Regex(pat.pattern)
      case ImpliesAnyOf(reqs)   => S.Implies(reqs)
      case ImpliedByAnyOf(reqs) => S.ImpliedBy(reqs)
      case AllOf(h, t)          => translateN(h +: t) map S.AllOf
      case AnyOf(h, t)          => translateN(h +: t) map S.AnyOf
      case Not(expr)            => translate(expr) map S.Not

      case Text(t) =>
        def check(q: Char) = t.indexOf(q) >= 0
        (check('\''), check('"'), check('`'))  match {
          case (false, false, false) => S.SimpleText(t)
          case (true , false, false) => S.QuotedText(t, '\'')
          case (false, true , false) => S.QuotedText(t, '"')
          case (false, false, true ) => S.QuotedText(t, '`')
          case _ => -\/(s"No suitable quote character for [$t]")
        }
    }
    translate(f)
  }
  */
}
