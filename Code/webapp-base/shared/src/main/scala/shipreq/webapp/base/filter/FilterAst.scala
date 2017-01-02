package shipreq.webapp.base.filter

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import java.util.regex.Pattern
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.traverse1._
import shipreq.base.util.Min2Set
import shipreq.base.util.univeq._
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
    case object AnyIssue extends Attr("issues", "issue")
    case object AnyTag   extends Attr("tags", "tag")

    implicit def univEq: UnivEq[Attr] = UnivEq.derive

    val values: NonEmptyVector[Attr] =
      AdtMacros.adtValues[Attr]

    def availableText: String =
      values.whole.map(_.name).mkString(", ")

    val names: Map[String, Attr] =
      values.foldLeft(Map.empty[String, Attr])((m, a) =>
        a.additionalNames.foldLeft(m.updated(a.name, a))(_.updated(_, a)))

    def apply(n: String): Option[Attr] =
      names.get(n.toLowerCase)
  }

  case class Presence      (attr: Attr)                 extends FilterAst
  case class Lack          (attr: Attr)                 extends FilterAst
  case class ReqType       (id: data.ReqTypeId)         extends FilterAst
  case class Tag           (id: data.ApplicableTagId)   extends FilterAst
  case class CustomIssue   (id: data.CustomIssueTypeId) extends FilterAst
  case class Text          (substring: String)          extends FilterAst
  case class ImpliesAnyOf  (reqs: Reqs)                 extends FilterAst
  case class ImpliedByAnyOf(reqs: Reqs)                 extends FilterAst
  case class AllOf         (inner: Min2Set[FilterAst])  extends FilterAst
  case class AnyOf         (inner: Min2Set[FilterAst])  extends FilterAst
  case class Not           (expr: FilterAst)            extends FilterAst

  case class TextPattern(pattern: Pattern) extends FilterAst {
    override def hashCode = pattern.pattern.##
    override def equals(o: Any) = o match {
      case TextPattern(q) => (pattern.pattern ==* q.pattern) && (pattern.flags ==* q.flags)
      case _              => false
    }
  }
  implicit def univEqTextPattern: UnivEq[TextPattern] = UnivEq.force

  implicit def univEq: UnivEq[FilterAst] = UnivEq.derive

  // -------------------------------------------------------------------------------------------------------------------

  def textPattern(regex: String): String \/ TextPattern =
    try
      \/-(TextPattern(Pattern compile regex))
    catch {
      // PatternSyntaxException not available in Scala.JS
      // case e: PatternSyntaxException => error(e.getDescription)
      case e: Throwable => -\/(s"Invalid regex: /$regex/")
    }

  def apply(p: data.Project, filterSpec: FilterSpec): String \/ FilterAst = {
    type R = String \/ FilterAst
    @inline implicit def autoR(a: FilterAst): R = \/-(a)
    @inline def error(msg: String) = -\/(msg)

    val reqTypesByMnemonic = p.config.reqTypes.allByMnemonic

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
      lookupReqType(mn).map(rt => p.reqs.pubids.value(rt.reqTypeId))

    val lookupReqs: S.ReqsSpec => String \/ Reqs = {
      case S.SomeOfType(mn, nums) =>
        lookupReqsByType(mn).map(vec =>
          nums.foldLeft[Reqs](Set.empty)((q, num) =>
            if (num > vec.length) q else q + vec(num - 1)))
      case S.WholeType(mn) =>
        lookupReqsByType(mn).map(_.toSet)
    }

    def byReqs(f: Reqs => FilterAst, reqs: S.Reqs): R =
      reqs.traverseU(lookupReqs).map(sets =>
        f(sets.reduce(_ ++ _)))

    def composite(f: Min2Set[FilterAst] => FilterAst, specs: NonEmptyVector[FilterSpec]): R =
      specs.traverseU(translate).map( v =>
        Min2Set.maybe1(v.toNES)(identity)(f))

    def translate(spec: FilterSpec): R =
      spec match {
        case S.SimpleText(text)    => Text(text)
        case S.QuotedText(text, _) => Text(text)
        case S.Presence(attr)      => byAttr(Presence, attr)
        case S.Lack(attr)          => byAttr(Lack, attr)
        case S.ReqType(mn)         => lookupReqType(mn).map(rt => ReqType(rt.reqTypeId))
        case S.Implies(reqs)       => byReqs(ImpliesAnyOf, reqs)
        case S.ImpliedBy(reqs)     => byReqs(ImpliedByAnyOf, reqs)
        case S.AllOf(inner)        => composite(AllOf, inner)
        case S.AnyOf(inner)        => composite(AnyOf, inner)
        case S.Not(expr)           => translate(expr) map Not
        case S.Regex(regex)        => textPattern(regex)

        case S.HashRef(text) =>
          p.config.hashRefLookup(text) match {
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

    def translateN(asts: NonEmptySet[FilterAst]): String \/ NonEmptySet[FilterSpec] =
      asts.traverseD(translate)

    def translate(f: FilterAst): R = {
      case Presence(a)          => S.Presence(a.name)
      case Lack(a)              => S.Lack(a.name)
      case ReqType(id)          => p.config.reqType        (id).map(r => S.ReqType(r.mnemonic))
      case Tag(id)              => p.config.atag           (id).map(t => S.HashRef(t.key))
      case CustomIssue(id)      => p.config.customIssueType(id).map(i => S.HashRef(i.key))
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
