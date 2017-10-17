package shipreq.webapp.base.filter

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import java.util.regex.Pattern
import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.base.util.{Min2Set, Min2Vector}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data

/**
 * A valid filter, ready to be applied to data.
 */
sealed trait ValidFilter

object ValidFilter {

  type ReqIds = Set[data.ReqId] // If empty, then it's instant fail for the filter.

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

  /** Non-recursive case */
  sealed trait Leaf extends ValidFilter

  final case class AllOf         (inner: Min2Set[ValidFilter]) extends ValidFilter
  final case class AnyOf         (inner: Min2Set[ValidFilter]) extends ValidFilter
  final case class Not           (expr: ValidFilter)           extends ValidFilter

  final case class Presence      (attr: Attr)                  extends Leaf
  final case class Lack          (attr: Attr)                  extends Leaf
  final case class Reqs          (reqs: ReqIds)                extends Leaf
  final case class ReqType       (id: data.ReqTypeId)          extends Leaf
  final case class Tag           (id: data.ApplicableTagId)    extends Leaf
  final case class CustomIssue   (id: data.CustomIssueTypeId)  extends Leaf
  final case class Text          (substring: String)           extends Leaf
  final case class ImpliesAnyOf  (reqs: ReqIds)                extends Leaf
  final case class ImpliedByAnyOf(reqs: ReqIds)                extends Leaf
  final case class TextPattern   (pattern: Pattern)            extends Leaf {
    override def hashCode = pattern.pattern.##
    override def equals(o: Any) = o match {
      case TextPattern(q) => (pattern.pattern ==* q.pattern) && (pattern.flags ==* q.flags)
      case _              => false
    }
  }

  implicit def univEqTextPattern: UnivEq[TextPattern] = UnivEq.force
  implicit def univEq: UnivEq[ValidFilter] = UnivEq.derive

  // -------------------------------------------------------------------------------------------------------------------

  def textPattern(regex: String): String \/ TextPattern =
    try
      \/-(TextPattern(Pattern compile regex))
    catch {
      // PatternSyntaxException not available in Scala.JS
      // case e: PatternSyntaxException => error(e.getDescription)
      case e: Throwable => -\/(s"Invalid regex: /$regex/")
    }

  /*
  def toSpec(p: data.Project, f: FilterAst): String \/ FilterSpec = {
    type R = String \/ FilterSpec
    implicit def mustToOpt(m: Must[FilterSpec]): R = m.fold(-\/.apply, \/-.apply)

    def byReqs(f: PF.Reqs => FilterSpec, reqs: Reqs): R = {
      val a: Must[Set[data.Req]] = p.reqs.data.reqsM(reqs)
      a.map(NonEmptySet.maybe(_, -\/("Empty <reqs>"): R)(rs =>
        rs.toStream.map(
      ))
    }

    def translateN(asts: NonEmptySet[FilterAst]): String \/ NonEmptySet[FilterSpec] =
      asts.traverseD(translate)

    def translate(f: FilterAst): R = {
      case Presence(a)          => PF.Presence(a.name)
      case Lack(a)              => PF.Lack(a.name)
      case ReqType(id)          => p.config.reqType        (id).map(r => PF.ReqType(r.mnemonic))
      case Tag(id)              => p.config.atag           (id).map(t => PF.HashRef(t.key))
      case CustomIssue(id)      => p.config.customIssueType(id).map(i => PF.HashRef(i.key))
      case TextPattern(pat)     => PF.Regex(pat.pattern)
      case ImpliesAnyOf(reqs)   => PF.Implies(reqs)
      case ImpliedByAnyOf(reqs) => PF.ImpliedBy(reqs)
      case AllOf(h, t)          => translateN(h +: t) map PF.AllOf
      case AnyOf(h, t)          => translateN(h +: t) map PF.AnyOf
      case Not(expr)            => translate(expr) map PF.Not

      case Text(t) =>
        def check(q: Char) = t.indexOf(q) >= 0
        (check('\''), check('"'), check('`'))  match {
          case (false, false, false) => PF.SimpleText(t)
          case (true , false, false) => PF.QuotedText(t, '\'')
          case (false, true , false) => PF.QuotedText(t, '"')
          case (false, false, true ) => PF.QuotedText(t, '`')
          case _ => -\/(s"No suitable quote character for [$t]")
        }
    }
    translate(f)
  }
  */

  /** Reorders a filter so that the quickest tests are performed first. */
  val orderFastestFirst: EndoFn[Min2Vector[ValidFilter]] = {

    def reorder(eval: ValidFilter => Int, max: Int): EndoFn[Min2Vector[ValidFilter]] = {
      // Oh the simplicity of single-threaded guarantees
      val buckets = Array.fill(max + 1)(collection.mutable.ListBuffer.empty[ValidFilter])
      as => {
        buckets.foreach(_.clear())
        for (a <- as)
          buckets(eval(a)) += a
        val all = buckets.foldLeft(Vector.empty[ValidFilter])(_ ++ _)
        Min2Vector force all
      }
    }

    @tailrec def evalSpeed(a: ValidFilter): Int =
      a match {
        case _: Presence
           | _: Lack
           | _: Reqs
           | _: ReqType
           | _: Tag
           | _: ImpliesAnyOf
           | _: ImpliedByAnyOf => 0
        case _: CustomIssue    => 2
        case _: AllOf          => 2
        case _: AnyOf          => 1
        case _: Text           => 2
        case _: TextPattern    => 3
        case Not(e)            => evalSpeed(e)
      }

    reorder(evalSpeed, 3)
  }

  def leafIterator(v: ValidFilter): Iterator[Leaf] = {
    v match {
      case l: Leaf  => Iterator.single(l)
      case x: AllOf => x.inner.iterator.flatMap(leafIterator)
      case x: AnyOf => x.inner.iterator.flatMap(leafIterator)
      case x: Not   => leafIterator(x.expr)
    }
  }

  object LeafRefs {
    val issues: ValidFilter.Leaf => List[data.CustomIssueTypeId] = {
      case x: CustomIssue    => x.id :: Nil
      case _: ImpliedByAnyOf
         | _: ImpliesAnyOf
         | _: Lack
         | _: Presence
         | _: Reqs
         | _: ReqType
         | _: Tag
         | _: Text
         | _: TextPattern    => Nil
    }

    val reqs: ValidFilter.Leaf => ReqIds = {
      case x: ImpliedByAnyOf => x.reqs
      case x: ImpliesAnyOf   => x.reqs
      case x: Reqs           => x.reqs
      case _: CustomIssue
         | _: Lack
         | _: Presence
         | _: ReqType
         | _: Tag
         | _: Text
         | _: TextPattern => Set.empty
    }

    val reqTypes: ValidFilter.Leaf => List[data.ReqTypeId] = {
      case x: ReqType        => x.id :: Nil
      case _: CustomIssue
         | _: ImpliedByAnyOf
         | _: ImpliesAnyOf
         | _: Lack
         | _: Presence
         | _: Reqs
         | _: Tag
         | _: Text
         | _: TextPattern    => Nil
    }

    val tags: ValidFilter.Leaf => List[data.ApplicableTagId] = {
      case x: Tag            => x.id :: Nil
      case _: CustomIssue
         | _: ImpliedByAnyOf
         | _: ImpliesAnyOf
         | _: Lack
         | _: Presence
         | _: Reqs
         | _: ReqType
         | _: Text
         | _: TextPattern    => Nil
    }
  }
}
