package shipreq.webapp.base.filter

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.recursion.Fix
import japgolly.microlibs.utils.StaticLookupFn
import scalaz.{Applicative, Traverse, Traverse1, \/}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.On
import shipreq.webapp.base.issue.IssueCategory

sealed trait FilterAst[+Attr, +Field, +FieldAttr, +IssueCat, +HashTag, +ReqSet, +ReqType, +F]

object FilterAst {
  final case class Text             (text: String, quoteChar: Option[Char]) extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Regex            (text: String)                          extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Presence      [A](attr: A)                               extends FilterAst[A      , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class FieldProp  [A, B](field: A, attr: B)                     extends FilterAst[Nothing, A      , B      , Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class HasIssue      [A](on: On, criteria: NonEmptyVector[A])   extends FilterAst[Nothing, Nothing, Nothing, A      , Nothing, Nothing, Nothing, Nothing]
  final case class HashRef       [A](value: A)                              extends FilterAst[Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing, Nothing]
  final case class ImpliesAnyOf  [A](reqs: A)                               extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing]
  final case class ImpliedByAnyOf[A](reqs: A)                               extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing]
  final case class Reqs          [A](reqs: A)                               extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing]
  final case class ReqType       [A](reqType: A)                            extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing]
  final case class Not           [A](clause: A)                             extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AllOf         [A](clauses: NonEmptyVector[A])            extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AnyOf         [A](head: A, tail: NonEmptyVector[A])      extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait EnumHelper[A] {

    protected def namesFor(a: A): Iterator[String]

    val values: NonEmptyVector[A]

    final def availableText: String =
      values.whole.map(namesFor(_).next()).mkString(", ")

    final lazy val names: Map[String, A] =
      values.iterator.flatMap(a => namesFor(a).map((_, a))).toMap

    final def apply(n: String): Option[A] =
      names.get(n.toLowerCase)
  }

  sealed abstract class Attr(final val name: String, final val additionalNames: String*)

  object Attr extends EnumHelper[Attr] {
    case object AnyIssue extends Attr("issue")
    case object AnyTag   extends Attr("tag")

    implicit def univEq: UnivEq[Attr] = UnivEq.derive

    val values: NonEmptyVector[Attr] =
      AdtMacros.adtValues[Attr]

    override protected def namesFor(a: Attr): Iterator[String] =
      Iterator.single(a.name) ++ a.additionalNames
  }

  sealed abstract class FieldAttr(final val name: String)

  object FieldAttr extends EnumHelper[FieldAttr] {
    case object Blank         extends FieldAttr("blank")
    case object DefaultInUse  extends FieldAttr("default")
    case object NotApplicable extends FieldAttr("n/a")

    implicit def univEq: UnivEq[FieldAttr] = UnivEq.derive

    val values: NonEmptyVector[FieldAttr] =
      AdtMacros.adtValues[FieldAttr]

    override protected def namesFor(a: FieldAttr): Iterator[String] =
      Iterator.single(a.name)
  }

  val issueCategoryToStr: IssueCategory => String = {
    case IssueCategory.BadData     => "bad"
    case IssueCategory.Futility    => "futile"
    case IssueCategory.MissingData => "missing"
    case IssueCategory.UserDefined => "userdef"
  }

  val issueCategoryFromStr: String => String \/ IssueCategory =
    StaticLookupFn.useMapBy(IssueCategory.values.whole)(issueCategoryToStr)
      .toEitherWithHelp(issueCategoryToStr)((k, h) => s"Unknown issue type: '$k'. Known: $h.")
      .andThen(\/.fromEither)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Fixed[A, B, C, D, E, F, G] = Fix[λ[X => FilterAst[A, B, C, D, E, F, G, X]]]

  def univEqFix[A: UnivEq, B: UnivEq, C: UnivEq, D: UnivEq, E: UnivEq, F: UnivEq, G: UnivEq]: UnivEq[Fixed[A, B, C, D, E, F, G]] =
    UnivEq.deriveFix[Fix, λ[X => FilterAst[A, B, C, D, E, F, G, X]]]

  def traverse[Attr, Field, FieldAttr, IssueCat, HashTag, ReqSet, RT]: Traverse[FilterAst[Attr, Field, FieldAttr, IssueCat, HashTag, ReqSet, RT, *]] =
    new Traverse[FilterAst[Attr, Field, FieldAttr, IssueCat, HashTag, ReqSet, RT, *]] {
      type F[A] = FilterAst[Attr, Field, FieldAttr, IssueCat, HashTag, ReqSet, RT, A]

      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case c: Text                             => c
        case c: Regex                            => c
        case c: Presence      [Attr]             => c
        case c: HasIssue      [IssueCat]         => c
        case c: HashRef       [HashTag]          => c
        case c: ImpliesAnyOf  [ReqSet]           => c
        case c: ImpliedByAnyOf[ReqSet]           => c
        case c: Reqs          [ReqSet]           => c
        case c: ReqType       [RT]               => c
        case c: FieldProp     [Field, FieldAttr] => c
        case c: Not           [A]                => Not  (f(c.clause))
        case c: AllOf         [A]                => AllOf(c.clauses map f)
        case c: AnyOf         [A]                => AnyOf(f(c.head), c.tail map f)
      }

      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = fa match {
        case c: Text                             => G pure c
        case c: Regex                            => G pure c
        case c: Presence      [Attr]             => G pure c
        case c: HasIssue      [IssueCat]         => G pure c
        case c: HashRef       [HashTag]          => G pure c
        case c: ImpliesAnyOf  [ReqSet]           => G pure c
        case c: ImpliedByAnyOf[ReqSet]           => G pure c
        case c: Reqs          [ReqSet]           => G pure c
        case c: ReqType       [RT]               => G pure c
        case c: FieldProp     [Field, FieldAttr] => G pure c
        case c: Not           [A]                => G.map(f(c.clause))(Not(_))
        case c: AllOf         [A]                => G.map(Traverse1[NonEmptyVector].traverse1(c.clauses)(f))(AllOf(_))
        case c: AnyOf         [A]                => G.apply2(f(c.head), Traverse1[NonEmptyVector].traverse1(c.tail)(f))(AnyOf(_, _))
      }
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Dsl {
    type Attr
    type Field
    type FieldAttr
    type IssueCat
    type HashTag
    type ReqSet
    type ReqType
    final type F[A] = FilterAst[Attr, Field, FieldAttr, IssueCat, HashTag, ReqSet, ReqType, A]

    def apply         (f: F[Fix[F]])                         : Fix[F] = Fix[F](f)
    def text          (text: String)                         : Fix[F] = Fix[F](Text          (text, None))
    def text          (text: String, quoteChar: Char)        : Fix[F] = Fix[F](Text          (text, Some(quoteChar)))
    def text          (text: String, quoteChar: Option[Char]): Fix[F] = Fix[F](Text          (text, quoteChar))
    def regex         (text: String)                         : Fix[F] = Fix[F](Regex         (text))
    def presence      (attr: Attr)                           : Fix[F] = Fix[F](Presence      (attr))
    def fieldProp     (field: Field, attr: FieldAttr)        : Fix[F] = Fix[F](FieldProp     (field, attr))
    def hasIssue      (on: On, h: IssueCat, t: IssueCat*)    : Fix[F] = Fix[F](HasIssue      (on, NonEmptyVector(h, t.toVector)))
    def hasIssue      (on: On, c: NonEmptyVector[IssueCat])  : Fix[F] = Fix[F](HasIssue      (on, c))
    def hashRef       (value: HashTag)                       : Fix[F] = Fix[F](HashRef       (value))
    def impliesAnyOf  (reqs: ReqSet)                         : Fix[F] = Fix[F](ImpliesAnyOf  (reqs))
    def impliedByAnyOf(reqs: ReqSet)                         : Fix[F] = Fix[F](ImpliedByAnyOf(reqs))
    def reqs          (reqs: ReqSet)                         : Fix[F] = Fix[F](Reqs          (reqs))
    def reqType       (reqType: ReqType)                     : Fix[F] = Fix[F](ReqType       (reqType))
    def not           (clause: Fix[F])                       : Fix[F] = Fix[F](Not           (clause))
    def allOf         (c1: Fix[F], cn: Fix[F]*)              : Fix[F] = Fix[F](AllOf         (NonEmptyVector(c1, cn.toVector)))
    def anyOf         (c1: Fix[F], c2: Fix[F], cn: Fix[F]*)  : Fix[F] = Fix[F](AnyOf         (c1, NonEmptyVector(c2, cn.toVector)))
  }

}
