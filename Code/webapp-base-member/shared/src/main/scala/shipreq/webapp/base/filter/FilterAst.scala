package shipreq.webapp.base.filter

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.recursion.Fix
import japgolly.microlibs.utils.StaticLookupFn
import scalaz.{Applicative, Traverse, Traverse1}
import shipreq.webapp.base.data.{On, ReqTypePos}
import shipreq.webapp.base.issue.IssueCategory

sealed trait FilterAst[
  +FieldCriteria[_], // 1
  +ImpCriteria  [_], // 2
  +Attr,             // 3
  +Field,            // 4
  +IssueCat,         // 5
  +HashTag,          // 6
  +ReqSet,           // 7
  +ReqType,          // 8
  +Scope,            // 9
  +Self,             // 10
]

object FilterAst {                                                                               // 1        2        3        4        5        6        7        8        9        10
  final case class Text                   (text: String, quoteChar: Option[Char]) extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Regex                  (text: String)                          extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Presence            [A](attr: A)                               extends FilterAst[Nothing, Nothing, A      , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class FieldProp  [A, F[_], B](field: A, criteria: F[B])              extends FilterAst[F      , Nothing, Nothing, A      , Nothing, Nothing, Nothing, Nothing, Nothing, B      ]
  final case class HasIssue            [A](on: On, criteria: NonEmptyVector[A])   extends FilterAst[Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class HashRef             [A](value: A)                              extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing, Nothing, Nothing]
  final case class ImpliesAnyOf  [F[_], B](criteria: F[B])                        extends FilterAst[Nothing, F      , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, B      ]
  final case class ImpliedByAnyOf[F[_], B](criteria: F[B])                        extends FilterAst[Nothing, F      , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, B      ]
  final case class Reqs                [A](reqs: A)                               extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing, Nothing]
  final case class ReqType             [A](reqType: A)                            extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      , Nothing, Nothing]
  final case class Scoped           [S, A](main: Boolean, scope: S, clause: A)    extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, S      , A      ]
  final case class Not                 [A](clause: A)                             extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AllOf               [A](clauses: NonEmptyVector[A])            extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AnyOf               [A](head: A, tail: NonEmptyVector[A])      extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, A      ]

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
    case object NotBlank      extends FieldAttr("notBlank")

    implicit def univEq: UnivEq[FieldAttr] = UnivEq.derive

    val values: NonEmptyVector[FieldAttr] =
      AdtMacros.adtValues[FieldAttr]

    override protected def namesFor(a: FieldAttr): Iterator[String] =
      Iterator.single(a.name)
  }

  sealed trait FieldCriteria[+Attr, +Query]

  object FieldCriteria {
    final case class Attr[+A](value: A)                            extends FieldCriteria[A, Nothing]
    final case class ReqTypePosSet(value: NonEmptySet[ReqTypePos]) extends FieldCriteria[Nothing, Nothing]
    final case class Query[+Q](value: Q)                           extends FieldCriteria[Nothing, Q]

    implicit def univEq[A: UnivEq, Q: UnivEq]: UnivEq[FieldCriteria[A, Q]] =
      UnivEq.derive

    def traverse[A]: Traverse[FieldCriteria[A, *]] =
      new Traverse[FieldCriteria[A, *]] {
        type F[X] = FieldCriteria[A, X]

        override def map[X, Y](fa: F[X])(f: X => Y): F[Y] = fa match {
          case c: Attr[A]       => c
          case c: ReqTypePosSet => c
          case c: Query[X]      => Query(f(c.value))
        }

        override def traverseImpl[G[_], X, Y](fa: F[X])(f: X => G[Y])(implicit G: Applicative[G]): G[F[Y]] = fa match {
          case c: Attr[A]       => G.pure(c)
          case c: ReqTypePosSet => G.pure(c)
          case c: Query[X]      => G.map(f(c.value))(Query(_))
        }
      }
  }

  sealed trait ImpCriteria[+Reqs, +Query]

  object ImpCriteria {
    final case class Reqs [+R](value: R) extends ImpCriteria[R, Nothing]
    final case class Query[+Q](value: Q) extends ImpCriteria[Nothing, Q]

    implicit def univEq[R: UnivEq, Q: UnivEq]: UnivEq[ImpCriteria[R, Q]] =
      UnivEq.derive

    def traverse[R]: Traverse[ImpCriteria[R, *]] =
      new Traverse[ImpCriteria[R, *]] {
        type F[X] = ImpCriteria[R, X]

        override def map[X, Y](fa: F[X])(f: X => Y): F[Y] = fa match {
          case c: Reqs [R] => c
          case c: Query[X] => Query(f(c.value))
        }

        override def traverseImpl[G[_], X, Y](fa: F[X])(f: X => G[Y])(implicit G: Applicative[G]): G[F[Y]] = fa match {
          case c: Reqs [R] => G.pure(c)
          case c: Query[X] => G.map(f(c.value))(Query(_))
        }
      }
  }

  sealed trait Scope[+DerivativeTagField]

  object Scope {
    final case class Derivation[+A](field: Option[A]) extends Scope[A]

    object Derivation {
      val main = "+"

      val keyword = "derivation"

      def quoteIfNecessary(s: String): String =
        if (s.contains(')'))
          "\"" + s + "\""
        else
          s
    }

    implicit def univEq[A: UnivEq]: UnivEq[Scope[A]] =
      UnivEq.derive
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

  type Fixed[A[_], B[_], C, D, E, F, G, H, I] = Fix[λ[X => FilterAst[A, B, C, D, E, F, G, H, I, X]]]

  @nowarn("cat=unused")
  def univEqFix[A[_], B[_], C: UnivEq, D: UnivEq, E: UnivEq, F: UnivEq, G: UnivEq, H: UnivEq, I: UnivEq](implicit A: UnivEq[A[Unit]], B: UnivEq[B[Unit]]): UnivEq[Fixed[A, B, C, D, E, F, G, H, I]] =
    UnivEq.deriveFix[Fix, λ[X => FilterAst[A, B, C, D, E, F, G, H, I, X]]]

  def traverse[FieldCriteria[_], ImpCriteria[_], Attr, Field, IssueCat, HashTag, ReqSet, RT, Scope](traverseFC: Traverse[FieldCriteria], traverseIC: Traverse[ImpCriteria]): Traverse[FilterAst[FieldCriteria, ImpCriteria, Attr, Field, IssueCat, HashTag, ReqSet, RT, Scope, *]] =
    new Traverse[FilterAst[FieldCriteria, ImpCriteria, Attr, Field, IssueCat, HashTag, ReqSet, RT, Scope, *]] {
      type F[A] = FilterAst[FieldCriteria, ImpCriteria, Attr, Field, IssueCat, HashTag, ReqSet, RT, Scope, A]

      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case c: Text                                    => c
        case c: Regex                                   => c
        case c: Presence      [Attr]                    => c
        case c: HasIssue      [IssueCat]                => c
        case c: HashRef       [HashTag]                 => c
        case c: ImpliesAnyOf  [ImpCriteria, A]          => c.copy(traverseIC.map(c.criteria)(f))
        case c: ImpliedByAnyOf[ImpCriteria, A]          => c.copy(traverseIC.map(c.criteria)(f))
        case c: Reqs          [ReqSet]                  => c
        case c: ReqType       [RT]                      => c
        case c: FieldProp     [Field, FieldCriteria, A] => c.copy(criteria = traverseFC.map(c.criteria)(f))
        case c: Scoped        [Scope, A]                => c.copy(clause = f(c.clause))
        case c: Not           [A]                       => Not  (f(c.clause))
        case c: AllOf         [A]                       => AllOf(c.clauses map f)
        case c: AnyOf         [A]                       => AnyOf(f(c.head), c.tail map f)
      }

      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = fa match {
        case c: Text                                    => G pure c
        case c: Regex                                   => G pure c
        case c: Presence      [Attr]                    => G pure c
        case c: HasIssue      [IssueCat]                => G pure c
        case c: HashRef       [HashTag]                 => G pure c
        case c: ImpliesAnyOf  [ImpCriteria, A]          => G.map(traverseIC.traverse(c.criteria)(f))(c.copy)
        case c: ImpliedByAnyOf[ImpCriteria, A]          => G.map(traverseIC.traverse(c.criteria)(f))(c.copy)
        case c: Reqs          [ReqSet]                  => G pure c
        case c: ReqType       [RT]                      => G pure c
        case c: FieldProp     [Field, FieldCriteria, A] => G.map(traverseFC.traverse(c.criteria)(f))(x => c.copy(criteria = x))
        case c: Scoped        [Scope, A]                => G.map(f(c.clause))(x => c.copy(clause = x))
        case c: Not           [A]                       => G.map(f(c.clause))(Not(_))
        case c: AllOf         [A]                       => G.map(Traverse1[NonEmptyVector].traverse1(c.clauses)(f))(AllOf(_))
        case c: AnyOf         [A]                       => G.apply2(f(c.head), Traverse1[NonEmptyVector].traverse1(c.tail)(f))(AnyOf(_, _))
      }
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Dsl {
    type FieldCriteriaF[_]
    type ImpCriteriaF[_]
    type Attr
    type Field
    type IssueCat
    type HashTag
    type ReqSet
    type ReqType
    type Scope

    final type F[A] = FilterAst[FieldCriteriaF, ImpCriteriaF, Attr, Field, IssueCat, HashTag, ReqSet, ReqType, Scope, A]

    final type FieldCriteria      = FieldCriteriaF[Fix[F]]
    final type ImpCriteria        = ImpCriteriaF[Fix[F]]
    final type FieldPropF[A]      = FilterAst.FieldProp[Field, FieldCriteriaF, A]
    final type ImpliesAnyOfF[A]   = FilterAst.ImpliesAnyOf[ImpCriteriaF, A]
    final type ImpliedByAnyOfF[A] = FilterAst.ImpliedByAnyOf[ImpCriteriaF, A]
    final type FieldProp          = FieldPropF[Fix[F]]
    final type ImpliesAnyOf       = ImpliesAnyOfF[Fix[F]]
    final type ImpliedByAnyOf     = ImpliedByAnyOfF[Fix[F]]
    final type Scoped             = FilterAst.Scoped[Scope, Fix[F]]

    def apply(f: F[Fix[F]]): Fix[F] =
      Fix[F](f)

    def text(text: String): Fix[F] =
      Fix[F](Text(text, None))

    def text(text: String, quoteChar: Char): Fix[F] =
      Fix[F](Text(text, Some(quoteChar)))

    def text(text: String, quoteChar: Option[Char]): Fix[F] =
      Fix[F](Text(text, quoteChar))

    def regex(text: String): Fix[F] =
      Fix[F](Regex(text))

    def presence(attr: Attr): Fix[F] =
      Fix[F](Presence(attr))

    def fieldProp(field: Field, attr: FieldCriteria): Fix[F] =
      Fix[F](FieldProp(field, attr))

    def hasIssue(on: On, h: IssueCat, t: IssueCat*): Fix[F] =
      Fix[F](HasIssue(on, NonEmptyVector(h, t.toVector)))

    def hasIssue(on: On, c: NonEmptyVector[IssueCat]): Fix[F] =
      Fix[F](HasIssue(on, c))

    def hashRef(value: HashTag): Fix[F] =
      Fix[F](HashRef(value))

    def impliesAnyOf(criteria: ImpCriteria): Fix[F] =
      Fix[F](ImpliesAnyOf(criteria))

    def impliedByAnyOf(criteria: ImpCriteria): Fix[F] =
      Fix[F](ImpliedByAnyOf(criteria))

    def reqs(reqs: ReqSet): Fix[F] =
      Fix[F](Reqs(reqs))

    def reqType(reqType: ReqType): Fix[F] =
      Fix[F](ReqType(reqType))

    def scoped(main: Boolean, scope: Scope, clause: Fix[F]): Fix[F] =
      Fix[F](Scoped(main, scope, clause))

    def not(clause: Fix[F]): Fix[F] =
      Fix[F](Not(clause))

    def allOf(c1: Fix[F], cn: Fix[F]*): Fix[F] =
      Fix[F](AllOf(NonEmptyVector(c1, cn.toVector)))

    def anyOf(c1: Fix[F], c2: Fix[F], cn: Fix[F]*): Fix[F] =
      Fix[F](AnyOf(c1, NonEmptyVector(c2, cn.toVector)))

  }

}
