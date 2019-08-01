package shipreq.webapp.base.filter

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.recursion.Fix
import scalaz.{Applicative, Traverse, Traverse1}
import shipreq.base.util.univeq._

sealed trait FilterAst[+Attr, +HashTag, +ReqSet, +ReqType, +F]

object FilterAst {
  final case class Text             (text: String, quoteChar: Option[Char]) extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Regex            (text: String)                          extends FilterAst[Nothing, Nothing, Nothing, Nothing, Nothing]
  final case class Presence      [A](attr: A)                               extends FilterAst[A      , Nothing, Nothing, Nothing, Nothing]
  final case class HashRef       [A](value: A)                              extends FilterAst[Nothing, A      , Nothing, Nothing, Nothing]
  final case class ImpliesAnyOf  [A](reqs: A)                               extends FilterAst[Nothing, Nothing, A      , Nothing, Nothing]
  final case class ImpliedByAnyOf[A](reqs: A)                               extends FilterAst[Nothing, Nothing, A      , Nothing, Nothing]
  final case class Reqs          [A](reqs: A)                               extends FilterAst[Nothing, Nothing, A      , Nothing, Nothing]
  final case class ReqType       [A](reqType: A)                            extends FilterAst[Nothing, Nothing, Nothing, A      , Nothing]
  final case class Not           [A](clause: A)                             extends FilterAst[Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AllOf         [A](clauses: NonEmptyVector[A])            extends FilterAst[Nothing, Nothing, Nothing, Nothing, A      ]
  final case class AnyOf         [A](head: A, tail: NonEmptyVector[A])      extends FilterAst[Nothing, Nothing, Nothing, Nothing, A      ]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Fixed[A, B, C, D] = Fix[λ[F => FilterAst[A, B, C, D, F]]]

  def univEqFix[A: UnivEq, B: UnivEq, C: UnivEq, D: UnivEq]: UnivEq[Fixed[A, B, C, D]] =
    UnivEq.deriveFix[Fix, λ[F => FilterAst[A, B, C, D, F]]]

  def traverse[Attr, HashTag, ReqSet, RT]: Traverse[FilterAst[Attr, HashTag, ReqSet, RT, ?]] =
    new Traverse[FilterAst[Attr, HashTag, ReqSet, RT, ?]] {
      type F[A] = FilterAst[Attr, HashTag, ReqSet, RT, A]

      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case c: Text                    => c
        case c: Regex                   => c
        case c: Presence      [Attr]    => c
        case c: HashRef       [HashTag] => c
        case c: ImpliesAnyOf  [ReqSet]  => c
        case c: ImpliedByAnyOf[ReqSet]  => c
        case c: Reqs          [ReqSet]  => c
        case c: ReqType       [RT]      => c
        case c: Not           [A]       => Not  (f(c.clause))
        case c: AllOf         [A]       => AllOf(c.clauses map f)
        case c: AnyOf         [A]       => AnyOf(f(c.head), c.tail map f)
      }

      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = fa match {
        case c: Text                    => G pure c
        case c: Regex                   => G pure c
        case c: Presence      [Attr]    => G pure c
        case c: HashRef       [HashTag] => G pure c
        case c: ImpliesAnyOf  [ReqSet]  => G pure c
        case c: ImpliedByAnyOf[ReqSet]  => G pure c
        case c: Reqs          [ReqSet]  => G pure c
        case c: ReqType       [RT]      => G pure c
        case c: Not           [A]       => G.map(f(c.clause))(Not(_))
        case c: AllOf         [A]       => G.map(Traverse1[NonEmptyVector].traverse1(c.clauses)(f))(AllOf(_))
        case c: AnyOf         [A]       => G.apply2(f(c.head), Traverse1[NonEmptyVector].traverse1(c.tail)(f))(AnyOf(_, _))
      }
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Dsl {
    type Attr
    type HashTag
    type ReqSet
    type ReqType
    final type F[A] = FilterAst[Attr, HashTag, ReqSet, ReqType, A]

    def apply         (f: F[Fix[F]])                         : Fix[F] = Fix[F](f)
    def text          (text: String)                         : Fix[F] = Fix[F](Text          (text, None))
    def text          (text: String, quoteChar: Char)        : Fix[F] = Fix[F](Text          (text, Some(quoteChar)))
    def text          (text: String, quoteChar: Option[Char]): Fix[F] = Fix[F](Text          (text, quoteChar))
    def regex         (text: String)                         : Fix[F] = Fix[F](Regex         (text))
    def presence      (attr: Attr)                           : Fix[F] = Fix[F](Presence      (attr))
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
