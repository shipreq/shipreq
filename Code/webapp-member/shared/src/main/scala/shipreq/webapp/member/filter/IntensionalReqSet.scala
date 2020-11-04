package shipreq.webapp.member.filter

import scalaz.{Applicative, Traverse}

sealed trait IntensionalReqSet[+RT] {
  val reqType: RT
}
object IntensionalReqSet {

  final case class WholeType [+RT](reqType: RT)                            extends IntensionalReqSet[RT]
  final case class SomeOfType[+RT](reqType: RT, numbers: NonEmptySet[Int]) extends IntensionalReqSet[RT]

  implicit def univEq[RT: UnivEq]: UnivEq[IntensionalReqSet[RT]] = UnivEq.derive

  implicit val traverse: Traverse[IntensionalReqSet] =
    new Traverse[IntensionalReqSet] {
      // override def map[A, B](fa: IntensionalReqSet[A])(f: A => B) = fa match {
      //   case WholeType (a)    => WholeType (f(a))
      //   case SomeOfType(a, n) => SomeOfType(f(a), n)
      // }
      override def traverseImpl[G[_], A, B](fa: IntensionalReqSet[A])(f: A => G[B])(implicit G: Applicative[G]) = fa match {
        case WholeType (a)    => G.map(f(a))(WholeType (_))
        case SomeOfType(a, n) => G.map(f(a))(SomeOfType(_, n))
      }
    }
}
