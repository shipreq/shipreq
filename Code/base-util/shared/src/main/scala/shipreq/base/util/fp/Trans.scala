package shipreq.base.util.fp

@scala.annotation.showAsInfix
trait ~~>[-F[_, _], +G[_, _]] {
  def apply[A, B](f: F[A, B]): G[A, B]
}
