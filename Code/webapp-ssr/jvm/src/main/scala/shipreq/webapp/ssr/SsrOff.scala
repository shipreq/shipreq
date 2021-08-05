package shipreq.webapp.ssr

import cats.Applicative
import shipreq.base.util.{Permission, Url}

final class SsrOff[F[_]]()(implicit F: Applicative[F]) extends SsrAlgebra[F] {

  override def prepare(b: Url.Absolute.Base, p: Permission) =
    F.point(SsrOff.prepared)
}

object SsrOff {

  def prepared[F[_]](implicit F: Applicative[F]): SsrAlgebra.Prepared[F] = {
    val none = F.pure(Option.empty[Html])
    SsrAlgebra.Prepared[F]((_, _) => none, _ => none, _ => none)
  }
}