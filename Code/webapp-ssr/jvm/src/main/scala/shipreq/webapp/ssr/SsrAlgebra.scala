package shipreq.webapp.ssr

trait SsrAlgebra[F[_]] {
  import SsrAlgebra.Types._

  // TODO Take url and userAgent too

  def public(i: PublicInitData): F[Html]
}

object SsrAlgebra {
  object Types {
    type PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData
    val  PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData

    final case class Html(value: String)
  }
}
