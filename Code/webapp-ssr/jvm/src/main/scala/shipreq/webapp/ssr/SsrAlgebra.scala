package shipreq.webapp.ssr

import scala.xml.XML
import shipreq.base.util.{Permission, Url}
import shipreq.webapp.base.data.Username

trait SsrAlgebra[F[_]] {

  def prepare(baseUrl: Url.Absolute.Base,
              publicRegistration: Permission): F[SsrAlgebra.Prepared[F]]
}

object SsrAlgebra {
  import SsrSharedData._

  type Output                 = Option[Html]
  type Public[F[_]]           = (Url.Relative, Option[Username]) => F[Output]
  type HomeSpaLoader[F[_]]    = HomeSpaLoaderData => F[Output]
  type ProjectSpaLoader[F[_]] = ProjectSpaLoaderData => F[Output]

  final case class Prepared[F[_]](public: Public[F],
                                  homeSpaLoader: HomeSpaLoader[F],
                                  projectSpaLoader: ProjectSpaLoader[F])

}

final case class Html(value: String) {
  val xml = XML.loadString(value)
}

object Html {
  implicit def univEq: UnivEq[Html] = UnivEq.derive
}