package shipreq.webapp.ssr

import scala.xml.XML
import shipreq.base.util.FxModule._
import shipreq.base.util.Url

trait SsrAlgebra[F[_]] {
  import SsrAlgebra.Types._

  def warmup: F[Unit]

  val public: (Url.Absolute, PublicInitData) => F[Option[Html]]

  val projectSpaLoader: ProjectSpaLoaderData => F[Option[Html]]
}

object SsrAlgebra {
  object Types {
    final case class Html(value: String) {
      def toXml = XML.loadString(value)
    }

    type PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData
    val  PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData
  }

  object Off extends SsrAlgebra[Fx] {
    import Types._
    private val none = Fx.pure(Option.empty[Html])
    override def warmup = Fx.unit
    override val public = (_, _) => none
    override val projectSpaLoader = _ => none
  }
}
