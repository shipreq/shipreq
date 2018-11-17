package shipreq.webapp.ssr

import scala.xml.XML
import shipreq.base.util.FxModule._
import shipreq.base.util.Url

trait SsrAlgebra[F[_]] {
  import SsrAlgebra.Types._

  val public: (Url.Absolute, PublicInitData) => F[Option[Html]]
}

object SsrAlgebra {
  object Types {
    type PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData
    val  PublicInitData = shipreq.webapp.client.public.PublicSpaProtocols.InitData

    final case class Html(value: String) {
      def toXml = XML.loadString(value)
    }
  }

  object Off extends SsrAlgebra[Fx] {
    import Types._
    private val none = Fx.pure(Option.empty[Html])
    override val public = (_, _) => none
  }
}
