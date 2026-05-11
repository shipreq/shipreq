package shipreq.webapp.client.ww.api

import org.scalajs.dom.URLSearchParams
import shipreq.webapp.base.data.ProjectCreator
import shipreq.webapp.base.util.{DomUtil, Obfuscated}

final case class WebWorkerQueryParams(creator: ProjectCreator) {
  import WebWorkerQueryParams.Key

  // Doesn't include a "?"
  def queryParamString: String = {
    val u = new URLSearchParams("")
    u.append(Key.creator, creator.userId.value)
    u.toString()
  }
}

object WebWorkerQueryParams {

  def read(): WebWorkerQueryParams =
    read(DomUtil.queryParams())

  def read(ps: URLSearchParams): WebWorkerQueryParams = {
    def get(key: String): Option[String] = Option(ps.get(key))
    def need(key: String): String = get(key).getOrElse(throw new RuntimeException(s"Query param '$key' required"))

    apply(
      creator = ProjectCreator(Obfuscated(need(Key.creator))),
    )
  }

  object Key {
    final val creator = "c"
  }
}
