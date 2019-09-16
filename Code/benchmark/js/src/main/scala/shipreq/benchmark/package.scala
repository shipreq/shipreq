package shipreq

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import shipreq.webapp.base.data.Project

package object benchmark {

  /**
    * Builder for a benchmark that accepts a project.
    */
  val projectBM = Benchmark.setup[Unit, Project](_ =>
    SampleData.project_100)

  def BASEURL = BaseUrl(dom.window.location.href.takeWhile(_ != '?'))
}
