package shipreq.webapp.client.ww

import shipreq.webapp.base.data._
import shipreq.webapp.client.ww.api.SVG

object Graphs {

  def useCaseStepFlow(id: UseCaseId, useCases: UseCases): SVG = {

    // TODO

    GraphViz.DOT("digraph G { a -> b -> c }").toSVG
  }
}
