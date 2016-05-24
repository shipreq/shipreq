package shipreq.webapp.client.project.lib

import japgolly.scalajs.react.extra._
import shipreq.webapp.client.project.widgets.high.ProjectWidgets
import shipreq.webapp.client.ww.api.SVG

object DataReusability extends shipreq.webapp.client.base.lib.DataReusability {

  implicit val reusabilitySVG: Reusability[SVG] =
    Reusability.caseClass

  implicit val reusabilityProjectWidgets: Reusability[ProjectWidgets] =
    Reusability.byRef

}
