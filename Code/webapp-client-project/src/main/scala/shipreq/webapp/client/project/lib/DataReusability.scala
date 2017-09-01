package shipreq.webapp.client.project.lib

import japgolly.scalajs.react.extra._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.client.ww.api.SVG

object DataReusability extends shipreq.webapp.base.lib.DataReusability {

  implicit def reusabilitySVG: Reusability[SVG] =
    Reusability.caseClass

  implicit def reusabilityProjectWidgets[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]] =
    Reusability.byRef

  implicit val reusabilityProjectWidgetsPubidFormat: Reusability[ProjectWidgets#PubidFormat] =
    Reusability.byRef

}
