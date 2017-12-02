package shipreq.webapp.client.project.lib

import japgolly.scalajs.react.extra._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.client.ww.api.Svg

object DataReusability extends shipreq.webapp.base.lib.DataReusability {

  implicit def reusabilitySvg: Reusability[Svg] =
    Reusability.caseClass

  implicit def reusabilityProjectWidgets_ : Reusability[ProjectWidgets.AnyCtx] =
    Reusability.byRef

  implicit def reusabilityProjectWidgets[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]] =
    Reusability.byRef

  implicit def reusabilityProjectWidgetsPubidFormat[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]#PubidFormat] =
    Reusability.byRef

}
