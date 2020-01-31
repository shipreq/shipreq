package shipreq.webapp.client.project.lib

import japgolly.scalajs.react._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.client.ww.api.Svg

object DataReusability extends shipreq.webapp.base.lib.DataReusability {

  implicit def reusabilitySvg: Reusability[Svg] =
    Reusability.derive

  implicit def reusabilityProjectText[C <: ProjectText.Context, A]: Reusability[ProjectText[C, A]] =
    Reusability.byRef

  implicit def reusabilityProjectWidgets[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]] =
    Reusability.byRef

  implicit def reusabilityProjectWidgetsPubidFormat[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]#PubidFormat] =
    Reusability.byRef

  implicit def reusabilityProjectWidgetsPubidFormatAny: Reusability[ProjectWidgets.AnyCtx#PubidFormat] =
    Reusability.byRef

}
