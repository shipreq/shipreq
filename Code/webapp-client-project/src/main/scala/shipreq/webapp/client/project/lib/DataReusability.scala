package shipreq.webapp.client.project.lib

import japgolly.scalajs.react._
import shipreq.webapp.member.text.ProjectText
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ViewTags}

object DataReusability extends shipreq.webapp.base.lib.DataReusability {

  implicit def reusabilityProjectText[C <: ProjectText.Context, A]: Reusability[ProjectText[C, A]] =
    Reusability.byRef

  implicit def reusabilityProjectWidgets[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]] =
    Reusability.byRef

  implicit def reusabilityViewTagsForReq[A]: Reusability[ViewTags.ForReq[A]] =
    Reusability.byRef

  implicit def reusabilityProjectWidgetsPubidFormat[C <: ProjectText.Context]: Reusability[ProjectWidgets[C]#PubidFormat] =
    Reusability.byRef

  implicit def reusabilityProjectWidgetsPubidFormatAny: Reusability[ProjectWidgets.AnyCtx#PubidFormat] =
    Reusability.byRef

}
