package shipreq.webapp.client.project.util

import japgolly.scalajs.react._
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ViewTags}
import shipreq.webapp.member.project.text.ProjectText

object DataReusability extends shipreq.webapp.member.project.util.DataReusability {

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
