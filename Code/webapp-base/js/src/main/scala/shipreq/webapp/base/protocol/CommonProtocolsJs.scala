package shipreq.webapp.base.protocol

import japgolly.scalajs.react.CallbackTo
import org.scalajs.dom.window
import shipreq.webapp.base.user.Username

object CommonProtocolsJs {

  object SubmitFeedback {
    import CommonProtocols.SubmitFeedback._

    def metadata(username: Username, p: Option[ProjectMetadata]): CallbackTo[Metadata] =
      CallbackTo {
        Metadata(
          project   = p,
          url       = window.location.href,
          userAgent = window.navigator.userAgent,
          username  = username,
        )
      }

    def metadataWithProject(username: Username, project: CallbackTo[ProjectMetadata]): CallbackTo[Metadata] =
      project.flatMap(p => metadata(username, Some(p)))

    def metadataWithoutProject(username: Username): CallbackTo[Metadata] =
      metadata(username, None)
  }

}
