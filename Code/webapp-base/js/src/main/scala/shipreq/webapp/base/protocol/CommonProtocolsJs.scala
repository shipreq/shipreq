package shipreq.webapp.base.protocol

import japgolly.scalajs.react.CallbackTo
import org.scalajs.dom.window
import shipreq.webapp.base.user.Username

object CommonProtocolsJs {

  object Metadata {
    import CommonProtocols.Metadata._

    def client(username: Username, p: Option[Project]): CallbackTo[Client] =
      CallbackTo {
        Client(
          project   = p,
          url       = window.location.href,
          userAgent = window.navigator.userAgent,
          username  = username,
        )
      }

    def clientWithProject(username: Username, project: CallbackTo[Project]): CallbackTo[Client] =
      project.flatMap(p => client(username, Some(p)))

    def clientWithoutProject(username: Username): CallbackTo[Client] =
      client(username, None)
  }

}
