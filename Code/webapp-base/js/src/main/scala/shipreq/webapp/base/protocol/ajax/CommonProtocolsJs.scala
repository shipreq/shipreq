package shipreq.webapp.base.protocol.ajax

import japgolly.scalajs.react.CallbackTo
import org.scalajs.dom.window
import shipreq.webapp.base.data.{ProjectId, Username}

object CommonProtocolsJs {

  object Metadata {
    import CommonProtocols.Metadata._

    def client(username: Option[Username], p: Option[Project]): CallbackTo[Client] =
      CallbackTo {
        Client(
          project   = p,
          url       = window.location.href,
          userAgent = window.navigator.userAgent,
          username  = username,
        )
      }

    def client(username: Option[Username]): CallbackTo[Client] =
      client(username, None)

    def client(username: Username): CallbackTo[Client] =
      client(Some(username))

    def client(username: Username, project: CallbackTo[Project]): CallbackTo[Client] =
      project.flatMap(p => client(Some(username), Some(p)))

    def client(username: Username, projectId: ProjectId.Public): CallbackTo[Client] =
      client(Some(username), Some(Project(projectId, None, Set.empty)))
  }

}
