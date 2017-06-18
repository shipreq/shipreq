package shipreq.webapp.server.snippet

import japgolly.microlibs.nonempty.NonEmptyVector
import net.liftweb.common.{Box, Full}
import net.liftweb.http._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scalaz.effect.IO
import scalaz.syntax.all._
import scalaz.{-\/, \/-}
import shipreq.base.util.FreeOption
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.gen.transform.ProjectSpaLoader
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.lib.SingleOpStatelessSnippet
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol._
import ProjectSpa._

object ProjectSpa {
  val EntryPoint = ClientSideProcInvoker(ProjectSpaProtocols.EntryPoint)
  val CometListener = ClientSideProcInvoker(ProjectSpaProtocols.CometListener)
}

final class ProjectSpa(projectId: ProjectId) extends SingleOpStatelessSnippet {

  override def render: NodeSeq => NodeSeq = {

    val user = currentUser_!()

    val comet: ProjectSpaComet =
      ProjectSpaComet(projectId) openOr shouldNeverHappen_!

    val logic = projectServer()

    def newRegId(): ProjectServer.RegId = {
      val register = logic.register(projectId, user.id, ve => comet.sendMsgIO(ProjectSpaComet.UpdateProject(ve)))

      val newRegId: ProjectServer.RegId =
        register.unsafePerformIO() match {
          case \/-(id)                            => id
          case -\/(ProjectServer.AccessDenied)    => respondImmediately(ForbiddenResponse())
          case -\/(ProjectServer.ProjectNotFound) => respondImmediately(NotFoundResponse())
          case -\/(_: ProjectServer.BuildError)   => shouldNeverHappen_!
        }

      comet ! ProjectSpaComet.AddRegistrant(newRegId)

      newRegId
    }

    val regId: ProjectServer.RegId =
      comet.getRegId getOrElse newRegId()

    val init: ProjectSpaProtocols.InitClient =
      logic.initialClient(regId, user.username).unsafePerformIO()

    "*" #> (
      ProjectSpaLoader.xml(user.username, init.project) :+
        EntryPoint.invokeOnLoadHtml(init))
    // ClientFn.ProjectSpa.htmlToLoadJsAndRun(Assets.ProjectSpa)(initData(user.username, p)))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ProjectSpaComet {

  /** This also attaches (subscribes) the current req to the comet */
  def apply(projectId: ProjectId): Box[ProjectSpaComet] =
    S.findOrCreateComet[ProjectSpaComet](
      cometName            = Full("p" + projectId.value),
      cometHtml            = NodeSeq.Empty,
      cometAttributes      = Map.empty,
      receiveUpdatesOnPage = true)

  sealed trait Msg
  final case class AddRegistrant(id: ProjectServer.RegId) extends Msg
  final case class UpdateProject(es: NonEmptyVector[VerifiedEvent]) extends Msg
}

/** One of these is created per session = 1/user/browser.
  * Multiple tabs in the same browser reuse this.
  */
final class ProjectSpaComet extends MessageCometActor with DI {
  import ProjectSpaComet._

  private var regId = FreeOption.empty[ProjectServer.RegId]

  def getRegId: FreeOption[ProjectServer.RegId] =
    regId

  override protected def localShutdown(): Unit = {
    for (id <- regId)
      try projectServer().unregister(id).unsafePerformIO()
      finally regId = FreeOption.empty
    super.localShutdown()
  }

  override def mediumPriority: PartialFunction[Any, Unit] = {
    case u: UpdateProject =>
      pushMessage(CometListener.invokeJsCmd(u.es))

    case AddRegistrant(id) =>
      if (regId.isEmpty && running)
        regId = FreeOption(id)
      else
        projectServer().unregister(id).attempt.unsafePerformIO()
  }

  def sendMsgIO(msg: Msg): IO[Unit] =
    IO(this ! msg)
}
