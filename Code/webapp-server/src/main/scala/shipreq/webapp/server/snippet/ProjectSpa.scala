package shipreq.webapp.server.snippet

import net.liftweb.common.{Box, Full}
import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scalaz.syntax.all._
import scalaz.{-\/, \/-}
import shipreq.base.util.FreeOption
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.gen.transform.ProjectSpaLoader
import shipreq.webapp.server.app.{Global, LiftDispatcher}
import shipreq.webapp.server.lib.SingleOpStatelessSnippet
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol._

object ProjectSpa extends SingleOpStatelessSnippet {

  val EntryPoint = ClientSideProcInvoker(ProjectSpaProtocols.EntryPoint)

  override def render = {
    val projectId = LiftDispatcher.ProjectIdVar.is
    assert(projectId != null, "Project SPA snippet invoked without a ProjectId")

    val user = currentUser_!()

    val comet: ProjectSpaComet =
      ProjectSpaComet(projectId) openOr shouldNeverHappen_!

    val logic = Global.logic.projectServer

    def newRegId(): ProjectServer.RegId = {
      val register = logic.register(projectId, user.id, ve => comet.sendMsgFx(ProjectSpaComet.UpdateProject(ve)))

      val newRegId: ProjectServer.RegId =
        register.unsafeRun() match {
          case \/-(id)                            => id
          case -\/(ProjectServer.AccessDenied)    => respondImmediately(ForbiddenResponse())
          case -\/(ProjectServer.ProjectNotFound) => respondImmediately(NotFoundResponse())
        }

      comet ! ProjectSpaComet.AddRegistrant(newRegId)

      newRegId
    }

    val regId: ProjectServer.RegId =
      comet.getRegId getOrElse newRegId()

    val init: ProjectSpaProtocols.InitData =
      logic.initialClient(regId, user.username).unsafeRun() match {
        case \/-(ok)                          => ok
        case -\/(ProjectServer.NotRegistered) => shouldNeverHappen_!
      }

    "*" #> (
      ProjectSpaLoader.xml(user.username, init.projectName) :+
        EntryPoint.invokeOnLoadHtml(init))
    // ClientFn.ProjectSpa.htmlToLoadJsAndRun(Assets.ProjectSpa)(initData(user.username, p)))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ProjectSpaComet {

  val CometListener = ClientSideProcInvoker(ProjectSpaProtocols.CometListener)

  /** This also attaches (subscribes) the current req to the comet */
  def apply(projectId: ProjectId): Box[ProjectSpaComet] =
    S.findOrCreateComet[ProjectSpaComet](
      cometName            = Full("p" + projectId.value),
      cometHtml            = NodeSeq.Empty,
      cometAttributes      = Map.empty,
      receiveUpdatesOnPage = true)

  sealed trait Msg
  final case class AddRegistrant(id: ProjectServer.RegId) extends Msg
  final case class UpdateProject(es: VerifiedEvent.NonEmptySeq) extends Msg
}

/** One of these is created per session = 1/user/browser.
  * Multiple tabs in the same browser reuse this.
  */
final class ProjectSpaComet extends MessageCometActor {
  import ProjectSpaComet._

  private var regId = FreeOption.empty[ProjectServer.RegId]

  def getRegId: FreeOption[ProjectServer.RegId] =
    regId

  @inline private def logic = Global.logic.projectServer

  override protected def localShutdown(): Unit = {
    for (id <- regId)
      try logic.unregister(id).unsafeRun()
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
        logic.unregister(id).attempt.unsafeRun()
  }

  def sendMsgFx(msg: Msg): Fx[Unit] =
    Fx(this ! msg)
}
