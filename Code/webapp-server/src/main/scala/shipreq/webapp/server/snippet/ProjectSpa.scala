package shipreq.webapp.server.snippet

import doobie.free.connection.{delay => Effect}
import doobie.imports.ConnectionIO
import shipreq.webapp.gen.transform.ProjectSpaLoader
import shipreq.base.db.DoobieHelpers._
import japgolly.microlibs.nonempty.NonEmptyVector
import java.time.{Duration, Instant}
import net.liftweb.actor.LAPinger
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http._
import scala.xml.NodeSeq
import scalaz.{-\/, \/-}
import scalaz.effect.IO
import scalaz.syntax.all._
import shipreq.webapp.server.lib.SingleOpStatefulSnippet
//import scalaz.syntax.catchable._
import shipreq.base.util.FreeOption
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.ProjectCatalogue
import shipreq.webapp.base.event.{ActiveEvent, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec.Collection
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.logic.DB.ProjectLoad
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.{ClientFn, ServerProtocol}
import net.liftweb.util.Helpers._

object ProjectSpa {

  // TODO Currently running everything through ConnectionIO

  @inline implicit class SnippetExt_ConnectionIO[A](private val self: ConnectionIO[A]) extends AnyVal {
    def asIO             : IO[A] = DI.dbAccess.io.trans(self)
    def unsafePerformIO(): A     = asIO.unsafePerformIO()
  }

  implicit val mutableProjectStore: ProjectServer.StoreAlgebra[ConnectionIO] =
    Store.Algebra.concurrentHashMap()

  implicit val dbAlgebra: DB.Algebra[ConnectionIO] =
    new DB.Algebra[ConnectionIO] {

      override def loadProjectSummary(id: ProjectId): ConnectionIO[Option[(ProjectCatalogue.Item, UserId)]] =
        DbLogic.project.findCatalogueItemAndUserId(id)

      override def loadProject(id: ProjectId): ConnectionIO[ProjectLoad] =
        DbLogic.event.findAll2(id)

      override def saveProjectEvent(id: ProjectId, seq: EventSeq, e: ActiveEvent, hrs: Collection): ConnectionIO[Option[Throwable]] =
        DbLogic.event.create(id, seq, e, hrs).attempt.map(_.fold[Option[Throwable]](Some(_), _ => None))

      override def inDbTransaction[A](f: ConnectionIO[A]): ConnectionIO[A] =
        f.inTransaction
    }

  implicit val serverAlgebra: Server.Algebra[ConnectionIO] =
    new Server.Algebra[ConnectionIO] {

      override def remoteFn(fn: RemoteFn)(localFn: fn.Input => ConnectionIO[fn.Response]): ConnectionIO[fn.Instance] =
        Effect(ServerProtocol.remoteFn(fn)(localFn(_).asIO))

      override val now: ConnectionIO[Instant] =
        Effect(Instant.now())

      override def delay[A](f: ConnectionIO[A], d: Duration): ConnectionIO[A] =
        Effect(Thread.sleep(d.toMillis)).flatMap(_ => f)
    }

  // ProjectServer interpreter should be provided via DI
  val logic: ProjectServer[ConnectionIO] = new ProjectServer
}
// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ProjectSpaComet {
  case class TestMsg(msg: String)
  case class AddRegistrant(id: ProjectSpa.logic.RegId)
}
class ProjectSpaComet extends MessageCometActor {
  import net.liftweb.http.js.JsCmds
  import ProjectSpa._
  import ProjectSpaComet._

  def log(msg: String): Unit = {
    import Console._
    println(s"$BOLD$YELLOW[ProjectSpaComet  .${"%08X" format ProjectSpaComet.this.##}]$RESET$YELLOW $msg$RESET")
  }

  log("new()")

  override def lifespan: Box[TimeSpan] = Full(90.seconds)

  private var regId = FreeOption.empty[logic.RegId]

  def gimmeRegId: FreeOption[logic.RegId] = regId

  override protected def localShutdown(): Unit = {
    log("localShutdown()")
    try regId.forEach(logic.unregister(_).unsafePerformIO())
    finally regId = FreeOption.empty
    super.localShutdown()
  }

  override def mediumPriority: PartialFunction[Any, Unit] = {
    case AddRegistrant(id) =>
      if (running && regId.isEmpty) {
        log(s"AddRegistrant : reqId = $id")
        regId = FreeOption(id)
      } else {
        log(s"AddRegistrant : unregister $id")
        logic.unregister(id).unsafePerformIO()
      }

    case TestMsg(m) =>
      import org.apache.commons.lang3.StringEscapeUtils.escapeEcmaScript
      val cmd = s"console.log('${escapeEcmaScript(m)}');"
      log(s"Sending: $cmd")
      pushMessage(JsCmds.Run(cmd))
//      LAPinger.schedule(this, TestMsg("PING"), 3000)
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████


final class ProjectSpa(projectId: ProjectId) extends DispatchSnippet with SnippetHelpers {
  import ProjectSpa._

  def log(msg: String): Unit = {
    import Console._
    println(s"$BOLD$CYAN[ProjectSpaSnippet.${"%08X" format ProjectSpa.this.##}]$BLUE $msg$RESET")
  }

  log("new()")

//  final def render2(xhtml: NodeSeq): NodeSeq = {
//    val cometClass = ""
//    val name = ""
//    for (sess <- S.session) {
//      sess.sendCometActorMessage(cometClass, Full(name), CometName(name))
//
//      sess.findComet(cometClass, Full(name)) match {
//        case Full(c) => c.!!(123).asInstanceOf[Any]
//        case Empty => sess.setupComet(cometClass, Full(name), CometName(name))
//      }
//    }
//
//
//    <lift:comet type={cometClass} name={name}>{xhtml}</lift:comet>
//  }

  override def dispatch = { case _ => render }

//  var regId = FreeOption.empty[logic.RegId]
//
//  private def broadcast(ves: NonEmptyVector[VerifiedEvent]): ConnectionIO[Unit] =
//    Effect(println(s"TODO - ${ves.whole.mkString("[", ", ", "]")}"))
//
//  override protected def localSetup() = {
//    println("HELLO 1!")
//    super.localSetup()
//    println("HELLO 2!")
//    val x = try {
//    logic.register(projectId, currentUserId_!(), broadcast).unsafePerformIO()
//    }catch {
//      case t: Throwable =>
//        t.printStackTrace()
//        println("ERRORRRR : " + t.getMessage)
//        throw t
//    }
//    println("HELLO 3!")
//    println(s"LOCAL SETUP ${currentUserId_!()} = $x")
//    x match {
//      case \/-(ok)                            => regId = FreeOption(ok)
//      case -\/(ProjectServer.AccessDenied)    => respondImmediately(ForbiddenResponse())
//      case -\/(ProjectServer.ProjectNotFound) => respondImmediately(NotFoundResponse())
//      case -\/(b: ProjectServer.BuildError)   => respondImmediately(InternalServerErrorResponse()) // TODO do more!
//    }
//  }
//
//  override protected def localShutdown() = {
//    super.localShutdown()
//    try regId.forEach(logic.unregister(_).unsafePerformIO())
//    finally regId = FreeOption.empty
//  }

  def render: NodeSeq => NodeSeq = {

    val user = currentUser_!()

    val cometBox = S.findOrCreateComet[ProjectSpaComet](
      cometName = Full(s"project-${projectId.value}"), //: Box[String],
      cometHtml = NodeSeq.Empty, //: NodeSeq,
      cometAttributes = Map.empty, //: Map[String, String],
      receiveUpdatesOnPage = true) //: Boolean
    log("Box[Comet] = " + cometBox)
    val comet = cometBox.openOrThrowException("FUCK WHAT?")

    val regId: logic.RegId =
      comet.gimmeRegId.getOrElse {

        val x = try {
          logic.register(projectId, user.id, ve => Effect(comet ! ProjectSpaComet.TestMsg(ve.toString()))).unsafePerformIO()
        } catch {
          case t: Throwable =>
            t.printStackTrace()
            log(s"logic.register error: ${t.getMessage}")
            throw t
        }
        log(s"logic.register = $x")
        val newRegId: logic.RegId =
        x match {
          case \/-(id)                            => id
          case -\/(ProjectServer.AccessDenied)    => respondImmediately(ForbiddenResponse())
          case -\/(ProjectServer.ProjectNotFound) => respondImmediately(NotFoundResponse())
          case -\/(b: ProjectServer.BuildError)   => respondImmediately(InternalServerErrorResponse()) // TODO do more!
        }
        comet ! ProjectSpaComet.AddRegistrant(newRegId)

        newRegId
      }

    log("regId = " + regId)

    val init = logic.initialData(regId, ProjectServer.BroadcastTo.All, user.username).unsafePerformIO()

    "*" #> (
      ProjectSpaLoader.xml(user.username, init.project) :+
        ClientFn.ProjectSpa.htmlToRunOnLoad(init))
    //          ClientFn.ProjectSpa.htmlToLoadJsAndRun(Assets.ProjectSpa)(initData(user.username, p)))
  }
}
