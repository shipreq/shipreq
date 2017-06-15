package shipreq.webapp.server.snippet

import doobie.imports._
import japgolly.microlibs.nonempty._
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import scalaz.effect.IO
import scalaz.syntax.catchable._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.gen.transform.ProjectSpaLoader
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.lib.{SingleOpStatefulSnippet, Taskman}
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol._
import shipreq.webapp.server.util.{KeyedMutexes, LockUsage}

object ProjectSpa {

  case class State(project: Project, seq: EventSeq)

  type LoadErrors = (String, Vector[EventSeq])

  val Mutexes = KeyedMutexes[ProjectId](LockUsage.Default)

  val NoChangeResponse: IO[GenericFailure \/ VerifiedEvents] = {
    val r = \/-(Vector.empty[VerifiedEvent])
    IO(r)
  }

  def loadErrorToString(p: ProjectId, e: LoadErrors): String = {
    val (err, seqs) = e
    val seqStr = NonEmptySet.maybe(seqs.toStream.map(_.value).toSet, "∅")(ConciseIntSetFormat.spaced)
    s"Error building project #${p.value} from DB events: $err\n\nSeqs: $seqStr"
  }

  def buildProject(es: Vector[(EventSeq, VerifiedEvent)]): LoadErrors \/ State =
    ApplyEvent.trusted.applyVerified(es.map(_._2))(Project.empty) match {
      case \/-(p) =>
        val seq = es.lastOption.fold(EventSeq(0))(_._1) // If empty, next will be 1. Nice to reserve 0 for ApplyTemplate.
        \/-(State(p, seq))
      case -\/(e) =>
        val seqs = es.map(_._1)
        -\/((e, seqs))
    }

  def loadProjectEvents(projectId: ProjectId): ConnectionIO[LoadErrors \/ State] =
    DbLogic.event.findAll(projectId).map(buildProject)

  def loadProjectEvents_!(projectId: ProjectId): ConnectionIO[State] =
    loadProjectEvents(projectId) map {
      case \/-(s) => s
      case -\/(e) => sys error loadErrorToString(projectId, e)
    }
}

/**
  * This works for now but doesn't handle parallelism at all.
  * Don't be fooled by the use of a keyed global mutex! That's just to prevent data corruption.
  *
  * If the same project is opened by multiple clients, each client gets a separate instance of this class and it's own
  * view on the latest current state. Not only is that a waste of memory but when client A updates the project, client
  * B isn't informed and so the next update performed by client B fails at the DB (because the event.seq is already
  * allocated). Terrible!
  *
  * Ignoring efficiency for now, in order to achieve correctness, clients would need to be aware of each others'
  * changes, or even share the server-side state. Additionally, when a client makes a change, the update should be
  * pushed to all clients. In such a case, the client should also handle conflict in the UI.
  */
class ProjectSpa(projectId: ProjectId) extends SingleOpStatefulSnippet {
  import ProjectSpa._

  val mutex = Mutexes(projectId)

  val state: LazyVar[State] =
    LazyVar.io(
      mutex.io(
        db().io.trans(loadProjectEvents_!(projectId))
      )
    )

  private def updateProject(f: Project => MakeEvent.Result): IO[GenericFailure \/ VerifiedEvents] =
    mutex.io {
      state.get.flatMap { curState =>
        // Thread.sleep(2000)
        // sys error "NO!"
        val event = f(curState.project)

        ApplyNewEvent(event, curState.project) match {

          case PotentialChange.Success(u) =>
            saveAndApplyNewEvent(curState, u) flatMap {
              case \/-(s2) => state.set(s2).map(_ => \/-(Vector1(u.ve)))
              case e@ -\/(_) => IO(e)
            }

          case PotentialChange.Unchanged =>
            NoChangeResponse

          case PotentialChange.Failure(e) =>
            IO {
              System.err.println(s"Error: $event failed with $e.")
              -\/(GenericFailure(e))
            }
        }
      }
    }

  private def saveAndApplyNewEvent(s1: State, u: ApplyNewEvent.Updated): IO[GenericFailure \/ State] = {
    val seq = s1.seq.succ
    db().io.trans(DbLogic.event.create(projectId, seq, u.ae, u.ve.hashRecs))
      .attempt
      .flatMap {
        case \/-(_) => IO(\/-(State(u.project, seq)))
        case -\/(t) =>
          val msg =
            s"""
               |Error saving new event ${u.ae} to project ${projectId.value} with seq $seq.
               |Previous state in memory has seq ${s1.seq}.
           """.stripMargin
          taskman().submitMsgAsync(Taskman.errorMsg(t, S.request.toOption.map(_.uri), msg))
            .map { _ =>
              log.error(t, msg)
              -\/(GenericFailure("Error occurred writing change to database."))
            }
      }
  }

  def initData(username: Username, project: ProjectCatalogue.Item) = {
    import ServerProtocol.remoteFn

    val projectInit = remoteFn(ProjectInit)(
      _ => state.get.map(s => \/-(s.project)))

    val customReqTypeCrud = remoteFn(CustomReqTypeCrud)(
      i => updateProject(MakeEvent.customReqTypeCrud(i, _)))

    val reqTypeImplicationMod = remoteFn(ReqTypeImplicationMod)(
      i => updateProject(_ => MakeEvent.reqTypeImplicationMod(i)))

    val customIssueTypeCrud = remoteFn(CustomIssueTypeCrud)(
      i => updateProject(MakeEvent.customIssueTypeCrud(i, _)))

    val tagCrud = remoteFn(TagCrud.Fn)(
      i => updateProject(MakeEvent.tagCrud(i, _)))

    val fieldCrud = remoteFn(FieldCrud.Fn)(
      i => updateProject(MakeEvent.fieldCrud(i, _)))

    val fieldMandatorinessMod = remoteFn(FieldMandatorinessMod)(
      i => updateProject(_ => MakeEvent.fieldMandatorinessMod(i)))

    val createContent = remoteFn(CreateContentFn)(
      i => updateProject(MakeEvent.createContent(i, _)))

    val updateContent = remoteFn(UpdateContentFn)(
      i => updateProject(MakeEvent.updateContent(i, _)))

    val projectNameSet = remoteFn(ProjectNameSetFn)(
      i => updateProject(_ => MakeEvent.projectNameSetFn(i)))

    InitDataForProjectSpa(
      username,
      project,
      projectInit,
      customIssueTypeCrud,
      customReqTypeCrud,
      reqTypeImplicationMod,
      fieldMandatorinessMod,
      fieldCrud,
      tagCrud,
      createContent,
      updateContent,
      projectNameSet)
  }

  override def render = {
    val user = currentUser_!()
    val project = db().io.trans(DbLogic.project.findCatalogueItem(user.id, projectId)).unsafePerformIO()
    project match {
      case Some(p) =>
        "*" #> (
          ProjectSpaLoader.xml(user.username, p) :+
            ClientFn.ProjectSpa.htmlToRunOnLoad(initData(user.username, p)))
//            ClientFn.ProjectSpa.htmlToLoadJsAndRun(Assets.ProjectSpa)(initData(user.username, p)))

      case None =>
        redirectHome()
    }
  }
}
