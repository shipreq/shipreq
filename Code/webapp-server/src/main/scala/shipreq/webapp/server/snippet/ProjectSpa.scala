package shipreq.webapp.server.snippet

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.server._
import shipreq.webapp.server.data.ProjectId
import shipreq.webapp.server.db.DaoT
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.lib.{SingleOpStatefulSnippet, Taskman}
import shipreq.webapp.server.protocol._
import shipreq.webapp.server.util.{KeyedMutexes, LockUsage}

object ProjectSpa {

  case class State(project: Project, seq: EventSeq)

  type LoadErrors = (String, Vector[EventSeq])

  val Mutexes = KeyedMutexes[ProjectId](LockUsage.Default)

  val RightUnit = \/-(())

  val NoChangeResponse = \/-(Vector.empty[VerifiedEvent])

  def loadErrorToString(p: ProjectId, e: LoadErrors): String = {
    val (err, seqs) = e
    val seqStr = NonEmptySet.maybe(seqs.toStream.map(_.value).toSet, "∅")(ConciseIntSetFormat.spaced)
    s"Error building project #${p.value} from DB events: $err\n\nSeqs: $seqStr"
  }

  def loadProjectEvents(dao: DaoT, projectId: ProjectId): LoadErrors \/ State = {
    val es = dao.findAllEvents(projectId)
    ApplyEvent.trusted.applyVerified(es.toStream.map(_._2))(Project.empty) match {

      case \/-(p) =>
        val seq = es.lastOption.fold(EventSeq(0))(_._1) // If empty, next will be 1. Nice to reserve 0 for ApplyTemplate.
        \/-(State(p, seq))

      case -\/(e) =>
        val seqs = es.map(_._1)
        -\/((e, seqs))
    }
  }

  def loadProjectEvents_!(dao: DaoT, projectId: ProjectId): State =
    loadProjectEvents(dao, projectId) match {
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

  var state = daoProvider.withTransaction(loadProjectEvents_!(_, projectId))

  private def updateProject(f: Project => MakeEvent.Result): GenericFailure \/ VerifiedEvents =
    mutex {
      // Thread.sleep(2000)
      // sys error "NO!"
      val event = f(state.project)
      ApplyNewEvent(event, state.project) match {

        case ValidUpdate.Success(u) =>
          saveAndApplyNewEvent(u).map(_ => Vector1(u.ve))

        case ValidUpdate.Unchanged  =>
          NoChangeResponse

        case ValidUpdate.Failure(e) =>
          System.err.println(s"Error: $event failed with $e.")
          -\/(GenericFailure(e))
      }
    }

  private def saveAndApplyNewEvent(u: ApplyNewEvent.Updated): GenericFailure \/ Unit = {
    val seq = state.seq.succ
    val s1 = state
    try {
      daoProvider.withTransaction(_.createEvent(projectId, seq, u.ae, u.ve.hashRecs))
      state = State(u.project, seq)
      RightUnit
    } catch {
      case t: Throwable =>
        val msg =
          s"""
             |Error saving new event ${u.ae} to project ${projectId.value} with seq $seq.
             |Previous state in memory has seq ${s1.seq}.
           """.stripMargin
        log.error(t, msg)
        taskman ! Taskman.errorMsg(t, S.request.toOption.map(_.uri), msg)
        -\/(GenericFailure("Error occurred writing change to database."))
    }
  }

  val initData = {
    import ServerProtocol.remoteFn

    val projectInit = remoteFn(ProjectInit)(
      _ => \/-(state.project))

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

    InitDataForProjectSpa (
      projectInit,
      customIssueTypeCrud,
      customReqTypeCrud,
      reqTypeImplicationMod,
      fieldMandatorinessMod,
      fieldCrud,
      tagCrud,
      createContent,
      updateContent)
  }

  override def render =
    "*" #> ClientFn.ProjectSpa.runOnLoadHtml(initData)
}
