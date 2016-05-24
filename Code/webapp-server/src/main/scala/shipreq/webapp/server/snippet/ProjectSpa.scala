package shipreq.webapp.server.snippet

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import scalaz.syntax.equal._
import scalaz.{-\/, \/, \/-}

import shipreq.base.util._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.{ProjectSpa => SpaFns, _}
import shipreq.webapp.base.server._
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.ProjectId
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.lib.{SingleOpStatefulSnippet, Taskman}
import shipreq.webapp.server.protocol._

object ProjectSpa extends DI with HasLogger {

  case class State(project: Project, seq: EventSeq)

  def loadProjectEvents(projectId: ProjectId): (String, Vector[EventSeq]) \/ State = {
    val es = daoProvider.withTransaction(_.findAllEvents(projectId))
    ApplyEvent.trusted.applyVerified(es.toStream.map(_._2))(Project.empty) match {

      case \/-(p) =>
        val seq = es.lastOption.fold(EventSeq(0))(_._1) // If empty, next will be 1. Nice to reserve 0 for ApplyTemplate.
        \/-(State(p, seq))

      case -\/(e) =>
        val seqs = es.map(_._1)
        -\/((e, seqs))
    }
  }

  def loadProjectEvents_!(projectId: ProjectId): State =
    loadProjectEvents(projectId) match {
      case \/-(s) => s
      case -\/((err, seqs)) =>
        val seqStr = NonEmptySet.maybe(seqs.toStream.map(_.value).toSet, "∅")(ConciseIntSetFormat.spaced)
        sys error s"Error building project #${projectId.value} from DB events: $err\n\nSeqs: $seqStr"
    }

  val rightUnit = \/-(())

  val noChangeResponse = \/-(Vector.empty[VerifiedEvent])
}


class ProjectSpa(projectId: ProjectId) extends SingleOpStatefulSnippet {
  import ProjectSpa._

  // TODO ProjectSpa needs thread-safety. Invalid hash generation is possible!!!

  // val project = RequestVars.Project.get.value

  var state = loadProjectEvents_!(projectId)

  private def updateProject(f: Project => MakeEvent.Result): GenericFailure \/ VerifiedEvents = {
//    Thread.sleep(2000)
//    sys error "NO!"
    val event = f(state.project)
    ApplyNewEvent(event, state.project) match {
      case ValidUpdate.Success(u) => applyNewEvent(u).map(_ => Vector1(u.ve))
      case ValidUpdate.Unchanged  => noChangeResponse
      case ValidUpdate.Failure(e) =>
        System.err.println(s"Error: $event failed with $e.")
        -\/(GenericFailure(e))
    }
  }

  private def applyNewEvent(u: ApplyNewEvent.Updated): GenericFailure \/ Unit = {
    val seq = state.seq.succ
    val s1 = state
    try {
      daoProvider.withTransaction(_.createEvent(projectId, seq, u.ae, u.ve.hashRecs))
      state = State(u.project, seq)
      rightUnit
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

  val spaFns = {
    val projectInit = ServerProtocol.remoteFn(ProjectInit)(
      _ => \/-(state.project))

    val customReqTypeCrud = ServerProtocol.remoteFn(CustomReqTypeCrud)(req =>
      updateProject(MakeEvent.customReqTypeCrud(req, _)))

    val reqTypeImplicationMod = ServerProtocol.remoteFn(ReqTypeImplicationMod)(req =>
      updateProject(_ => MakeEvent.reqTypeImplicationMod(req)))

    val customIssueTypeCrud = ServerProtocol.remoteFn(CustomIssueTypeCrud)(req =>
      updateProject(MakeEvent.customIssueTypeCrud(req, _)))

    val tagCrud = ServerProtocol.remoteFn(TagCrud.Fn)(req =>
      updateProject(MakeEvent.tagCrud(req, _)))

    val fieldCrud = ServerProtocol.remoteFn(FieldCrud.Fn)(req =>
      updateProject(MakeEvent.fieldCrud(req, _)))

    val fieldMandatorinessMod = ServerProtocol.remoteFn(FieldMandatorinessMod)(req =>
      updateProject(_ => MakeEvent.fieldMandatorinessMod(req)))

    val createContent = ServerProtocol.remoteFn(CreateContentFn)(req =>
      updateProject(MakeEvent.createContent(req, _)))

    val updateContent = ServerProtocol.remoteFn(UpdateContentFn)(req =>
      updateProject(MakeEvent.updateContent(req, _)))

    SpaFns(projectInit     ,
      customIssueTypeCrud  ,
      customReqTypeCrud    ,
      reqTypeImplicationMod,
      fieldMandatorinessMod,
      fieldCrud            ,
      tagCrud              ,
      createContent        ,
      updateContent        )
  }

  override def render =
    "*" #> ClientFn.ProjectSpa.runOnLoadHtml(spaFns)
}
