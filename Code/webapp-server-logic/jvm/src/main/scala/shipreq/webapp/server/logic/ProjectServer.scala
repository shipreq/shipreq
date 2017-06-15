package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.univeq._
import java.time.Instant
import scala.collection.immutable.SortedSet
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.{Project, ProjectCatalogue, Username}
import shipreq.webapp.base.event.{ApplyEvent, VerifiedEvent, VerifiedEvents}
import shipreq.webapp.base.protocol.InitDataForProjectSpa
import Server.Retries

object ProjectServer {

  /**
    * @param userId The only user with access to the project.
    *               This will change in Phase 3 when collaborative features are added.
    */
  final case class State(userId : UserId,
                         summary: ProjectCatalogue.Item,
                         project: Project,
                         nextSeq: EventSeq) {

    def update(project: Project, latestSeq: EventSeq, updatedAt: Instant): State = {
      val newSummary = ProjectCatalogue.Item(
        id            = summary.id,
        name          = summary.name,
        eventCount    = summary.eventCount + 1,
        reqCount      = project.reqs.size,
        createdAt     = summary.createdAt,
        lastUpdatedAt = Some(updatedAt))
      State(userId, newSummary, project, latestSeq.succ)
    }
  }

  sealed trait RegistrationError
  case object ProjectNotFound extends RegistrationError
  case object AccessDenied extends RegistrationError
  final case class BuildError(error: String, events: SortedSet[EventSeq]) extends RegistrationError {
    def eventRange: String =
      NonEmptySet.maybe(events.map(_.value), "∅")(ConciseIntSetFormat.spaced)
  }

  sealed trait AddEventError
  final case class SaveError(opsInfo: String, error: Throwable) extends AddEventError
  final case class EventRejected(reason: String) extends AddEventError

  type Recv[F[_]] = NonEmptyVector[VerifiedEvent] => F[Unit]

  type StoreAlgebra[F[_]] = Store.Register.Algebra[F, ProjectId, State, Recv[F]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def buildProject(load: DB.ProjectLoad): BuildError \/ (Project, EventSeq) =
    ApplyEvent.trusted.applyVerified(load.values)(Project.empty) match {
      case \/-(p) =>
        val seq = if (load.isEmpty)
          EventSeq(1) // Nice to reserve 0 for ApplyTemplate.
        else
          load.lastKey.succ
        \/-((p, seq))
      case -\/(e) =>
        -\/(BuildError(e, load.keySet))
    }


  val SaveRetries: Retries =
    Server.retriesFrom(15.millis)
      .takeWhile(_.toMillis <= 3.seconds.toMillis)
      .toList
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

import ProjectServer._

final class ProjectServer[F[_]](implicit db   : DB.Algebra[F],
                                         store: StoreAlgebra[F],
                                         svr  : Server.Algebra[F],
                                         F    : Monad[F]) {

  private val fUnit: F[Unit] = F.pure(())

  private val register = new Store.Register.Dsl[F, ProjectId, State, Recv[F]]

  type RegId = Store.Register.RegId[ProjectId]

  def register(pid: ProjectId, userId: UserId, recv: Recv[F]): F[RegistrationError \/ RegId] = {
    def initState: F[RegistrationError \/ State] =
      db.inDbTransaction(
        db.loadProjectSummary(pid).flatMap {
          case Some((uid, summary)) =>
            if (userId ==* uid)
              db.loadProject(pid).map(buildProject(_).map(b => State(uid, summary, b._1, b._2)))
            else
              F point -\/(AccessDenied)
          case None =>
            F point -\/(ProjectNotFound)
        }
      )

    register.registerAttempt(pid, recv, initState)
  }

  def unregister(r: RegId): F[Unit] =
    register.unregister(r)

  // TODO Handle null ↓ Should never happen but in the world of concurrency stranger things have happened
  private def readState(r: RegId): F[State] =
    register.get(r.key).map(_.getOrNull)

  private def initDataForProjectSpa(r: RegId, username: Username, s: State): F[InitDataForProjectSpa] = {
    import shipreq.webapp.base.protocol._

    def updProj(mkEvent: Project => MakeEvent.Result): F[GenericFailure \/ VerifiedEvents] =
      addEvent(r, mkEvent, SaveRetries).map {
        case PotentialChange.Success(ve) => \/-(Vector1(ve))
        case PotentialChange.Unchanged   => \/-(Vector.empty)
        case PotentialChange.Failure(e)  =>
          val msg: String = e match {
            case EventRejected(reason) => reason
            case _: SaveError          => "Something went wrong on our end trying to update the project."
          }
          -\/(GenericFailure(msg))
      }

    import svr.{remoteFn => f}
    for {
      projectInit           ← f(ProjectInit          )(_ => readState(r).map(s => \/-(s.project)))
      customReqTypeCrud     ← f(CustomReqTypeCrud    )(i => updProj(p ⇒ MakeEvent.customReqTypeCrud(i, p)))
      reqTypeImplicationMod ← f(ReqTypeImplicationMod)(i => updProj(_ ⇒ MakeEvent.reqTypeImplicationMod(i)))
      customIssueTypeCrud   ← f(CustomIssueTypeCrud  )(i => updProj(p ⇒ MakeEvent.customIssueTypeCrud(i, p)))
      tagCrud               ← f(TagCrud.Fn           )(i => updProj(p ⇒ MakeEvent.tagCrud(i, p)))
      fieldCrud             ← f(FieldCrud.Fn         )(i => updProj(p ⇒ MakeEvent.fieldCrud(i, p)))
      fieldMandatorinessMod ← f(FieldMandatorinessMod)(i => updProj(_ ⇒ MakeEvent.fieldMandatorinessMod(i)))
      createContent         ← f(CreateContentFn      )(i => updProj(p ⇒ MakeEvent.createContent(i, p)))
      updateContent         ← f(UpdateContentFn      )(i => updProj(p ⇒ MakeEvent.updateContent(i, p)))
      projectNameSet        ← f(ProjectNameSetFn     )(i => updProj(_ ⇒ MakeEvent.projectNameSetFn(i)))
    } yield InitDataForProjectSpa(
      username,
      s.summary,
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

  def initialData(r: RegId, username: Username): F[InitDataForProjectSpa] =
    readState(r).flatMap(initDataForProjectSpa(r, username, _))

  private def addEvent(r: RegId, mkEvent: Project => MakeEvent.Result, retries: Retries): F[PotentialChange[AddEventError, VerifiedEvent]] =
    // Non-atomicity guarded by DB constraint on eventSeq
    readState(r).flatMap(s1 =>
      ApplyNewEvent(mkEvent(s1.project), s1.project) match {
        case PotentialChange.Success(updated) =>
          val eventSeq = s1.nextSeq
          db.saveProjectEvent(r.key, eventSeq, updated.ae, updated.ve.hashRecs).flatMap {

            case None =>
              for {
                now <- svr.now
                s2 <- store.storeValueMod(r.key)(_.modValue(_.update(updated.project, eventSeq, now)))
                _ <- s2.fold(fUnit, broadcastEvents(r, NonEmptyVector one updated.ve))
              } yield PotentialChange.Success(updated.ve)

            case Some(error) =>
              retries match {
                case Nil =>
                  val opsInfo = s"Error saving new event ${updated.ae} to project ${r.key.value} with seq #${eventSeq.value}."
                  F pure PotentialChange.Failure(SaveError(opsInfo, error))
                case delay :: nextRetries =>
                  val retry = addEvent(r, mkEvent, nextRetries)
                  svr.delay(retry, delay)
              }
          }

        case PotentialChange.Unchanged =>
          F pure PotentialChange.Unchanged

        case PotentialChange.Failure(reason) =>
          F pure PotentialChange.Failure(EventRejected(reason))
      }
    )

  private def broadcastEvents(exclude: RegId, verifiedEvents: NonEmptyVector[VerifiedEvent])
                             (s: Store.Register.Node[State, Recv[F]]): F[Unit] = {

    //s.registrants.iterator
    //  .filter(_._1 !=* exclude.id)
    //  .map(_._2(verifiedEvents))
    //  .foldLeft(fUnit)((q, n) => F.bind(q)(_ => n))

    // A little boilerplate for a little server capacity increase
    var r = fUnit
    var first = true
    var i = s.registrants
    val excludeId = exclude.id
    while (i.nonEmpty) {
      // Long explicitness ↓ to protect against != unsafeness. (Not sure if UnivEq boxes or not)
      if ((i.head._1: Long) != (excludeId: Long)) {
        val broadcast = i.head._2(verifiedEvents)
        if (first) {
          r = broadcast
          first = false
        } else
          r = F.bind(r)(_ => broadcast)
      }
      i = i.tail
    }
    r
  }

}
