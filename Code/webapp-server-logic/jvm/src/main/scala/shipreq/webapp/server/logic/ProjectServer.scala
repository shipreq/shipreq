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

  sealed abstract class BroadcastTo
  object BroadcastTo {
    case object None extends BroadcastTo
    case object AllExceptSelf extends BroadcastTo {
      def filter(self: Long): Long => Boolean = _ != self
    }
    case object All extends BroadcastTo {
      val filter: Long => Boolean = _ => true
    }
  }

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

  def debug(pid: ProjectId): F[Unit] =
    F.point(()) // store.storeGet(pid).map(x => println(" >>>>>>>>>>>>>>>> " + x))

  def register(pid: ProjectId, userId: UserId, recv: Recv[F]): F[RegistrationError \/ RegId] = {
    def initState: F[RegistrationError \/ State] =
      db.inDbTransaction(
        db.loadProjectSummary(pid).flatMap {
          case Some((summary, uid)) =>
            println(s"($summary, $uid) @ $pid | $userId")
            if (userId ==* uid)
              db.loadProject(pid).map(buildProject(_).map(b => State(uid, summary, b._1, b._2)))
            else
              F point -\/(AccessDenied)
          case None =>
            F point -\/(ProjectNotFound)
        }
      )

    debug(pid) >> register.registerAttempt(pid, recv, initState)
  }

  def unregister(r: RegId): F[Unit] =
    debug(r.key) >> register.unregister(r)

  def initialData(r: RegId, broadcastTo: BroadcastTo, username: Username): F[InitDataForProjectSpa] =
    debug(r.key) >> readState(r).flatMap(initDataForProjectSpa(r, broadcastTo, username, _))

  // TODO Handle null ↓ Should never happen but in the world of concurrency stranger things have happened
  private def readState(r: RegId): F[State] =
    register.get(r.key).map(_.getOrNull)

  private def initDataForProjectSpa(r: RegId, broadcastTo: BroadcastTo, username: Username, s: State): F[InitDataForProjectSpa] = {
    import shipreq.webapp.base.protocol._

    def updProj(mkEvent: Project => MakeEvent.Result): F[GenericFailure \/ VerifiedEvents] =
      addEvent(r, broadcastTo, mkEvent, SaveRetries).map {
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

  private def addEvent(r: RegId, broadcastTo: BroadcastTo, mkEvent: Project => MakeEvent.Result, retries: Retries): F[PotentialChange[AddEventError, VerifiedEvent]] =
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
                _ <- s2.fold(fUnit, broadcastEvents(r, broadcastTo, NonEmptyVector one updated.ve))
              } yield PotentialChange.Success(updated.ve)

            case Some(error) =>
              retries match {
                case Nil =>
                  val opsInfo = s"Error saving new event ${updated.ae} to project ${r.key.value} with seq #${eventSeq.value}."
                  F pure PotentialChange.Failure(SaveError(opsInfo, error))
                case delay :: nextRetries =>
                  val retry = addEvent(r, broadcastTo, mkEvent, nextRetries)
                  svr.delay(retry, delay)
              }
          }

        case PotentialChange.Unchanged =>
          F pure PotentialChange.Unchanged

        case PotentialChange.Failure(reason) =>
          F pure PotentialChange.Failure(EventRejected(reason))
      }
    )

  private def broadcastEvents(self: RegId,
                              broadcastTo: BroadcastTo,
                              verifiedEvents: NonEmptyVector[VerifiedEvent])
                             (state: Store.Register.Node[State, Recv[F]]): F[Unit] = {

    def go(allow: Long => Boolean): F[Unit] = {
      //s.registrants.iterator
      //  .filter(x => allow(x._1))
      //  .map(_._2(verifiedEvents))
      //  .foldLeft(fUnit)((q, n) => F.bind(q)(_ => n))

      // A little boilerplate for a little server capacity increase
      var r = fUnit
      var first = true
      var i = state.registrants
      while (i.nonEmpty) {
        if (allow(i.head._1)) {
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

    broadcastTo match {
      case BroadcastTo.All           => go(BroadcastTo.All.filter)
      case BroadcastTo.AllExceptSelf => go(BroadcastTo.AllExceptSelf.filter(self.id))
      case BroadcastTo.None          => fUnit
    }
  }

}
