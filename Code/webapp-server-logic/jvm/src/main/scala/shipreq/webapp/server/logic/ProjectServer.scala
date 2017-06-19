package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq._
import java.time.Instant
import scala.collection.immutable.SortedSet
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/, \/-, ~>}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.{Project, ProjectMetaData, Username}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import ProjectServer._
import Server.Retries

trait ProjectServer[F[_]] {
  def register(pid: ProjectId, userId: UserId, recv: Recv[F]): F[RegistrationError \/ RegId]
  def unregister(r: RegId): F[Unit]
  def initialClient(r: RegId, username: Username): F[ProjectSpaProtocols.InitData]
}

object ProjectServer {

  type RegId = Store.Register.RegId[ProjectId]

  type Recv[F[_]] = VerifiedEvent.NonEmptySeq => F[Unit]

  sealed trait RegistrationError
  case object ProjectNotFound extends RegistrationError
  case object AccessDenied extends RegistrationError
  final case class BuildError(error: String, events: SortedSet[EventOrd]) extends RegistrationError {
    def eventRange: String =
      NonEmptySet.maybe(events.map(_.value), "∅")(ConciseIntSetFormat.spaced)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * @param userId The only user with access to the project.
    *               This will change in Phase 3 when collaborative features are added.
    */
  final case class State(userId         : UserId,
                         projectMetaData: ProjectMetaData,
                         project        : Project,
                         nextOrd        : EventOrd) {

    def update(project: Project, ve: VerifiedEvent, latestOrd: EventOrd, when: Instant): State = {
      val md = projectMetaData.applyEvent(ve, when)
      State(userId, md, project, latestOrd + 1)
    }
  }

  sealed trait AddEventError
  final case class SaveError(opsInfo: String, error: Throwable) extends AddEventError
  final case class EventRejected(reason: String) extends AddEventError

  val SaveRetries: Retries =
    Server.retriesFrom(15.millis)
      .takeWhile(_.toMillis <= 3.seconds.toMillis)
      .toList

  def buildProject(load: DB.ProjectLoad): BuildError \/ (Project, EventOrd) =
    ApplyEvent.trusted.applyVerified(load.values)(Project.empty) match {
      case \/-(p) =>
        val seq = if (load.isEmpty)
          EventOrd(1) // Nice to reserve 0 for ApplyTemplate.
        else
          load.lastKey + 1
        \/-((p, seq))
      case -\/(e) =>
        -\/(BuildError(e, load.keySet))
    }

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

  def apply[D[_], F[_]](broadcastTo: BroadcastTo)
                       (implicit db: DB.Algebra[D],
                        store: StoreAlgebra[F],
                        svr: Server.Algebra[F],
                        runDB: D ~> F,
                        F: Monad[F],
                        D: Monad[D]): ProjectServer[F] =
    new ProjectServer[F] {

      type Node = Store.Register.Node[State, Recv[F]]

      val fUnit: F[Unit] = F.pure(())

      val register = new Store.Register.Dsl[F, ProjectId, State, Recv[F]]

      def register(pid: ProjectId, userId: UserId, recv: Recv[F]): F[RegistrationError \/ RegId] = {
        def initState: D[RegistrationError \/ State] =
          db.inDbTransaction(
            db.loadProjectMetaDataAndUser(pid).flatMap {
              case Some((md, uid)) =>
                if (userId ==* uid)
                  db.loadProject(pid).map(buildProject(_).map(b => State(uid, md, b._1, b._2)))
                else
                  D point -\/(AccessDenied)
              case None =>
                D point -\/(ProjectNotFound)
            }
          )

        register.registerAttempt(pid, recv, runDB(initState))
      }

      def unregister(r: RegId): F[Unit] =
        register.unregister(r)

      // TODO Handle null ↓ Should never happen but in the world of concurrency stranger things have happened
      private def readState(r: RegId): F[State] =
        register.get(r.key).map(_.getOrNull)

      def initialClient(r: RegId, username: Username): F[ProjectSpaProtocols.InitData] =
        readState(r).flatMap(initDataForProjectSpa(r, username, _))

      def initAsyncData(r: RegId): F[ProjectSpaProtocols.InitAsyncData] =
        readState(r).map(s => ProjectSpaProtocols.InitAsyncData(
          s.project, s.nextOrd - 1))

      private def initDataForProjectSpa(r: RegId, username: Username, s: State): F[ProjectSpaProtocols.InitData] = {
        import shipreq.webapp.base.protocol._
        import ProjectSpaProtocols._

        def updProj(mkEvent: Project => MakeEvent.Result): F[ErrorMsg \/ VerifiedEvent.Seq] =
          addEvent(r, mkEvent, SaveRetries).map {
            case PotentialChange.Success(es) => \/-(es)
            case PotentialChange.Unchanged   => \/-(VerifiedEvent.EmptySeq)
            case PotentialChange.Failure(e)  =>
              val msg: String = e match {
                case EventRejected(reason) => reason
                case _: SaveError          => "Something went wrong on our end trying to update the project."
              }
              -\/(ErrorMsg(msg))
          }

        import svr.{createServerSideProc => f}
        for {
          projectInit           ← f(InitAsync            )(_ => initAsyncData(r).map(\/-(_)))
          customReqTypeCrud     ← f(CustomReqTypeCrud    )(i => updProj(p ⇒ MakeEvent.customReqTypeCrud(i, p)))
          reqTypeImplicationMod ← f(ReqTypeImplicationMod)(i => updProj(_ ⇒ MakeEvent.reqTypeImplicationMod(i)))
          customIssueTypeCrud   ← f(CustomIssueTypeCrud  )(i => updProj(p ⇒ MakeEvent.customIssueTypeCrud(i, p)))
          tagCrud               ← f(TagCrud.Protocol     )(i => updProj(p ⇒ MakeEvent.tagCrud(i, p)))
          fieldCrud             ← f(FieldCrud.Protocol   )(i => updProj(p ⇒ MakeEvent.fieldCrud(i, p)))
          fieldMandatorinessMod ← f(FieldMandatorinessMod)(i => updProj(_ ⇒ MakeEvent.fieldMandatorinessMod(i)))
          createContent         ← f(CreateContent        )(i => updProj(p ⇒ MakeEvent.createContent(i, p)))
          updateContent         ← f(UpdateContent        )(i => updProj(p ⇒ MakeEvent.updateContent(i, p)))
          projectNameSet        ← f(ProjectNameSet       )(i => updProj(_ ⇒ MakeEvent.projectNameSetFn(i)))
        } yield InitData(
          username,
          s.projectMetaData,
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

      private def addEvent(r: RegId, mkEvent: Project => MakeEvent.Result, retries: Retries): F[PotentialChange[AddEventError, VerifiedEvent.NonEmptySeq]] =
        // Non-atomicity guarded by DB constraint on eventOrd
        readState(r).flatMap(s1 =>
          ApplyNewEvent(mkEvent(s1.project), s1.project) match {
            case PotentialChange.Success(updated) =>
              val ord = s1.nextOrd
              runDB(db.saveProjectEvent(r.key, ord, updated.ae, updated.ve.hashRecs)).flatMap {

                case None =>
                  val ves = VerifiedEvent.NonEmptySeq.one(ord, updated.ve)
                  for {
                    now <- svr.now
                    s2 <- store.storeValueMod(r.key)(_.modValue(_.update(updated.project, updated.ve, ord, now)))
                    _ <- s2.fold(fUnit, broadcastEvents(r, ves, _))
                  } yield PotentialChange.Success(ves)

                case Some(error) =>
                  retries match {
                    case Nil =>
                      val opsInfo = s"Error saving new event ${updated.ae} to project ${r.key.value} with ordinal #${ord.value}."
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

      val broadcastEvents: (RegId, VerifiedEvent.NonEmptySeq, Node) => F[Unit] = {
        def go(allow: Long => Boolean, verifiedEvents: VerifiedEvent.NonEmptySeq, state: Node): F[Unit] = {
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
          case BroadcastTo.All           => (_, e, s) => go(BroadcastTo.All.filter, e, s)
          case BroadcastTo.AllExceptSelf => (r, e, s) => go(BroadcastTo.AllExceptSelf.filter(r.id), e, s)
          case BroadcastTo.None          => (_, _, _) => fUnit
        }
      }
    }
}
