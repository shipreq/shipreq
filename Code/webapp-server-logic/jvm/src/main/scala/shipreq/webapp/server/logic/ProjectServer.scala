package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.time.Instant
import monocle.macros.Lenses
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import scalaz.{-\/, Monad, \/, \/-, ~>}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.base.user._
import ProjectServer._

trait ProjectServer[F[_]] {
  def register(pid: ProjectId, userId: UserId, onChange: OnChange[F]): F[RegistrationError \/ RegId]
  def unregister(r: RegId): F[Unit]
  def initialClient(r: RegId, username: Username): F[NotRegistered \/ ProjectSpaProtocols.InitData]
}

object ProjectServer {

  type RegId = Store.Register.RegId[ProjectId]

  type OnChange[F[_]] = VerifiedEvent.NonEmptySeq => F[Unit]

  type StoreAlgebra[F[_]]          = Store.Register.Algebra[F, ProjectId, State, OnChange[F]]
  type StoreNode   [F[_]]          = Store.Register.Node[State, OnChange[F]]
  type StoreMap    [F[_], M[_, _]] = M[ProjectId, StoreNode[F]]

  @Lenses
  final case class State(header: ProjectHeader, body: Promise[LoadError, LoadedState]) {
    def name: Project.Name =
      body.toOption.fold(header.name)(_.project.name)

    def set(s: LoadedState): State =
      copy(body = Promise.Available(s))
  }

  final case class LoadedState(project: Project, projectMetaData: ProjectMetaData, nextOrd: EventOrd) {
    def update(project: Project, ve: VerifiedEvent, when: Instant): LoadedState = {
      val md = projectMetaData.applyEvent(ve, when)
      LoadedState(project, md, ve.ord + 1)
    }
  }

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
  // Errors

  sealed trait RegistrationError
  case object ProjectNotFound extends RegistrationError
  case object AccessDenied extends RegistrationError
  implicit def univEqRegistrationError: UnivEq[RegistrationError] = UnivEq.derive

  sealed trait LoadError {
    def errorMsg: ErrorMsg
  }
  case object LoadNotStarted extends LoadError {
    def errorMsg = Server.ErrorMsgs.ShouldNeverHappen
  }
  final case class BuildError(error: String, events: VerifiedEvent.Seq) extends LoadError {
    def errorMsg =
      ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}\n\nEvent application failure.\n$error")
//    def eventRange: String =
//      NonEmptySet.maybe(events.map(_.ord.value), "∅")(ConciseIntSetFormat.spaced)
  }

  sealed trait AddEventError {
    def errorMsg: ErrorMsg
  }
  final case class PostLoadError(error: Promise.GetOrSet.Failure[LoadError]) extends AddEventError {
    override def errorMsg = errorMsgForPromise(error)(_.errorMsg)
  }
  final case class SaveError(opsInfo: String, error: Throwable) extends AddEventError  {
    override val errorMsg = ErrorMsg("Something went wrong on our end trying to update the project.")
  }
  final case class EventRejected(reason: String) extends AddEventError  {
    override def errorMsg = ErrorMsg(reason)
  }
  case object NotRegistered extends AddEventError {
    override val errorMsg = ErrorMsg("Session expired.")
  }
  type NotRegistered = NotRegistered.type

  def errorMsgForPromise[E](a: Promise.GetOrSet.Failure[E])(f: E => ErrorMsg): ErrorMsg =
    a match {
      case Promise.GetOrSet.CustomFailure(e) => f(e)
      case Promise.GetOrSet.NoPromise        => NotRegistered.errorMsg
      case Promise.GetOrSet.Timeout          => Server.ErrorMsgs.Timeout
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Internal

  // 21.138s = 0.015s + 0.027s + 0.0486s + 0.0864s + 0.1548s + 0.2772s + 0.4986s + 0.8964s + 1.6128s + 2.9016s + 5.2218s + 9.3978s
  val LoadRetries: Retries =
    Retries.exponentiallyFrom(15.millis, factor = 1.8)(_.toMillis <= 10.seconds.toMillis)

  // 7.665s = 0.015s + 0.03s + 0.06s + 0.12s + 0.24s + 0.48s + 0.96s + 1.92s + 3.84s
  val SaveRetries: Retries =
    Retries.exponentiallyFrom(15.millis)(_.toMillis <= 4.seconds.toMillis)

  def buildProject(load: VerifiedEvent.Seq): BuildError \/ (Project, EventOrd) =
    if (load.isEmpty) {
      val ord = EventOrd(1) // Nice to reserve 0 for ApplyTemplate.
      \/-((Project.empty, ord))
    } else
      ApplyEvent.trusted.applyVerified(load)(Project.empty) match {
        case \/-(p) => \/-((p, load.lastKey.ord + 1))
        case -\/(e) => -\/(BuildError(e, load))
      }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  def apply[D[_], F[_]](broadcastTo: BroadcastTo)
                       (implicit db: DB.ForProjectSpa[D],
                        store: StoreAlgebra[F],
                        svr: Server.Algebra[F],
                        trace: Trace.Basic[F],
                        runDB: D ~> F,
                        F: Monad[F],
                        D: Monad[D]): ProjectServer[F] =
    new ProjectServer[F] {

      type Node = StoreNode[F]

      val fUnit: F[Unit] = F.pure(())
      val _fUnit: Any => F[Unit] = _ => fUnit

      val nodeToState: monocle.Optional[Option[Node], State] =
        monocle.std.option.some[Node] ^|-> Store.Register.Node.value

      val promiseOptics = Promise.Optics(nodeToState, State.body)

      val register = new Store.Register.Dsl[F, ProjectId, State, OnChange[F]]

      override def register(pid: ProjectId, userId: UserId, onChange: OnChange[F]): F[RegistrationError \/ RegId] = {
        def loadState: D[RegistrationError \/ State] =
          db.getProjectHeader(pid).map {
            case Some(h) =>
              if (userId ==* h.userId)
                \/-(State(h, Promise.Failure(LoadNotStarted)))
              else
                -\/(AccessDenied)
            case None =>
              -\/(ProjectNotFound)
          }

        def init: F[RegistrationError \/ State] =
          runDB(loadState)

        def authorise: State => Option[RegistrationError] =
          s => Option.when(s.header.userId !=* userId)(AccessDenied)

        for {
          x <- register.registerAttempt(pid, onChange, init, authorise)
          _ <- x.fold(_fUnit, loadStateInBackground)
        } yield x
      }

      override def unregister(r: RegId): F[Unit] =
        register.unregister(r)

      private def readState(r: RegId): F[NotRegistered \/ State] =
        register.get(r.key).map(_ \/> NotRegistered)

      private def getOrSetLoadedState(regId: RegId): F[Promise.GetOrSet[LoadError, (State, LoadedState)]] = {
        val pid = regId.key

        def init: F[LoadError \/ LoadedState] =
          runDB(db.inDbTransaction(for {
            pl <- db.getAllProjectEvents(pid)
            md <- db.getProjectMetaData(pid) // only really need createdAt and lastUpdatedAt
          } yield buildProject(pl).map(b => LoadedState(b._1, md.get, b._2))))

        Promise.getOrSet(store, promiseOptics)(regId.key, LoadRetries, _ => Some(init))
      }

      private def loadStateInBackground(regId: RegId): F[Unit] =
        svr.fork(getOrSetLoadedState(regId))

      override def initialClient(r: RegId, username: Username): F[NotRegistered \/ ProjectSpaProtocols.InitData] =
        readState(r).flatMap {
          case \/-(s) => initDataForProjectSpa(r, username, s).map(\/-(_))
          case e@ -\/(_) => F pure e
        }

      private def initAsyncData(r: RegId): F[ProjectSpaProtocols.InitAsync.Output] =
        getOrSetLoadedState(r) map {

          case Promise.GetOrSet.Success((_, s)) =>
            \/-(ProjectSpaProtocols.InitAsyncData(s.project, s.projectMetaData, s.nextOrd - 1))

          case e: Promise.GetOrSet.Failure[LoadError] =>
            -\/(errorMsgForPromise(e)(_.errorMsg))
        }

      private def initDataForProjectSpa(r: RegId, username: Username, s: State): F[ProjectSpaProtocols.InitData] = {
        import shipreq.webapp.base.protocol._
        import ProjectSpaProtocols._

        def updProj(mkEvent: Project => MakeEvent.Result): F[ErrorMsg \/ VerifiedEvent.Seq] =
          trace.sub("UpdateProject")(
            addEvent(r, mkEvent, SaveRetries).map {
              case PotentialChange.Success(es) => \/-(es)
              case PotentialChange.Unchanged   => \/-(VerifiedEvent.Seq.empty)
              case PotentialChange.Failure(e)  => -\/(e.errorMsg)
            }
          )

        import svr.{createServerSideProc => f}
        for {
          projectInit           ← f(InitAsync            )(_ => initAsyncData(r))
          customReqTypeCrud     ← f(CustomReqTypeCrud    )(i => updProj(p ⇒ MakeEvent.customReqTypeCrud(i, p)))
          reqTypeImplicationMod ← f(ReqTypeImplicationMod)(i => updProj(_ ⇒ MakeEvent.reqTypeImplicationMod(i)))
          customIssueTypeCrud   ← f(CustomIssueTypeCrud  )(i => updProj(p ⇒ MakeEvent.customIssueTypeCrud(i, p)))
          tagCrud               ← f(TagCrud.Protocol     )(i => updProj(p ⇒ MakeEvent.tagCrud(i, p)))
          fieldCrud             ← f(FieldCrud.Protocol   )(i => updProj(p ⇒ MakeEvent.fieldCrud(i, p)))
          fieldMandatorinessMod ← f(FieldMandatorinessMod)(i => updProj(_ ⇒ MakeEvent.fieldMandatorinessMod(i)))
          createContent         ← f(CreateContent        )(i => updProj(p ⇒ MakeEvent.createContent(i, p)))
          updateContent         ← f(UpdateContent        )(i => updProj(p ⇒ MakeEvent.updateContent(i, p)))
          updateSavedViews      ← f(UpdateSavedViews     )(i => updProj(p ⇒ MakeEvent.updateSavedViews(i, p)))
          projectNameSet        ← f(ProjectNameSet       )(i => updProj(_ ⇒ MakeEvent.projectNameSetFn(i)))
        } yield InitData(
          username,
          s.name,
          projectInit,
          customIssueTypeCrud,
          customReqTypeCrud,
          reqTypeImplicationMod,
          fieldMandatorinessMod,
          fieldCrud,
          tagCrud,
          createContent,
          updateContent,
          updateSavedViews,
          projectNameSet)
      }

      private def addEvent(r: RegId, mkEvent: Project => MakeEvent.Result, retries: Retries): F[PotentialChange[AddEventError, VerifiedEvent.NonEmptySeq]] =
        // Non-atomicity guarded by DB constraint on eventOrd
        getOrSetLoadedState(r).flatMap {
          case Promise.GetOrSet.Success((_, s1)) =>
            trace.sub("MakeEvent")(F pure ApplyNewEvent(mkEvent(s1.project), s1.project)) flatMap {
              case PotentialChange.Success(updated) =>
                val ord = s1.nextOrd
                runDB(db.saveProjectEvent(r.key)(ord, updated.event, updated.hashRecs)).flatMap {

                  case None =>
                    val ve = VerifiedEvent(ord, updated.event, updated.hashRecs)
                    val ves = VerifiedEvent.NonEmptySeq.one(ve)
                    for {
                      now <- svr.now
                      s2a = s1.update(updated.project, ve, now)
                      s2  <- store.storeModIfPresent(r.key)(_.modValue(_ set s2a))
                      _   <- s2.fold(fUnit)(broadcastEvents(r, ves, _))
                    } yield PotentialChange.Success(ves)

                  case Some(error) =>
                    retries.pop match {
                      case Some((delay, nextRetries)) =>
                        val retry = addEvent(r, mkEvent, nextRetries)
                        svr.delay(retry, delay)
                      case None =>
                        val opsInfo = s"Error saving new event ${updated.event} to project ${r.key.value} with ordinal #${ord.value}."
                        F pure PotentialChange.Failure(SaveError(opsInfo, error))
                    }
                }

              case PotentialChange.Unchanged =>
                F pure PotentialChange.Unchanged

              case PotentialChange.Failure(reason) =>
                F pure PotentialChange.Failure(EventRejected(reason))
            }

          case e: Promise.GetOrSet.Failure[LoadError] =>
            F pure PotentialChange.Failure(PostLoadError(e))
        }

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
