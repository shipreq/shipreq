package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.time.{Duration, Instant}
import monocle.macros.Lenses
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import scalaz.{-\/, Monad, \/, \/-, ~>}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.base.util.log.HasLogger
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.base.user._

trait ProjectServer[F[_]] {
  import ProjectServer._
  def register(pid: ProjectId, userId: UserId, onChange: OnChange[F]): F[RegistrationError \/ RegId]
  def unregister(r: RegId): F[Unit]
  def initialClient(r: RegId, username: Username): F[NotRegistered \/ ProjectSpaProtocols.InitData]
}

object ProjectServer extends HasLogger {

  type RegId = Store.Register.RegId[ProjectId]

  type OnChange[F[_]] = VerifiedEvent.NonEmptySeq => F[Unit]

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  def apply[D[_], F[_]](broadcastTo: BroadcastTo): ProjectServer[F] =
    new ProjectServer[F] {
      override def register(pid: ProjectId, userId: UserId, onChange: OnChange[F]) = ???
      override def unregister(r: RegId): F[Unit] = ???
      override def initialClient(r: RegId, username: Username) = ???
    }
}
