package shipreq.webapp.member.project.protocol.websocket

import cats.Monad
import cats.syntax.functor._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.base.data.{Rolodex, UserId, Username}
import shipreq.webapp.member.project.event.{Event, VerifiedEvent}
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.Supplimentary

/** Logic to obtain necessary supplimentary data required when certain events are received. */
object SupplimentaryLogic {

  type Fn[F[_]] = VerifiedEvent.Seq => F[ProjectSpaProtocols.Supplimentary]

  def apply[F[_]](needUsernamesByUserId: Set[UserId] => F[Map[UserId, Username]],
                  obfuscate            : UserId => UserId.Public,
                  deobfuscate          : UserId.Public => UserId)
                 (implicit F: Monad[F]): Fn[F] = {
    val empty = F.pure(Supplimentary.empty)

    events =>
      if (events.isEmpty)
        empty
      else {
        var newPublicUserIds = Set.empty[UserId.Public]

        for (ve <- events)
          ve.event match {

            case e: Event.AccessUpdate =>
              e.updates.foreach {
                case (uid, Some(_)) => newPublicUserIds += uid
                case _              =>
              }

            case _ =>
          }

        for {
          usernames <- needUsernamesByUserId(newPublicUserIds.map(deobfuscate))
        } yield {
          val rolodex = Rolodex(usernames.mapKeysNow(obfuscate))
          Supplimentary(rolodex)
        }
      }
  }

}
