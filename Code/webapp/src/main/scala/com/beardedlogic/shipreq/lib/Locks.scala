package shipreq.webapp
package lib

import util.{DefaultLockProvider, LockToken, LockProvider}
import LockProvider._
import Types._

object Locks {

  sealed trait SingleUseCase extends LockToken

  sealed trait UseCaseNumbers extends SingleUseCase

  /**
   * Locks a project so that UC numbers are stable.
   *
   * Write-access blocks SingleUseCase write-access.
   */
  val UseCaseNumbers = DefaultLockProvider.simple[ProjectId, UseCaseNumbers]

  /**
   * Locks a single UC.
   *
   * Any access here blocks UseCaseNumbers write-access.
   */
  val SingleUseCase =
    new LockProvider[(UseCaseIdentId, ProjectId), SingleUseCase, Ø, UseCaseNumbers, SingleUseCase] {

      private val UCL = DefaultLockProvider.simple[UseCaseIdentId, SingleUseCase]

      override def read[U](ids: (UseCaseIdentId, ProjectId))(block: RL => U): U =
        UseCaseNumbers.shareAccess(ids._2)(l1 =>
          UCL.read(ids._1)(l2 => {
            val l = merge(l1, l2)
            block(l)
          }))

      override def write[U](ids: (UseCaseIdentId, ProjectId))(block: WL => U): U =
        UseCaseNumbers.shareAccess(ids._2)(l1 =>
          UCL.write(ids._1)(l2 => {
            val l = merge(l1, l2)
            block(l)
          }))
    }
}
