package com.beardedlogic.usecase
package lib

import app.RequestVars.SoleProject
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
  val SingleUseCase: LockProvider[UseCaseIdentId, SingleUseCase, Ø, UseCaseNumbers, SingleUseCase] =
    new DefaultLockProvider[UseCaseIdentId, SingleUseCase, Ø, UseCaseNumbers, SingleUseCase] {

      private val UCL = DefaultLockProvider.simple[UseCaseIdentId, SingleUseCase]

      def pid: ProjectId = SoleProject.is.id

      override def read[U](id: UseCaseIdentId)(block: RL => U): U =
        UseCaseNumbers.shareAccess(pid)(l1 =>
          UCL.read(id)(l2 => {
            val l = merge(l1, l2)
            block(l)
          }))

      override def write[U](id: UseCaseIdentId)(block: WL => U): U =
        UseCaseNumbers.shareAccess(pid)(l1 =>
          UCL.write(id)(l2 => {
            val l = merge(l1, l2)
            block(l)
          }))
    }
}
