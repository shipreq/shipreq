package com.beardedlogic.shipreq.feature.uc.change

import com.beardedlogic.shipreq.feature.uc.UseCase
import com.beardedlogic.shipreq.feature.validation.VFailure
import com.beardedlogic.shipreq.lib.Types.UcUpdateResult

/**
 * A filter on use case changes just before they are applied.
 */
trait ChangeConstraint {

  final def apply(r: UcUpdateResult): UcUpdateResult =
    r match {
      case c@Changed(_, _) => check(c).getOrElse(c)
      case otherwise       => otherwise
    }

  def check(c: Changed[UseCase, Change]): Option[ChangeFailure]
}

/**
 * Prevents the user from created more than a given number of steps.
 */
case class LimitTotalNumberOfSteps(limit: Int) extends ChangeConstraint {
  require(limit > 2)

  val error = Some(ChangeFailure(VFailure.looseMsg(s"This use case has a limit of $limit steps.")))

  override def check(c: Changed[UseCase, Change]) =
    if (c.value.totalStepCount <= limit)
      None
    else
      error
}