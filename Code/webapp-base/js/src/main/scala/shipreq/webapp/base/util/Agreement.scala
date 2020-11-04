package shipreq.webapp.base.util

import shipreq.base.util.IsoBool

sealed trait Agreement extends IsoBool[Agreement] with IsoBool.WithBoolOps[Agreement] {
  override final def companion = Agreement
}

object Agreement extends IsoBool.Object[Agreement] {
  override def positive = Agree
  override def negative = Disagree
}

case object Agree extends Agreement
case object Disagree extends Agreement
