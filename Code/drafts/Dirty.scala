package shipreq.base.test.drafts

import shipreq.base.util.IsoBool

sealed trait Dirty extends IsoBool.WithBoolOps[Dirty] {
  override final def companion = Dirty
}

case object Dirty extends Dirty with IsoBool.Object[Dirty] {
  override def positive = Clean
  override def negative = Dirty
}

case object Clean extends Dirty