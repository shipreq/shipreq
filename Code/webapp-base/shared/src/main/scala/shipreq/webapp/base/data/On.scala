package shipreq.webapp.base.data

import shipreq.base.util.IsoBool

/** Is a subject on or off? */
sealed trait On extends IsoBool.WithBoolOps[On] {
  override final def companion = On
}

case object On extends On with IsoBool.Object[On] {
  override def positive = On
  override def negative = Off
}

case object Off extends On