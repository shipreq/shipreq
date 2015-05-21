package shipreq.webapp.client.util

import shipreq.base.util.IsoBool

/** Is a subject on or off? */
sealed trait On

case object On extends On with IsoBool.Obj[On] {
  override protected def neg = Off
}

case object Off extends On with IsoBool[On] {
  override protected def neg = On
}