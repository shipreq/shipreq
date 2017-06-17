package shipreq.base.util

import japgolly.univeq.UnivEq

/** Implements .equals using reference equality */
trait EqualsByRef { self: AnyRef => // TODO Move into microlibs

  final override def equals(obj: Any): Boolean =
    obj match {
      case o: AnyRef => this eq o
      case _ => false
    }
}

object EqualsByRef {
  implicit def univEq[A <: EqualsByRef]: UnivEq[A] =
    UnivEq.force
}