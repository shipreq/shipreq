package shipreq.webapp.client.base.lib

import japgolly.scalajs.react.extra.Reusability

/**
  * Just a named pair.
  *
  * `_.commit` is much clearer than `_._2` which matters when the abort/commit types are the same.
  */
case class AbortCommit[A, C](abort: A, commit: C)

object AbortCommit {
  implicit def reuse[A: Reusability, C: Reusability]: Reusability[AbortCommit[A, C]] =
    Reusability.caseClass
}