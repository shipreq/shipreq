package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util._

sealed abstract class ProjectPerm(final val ord: Int) {
  import ProjectPerm._

  /** `this` is the required permission, `subject` is the actual permission of the user. */
  final def isSatisfiedBy(subject: ProjectPerm): Permission =
    this match {
      case Admin => subject match {
        case Admin => Allow
        case Collaborator => Deny
      }
      case Collaborator => subject match {
        case Admin | Collaborator => Allow
      }
    }

  /** `this` is the required permission, `subject` is the actual permission of the user. */
  final def isSatisfiedBy(subject: Option[ProjectPerm]): Permission =
    subject match {
      case Some(s) => isSatisfiedBy(s)
      case None    => Deny
    }
}

object ProjectPerm {
  case object Admin        extends ProjectPerm(0)
  case object Collaborator extends ProjectPerm(1)

  val values = AdtMacros.adtValues[ProjectPerm]

  implicit def univEq: UnivEq[ProjectPerm] = UnivEq.derive
}
