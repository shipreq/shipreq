package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util._

sealed abstract class ProjectRole(final val ord: Int) {
  import ProjectRole._

  /** `this` is the required role, `subject` is the actual role of the user. */
  final def isSatisfiedBy(subject: ProjectRole): Permission =
    this match {
      case Admin => subject match {
        case Admin                 => Allow
        case Collaborator | Viewer => Deny
      }
      case Collaborator => subject match {
        case Admin | Collaborator => Allow
        case Viewer               => Deny
      }
      case Viewer => subject match {
        case Admin | Collaborator | Viewer => Allow
      }
    }

  /** `this` is the required role, `subject` is the actual role of the user. */
  final def isSatisfiedBy(subject: Option[ProjectRole]): Permission =
    subject match {
      case Some(s) => isSatisfiedBy(s)
      case None    => Deny
    }
}

object ProjectRole {
  case object Admin        extends ProjectRole(0)
  case object Collaborator extends ProjectRole(1)
  case object Viewer       extends ProjectRole(2)

  // The order specified here defines the order rendered in UI dropdowns
  val values = AdtMacros.adtValuesManually[ProjectRole](
    Admin,
    Collaborator,
    Viewer,
  )

  implicit def univEq: UnivEq[ProjectRole] = UnivEq.derive

  /** [[ProjectRole]] with the least rights */
  def min: ProjectRole = Viewer
}
