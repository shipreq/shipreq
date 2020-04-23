package shipreq.webapp.base.data

import shipreq.base.util.storecache._
import ProjectDerivations.Logic

final case class ProjectDerivations()

object ProjectDerivations {

  def next(prev: Option[ProjectDerivations], project: Project): ProjectDerivations = {
    apply()
  }

  // ===================================================================================================================

  object QuickEqInstances {
  }

  // ===================================================================================================================

  object Logic {
    import QuickEqInstances._

  }
}
