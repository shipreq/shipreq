package shipreq.webapp.feature

import shipreq.base.util.BiMap
import shipreq.webapp.lib.Types._
import scalaz.{Value, Name}

package object uc {

  type SavedSteps = BiMap[TextIdentId, LocalStepId]

  object SavedSteps {
    val empty: SavedSteps = BiMap.empty
  }

  type StepAndLabelBiMap = Name[BiMap[LocalStepId, StepLabel]]

  object StepAndLabelBiMap {
    val empty: StepAndLabelBiMap = Value(BiMap.empty)
  }

}
