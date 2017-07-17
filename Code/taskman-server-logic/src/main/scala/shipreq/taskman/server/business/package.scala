package shipreq.taskman.server

import scalaz.~>
import shipreq.base.util.effect.FxE

package object business {

  type BopReifier = Bop ~> FxE

}
