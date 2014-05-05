package shipreq.taskman.server

import scalaz.~>
import shipreq.base.util.effect.IOE

package object business {

  type BopReifier = Bop ~> IOE

}
