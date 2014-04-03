package shipreq.taskman

import scalaz.~>
import scalaz.effect.IO

package object api {

  type ApiOpReifier = ApiOp ~> IO

}
