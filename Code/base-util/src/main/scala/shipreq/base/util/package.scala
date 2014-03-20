package shipreq.base

import scalaz.\/

package object util {

  type ErrorOr[A] = Error \/ A

}
