package shipreq.base

import scalaz.{\&/, \/}

package object util {

  type Error = String \&/ Throwable
  type ErrorOr[A] = Error \/ A

}
