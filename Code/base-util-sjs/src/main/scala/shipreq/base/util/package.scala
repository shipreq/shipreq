package shipreq.base

import scalaz.{OneAnd, \/}

package object util {

  type ErrorOr[A] = Error \/ A

  /** Faster than Vector(a) */
  @inline def Vector1[A](a: A): Vector[A] = Vector.empty :+ a
}
