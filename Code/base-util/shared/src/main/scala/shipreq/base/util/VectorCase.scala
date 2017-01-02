package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptyVector

/**
 * Pattern matching on Vectors.
 */
object VectorCase {

  object Empty {
    def unapply[A](v: Vector[A]) = v.isEmpty
  }

  object Sole {
    def unapply[A](v: Vector[A]) = new Unapply(v)
    final class Unapply[A](val v: Vector[A]) extends AnyVal {
      def isEmpty = v.length != 1
      def get     = v.head
    }
  }

  object NonEmpty {
    def unapply[A](v: Vector[A]) = new Unapply(v)
    final class Unapply[A](val v: Vector[A]) extends AnyVal {
      def isEmpty = v.isEmpty
      def get     = NonEmptyVector(v.head, v.tail)
    }
  }
}
