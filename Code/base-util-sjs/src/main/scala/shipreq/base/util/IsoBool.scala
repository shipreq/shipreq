package shipreq.base.util

import scalaz.Isomorphism.<=>

/**
 * Boolean isomorphism.
 */
trait IsoBool[B] extends (Boolean <=> B) {
  this: B =>

  protected def neg: B

  final val ~> : B => Boolean = _ == this
  final val <~ : Boolean => B = if (_) this else neg

  final override def from = ~>
  final override def to   = <~
}

object IsoBool {

  trait ObjOnly[B] {
    implicit final def equality = UnivEq.force[B]
    protected def pos: B with IsoBool[B]
    protected def neg: B

    final def memo[A](f: B => A): B => A = {
      val p = f(pos)
      val n = f(neg)
      b => if (pos ~> b) p else n
    }
  }

  /**
   * Boolean isomorphism: case + object.
   */
  trait Obj[B] extends IsoBool[B] with ObjOnly[B] {
    this: B =>
    final override protected def pos = this
  }
}