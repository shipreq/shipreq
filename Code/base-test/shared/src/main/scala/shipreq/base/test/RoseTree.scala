package shipreq.base.test

import scala.collection.View

final case class RoseTree[+A](value: A, children: View[RoseTree[A]]) {

  def ++[AA >: A](moreChildren: IterableOnce[RoseTree[AA]]): RoseTree[AA] =
    RoseTree(value, children ++ moreChildren)

  def map[B](f: A => B): RoseTree[B] =
    RoseTree(f(value), children.map(_.map(f)))
}

object RoseTree {

  def apply[A](a: A): RoseTree[A] =
    RoseTree(a, View.empty)

}
