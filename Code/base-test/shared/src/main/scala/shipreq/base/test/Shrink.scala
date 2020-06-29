package shipreq.base.test

import scala.annotation.tailrec
import shipreq.base.util._

object Shrink {

  val DefaultBreadthLimit = 3

  /**
   * @param validity Continues to shrink while result is Invalid
   */
  def apply[A](initialValue: A)
              (shrinker    : Shrinker[A],
               size        : A => Int,
               validity    : A => Validity,
               breadthLimit: Int = DefaultBreadthLimit): A = {

    @tailrec
    def go(root: RoseTree[A]): A = {
      val rootSize = size(root.value)
      val candidates =
        root.children.iterator.flatMap { child =>
          val childSize = size(child.value)
          if (childSize < rootSize && validity(child.value).is(Invalid))
            (child, childSize) :: Nil
          else
            Nil
        }.take(breadthLimit)

      val child = chooseSmallest(root, rootSize, candidates)
      if (child ne root) go(child) else root.value
    }

    go(shrinker.start(initialValue))
  }

  private[test] def chooseSmallest[A](root: A, rootSize: Int, candidates: IterableOnce[(A, Int)]): A = {
    var best = (root, rootSize)
    for (c <- candidates.iterator) {
      if (c._2 < best._2)
        best = c
    }
    best._1
  }
}
