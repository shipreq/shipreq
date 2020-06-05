package shipreq.base.util.algorithm

import scala.annotation.tailrec

/** Iterative Deepening A*
  *
  * IDA* is guaranteed to find the shortest path leading from the given start node to any goal node in the problem graph,
  * if the heuristic function h is admissible, that is h(n) ≤ c(n) for all nodes n, where c is the true cost of the
  * shortest path from n to the nearest goal.
  *
  * @param estCost Estimated cost of the cheapest path. Must never exceed the real cost.
  * @param cost Real cost between two nodes
  */
final class IterativeDeepeningA[Node](estCost : Node => Double,
                                      cost    : (Node, Double, Node) => Double,
                                      isGoal  : (Node, Double) => Boolean,
                                      children: Node => Array[Node]) {

  private type Path = IterativeDeepeningA.Path[Node]

  /**
   * @return Nil if solution not found, else a path with most recent at the head and the root last.
   */
  def findPathFrom(root: Node): List[Node] = {
    val path = new IterativeDeepeningA.Path(root)
    @tailrec
    def loop(threshold: Double): List[Node] = {
      val t = search(path, 0, threshold)
      if (t.isNaN)
        path.value
      else if (t == Double.PositiveInfinity)
        Nil
      else
        loop(t)
    }
    loop(estCost(root))
  }

  def findGoalFrom(root: Node): Option[Node] =
    findPathFrom(root).headOption

  /**
    * @param path is mutated directly
    * @param g cost to reach current node
    * @return NaN for FOUND, ∞ for NOT_FOUND, otherwise next bound
    */
  private def search(path: Path, g: Double, threshold: Double): Double = {
    val node = path.last()
    val h = estCost(node)
    val f = g + h

    // println(s"search: path=${path.value.mkString("-")}, g=$g, threshold=$threshold, node=$node, h=$h, f=$f")

    if (f > threshold)
      f
    else if (isGoal(node, h))
      Double.NaN
    else {
      var min = Double.PositiveInfinity
      val cs = children(node)
      var i = cs.length
      while (i > 0) {
        i -= 1
        val c = cs(i)

        if (!path.contains(c)) {
          path.push(c)
          val t = search(path, g + cost(node, h, c), threshold)
          if (t.isNaN)
            return t
          if (t < min)
            min = t
          path.pop()
        }
      }
      min
    }
  }
}

object IterativeDeepeningA {

  def apply[Node](estCost : Node => Double,
                  cost    : (Node, Double, Node) => Double,
                  isGoal  : (Node, Double) => Boolean,
                  children: Node => Array[Node]): IterativeDeepeningA[Node] =
    new IterativeDeepeningA[Node](
      estCost  = estCost,
      cost     = cost,
      isGoal   = isGoal,
      children = children,
    )

  private final class Path[A](root: A) {
    var value                 : List[A] = root :: Nil
    @inline def last()        : A       = value.head
    @inline def contains(a: A): Boolean = value.contains(a)
    @inline def push(a: A)    : Unit    = value ::= a
    @inline def pop()         : Unit    = value = value.tail
  }
}
