package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.univeq.UnivEq

object ConsolidatedSeq {

  object Logic {
    def apply[A](doConsolidate: Candidate[A] => Boolean): Dsl[A] =
      new Dsl(doConsolidate)

    final class Dsl[A](doConsolidate: Candidate[A] => Boolean) {
      def apply[B](groupHead    : Group[A] => B,
                   groupTail    : Group[A] => Option[B] = (_: Any) => None): Logic[A, B] =
        new Logic(doConsolidate, groupHead, groupTail)
    }
  }

  final class Logic[-A, +B](doConsolidate: Candidate[A] => Boolean,
                            groupHead    : Group[A] => B,
                            groupTail    : Group[A] => Option[B]) {

    def contramap[C](f: C => A): Logic[C, B] =
      new Logic[C, B](
        c => doConsolidate(c.map(f)),
        g => groupHead(g.map(f)),
        g => groupTail(g.map(f)))

    def map[C](f: B => C): Logic[A, C] =
      new Logic[A, C](
        doConsolidate,
        g => f(groupHead(g)),
        g => groupTail(g).map(f))

    def consolidate(as: TraversableOnce[A]): ConsolidatedSeq[B] = {
      val it            = as.toIterator
      var srcIndexStart = 0
      var group         = 0
      var buffer        = Vector.empty[A]
      val values        = Array.newBuilder[Option[B]]
      val groups        = Array.newBuilder[Int]

      def commit(): Unit = {
        val size = buffer.length
        val g = Group(NonEmptyVector force buffer, srcIndexStart, group)
        values += Some(groupHead(g))
        groups += group
        if (size > 1) {
          val o = groupTail(g)
          for (_ <- 1 until size) {
            values += o
            groups += group
          }
        }

        group += 1
        srcIndexStart += size
        buffer = Vector.empty
      }

      while (it.hasNext) {
        val cur = it.next()
        if (buffer.isEmpty)
          buffer :+= cur
        else {
          val c = Candidate(buffer, cur, srcIndexStart, group)
          if (!doConsolidate(c))
            commit()
          buffer :+= cur
        }
      }

      if (buffer.nonEmpty)
        commit()

      new ConsolidatedSeq(values.result(), groups.result())
    }
  }

  final case class Candidate[+A](allPrev: Vector[A], cur: A, srcIndexStart: Int, group: Int) {
    val prev        = allPrev.last
    def first       = if (allPrev.nonEmpty) allPrev.head else prev
    def srcIndexEnd = srcIndexStart + allPrev.length
    def map[B](f: A => B) = copy(allPrev.map(f), f(cur))
  }

  final case class Group[+A](values: NonEmptyVector[A], srcIndexStart: Int, group: Int) {
    def value       = values.head
    def length      = values.length
    def srcIndexEnd = srcIndexStart + values.length - 1
    def map[B](f: A => B) = copy(values.map(f))
  }

  implicit def univEqC[A: UnivEq]: UnivEq[Candidate      [A]] = UnivEq.derive
  implicit def univEqG[A: UnivEq]: UnivEq[Group          [A]] = UnivEq.derive
  implicit def univEq [A: UnivEq]: UnivEq[ConsolidatedSeq[A]] = UnivEq.force
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class ConsolidatedSeq[+A](values: Array[Option[A]], groups: Array[Int]) {
  def indices           = values.indices
  def length            = values.length
  def isEmpty           = values.isEmpty
  @inline def nonEmpty  = !isEmpty
  def elementIterator() = values.iterator
  def groupIterator()   = groups.iterator
  def groupCount        = if (groups.isEmpty) 0 else groups.last + 1

  def apply(i: Int): Option[A] =
    if (i >= 0 && i < length)
      values(i)
    else
      None

  def group(i: Int): Int =
    if (i >= 0 && i < length)
      groups(i)
    else
      -1

  def map[B](f: A => B): ConsolidatedSeq[B] =
    new ConsolidatedSeq(values.map(_.map(f)), groups)

  override def hashCode =
    values.## * 31 + groups.##

  override def equals(obj: Any): Boolean = obj match {
    case x: ConsolidatedSeq[_] =>
      (length == x.length) && indices.forall(i => values(i) == x(i) && groups(i) == x.group(i))
    case _ =>
      false
  }

  override def toString: String =
    values.iterator.map(_.fold("")("" + _)).mkString("ConsolidatedSeq(", ",", ")")
}
