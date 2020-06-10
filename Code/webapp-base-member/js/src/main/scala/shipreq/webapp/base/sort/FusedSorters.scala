package shipreq.webapp.base.sort

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import scala.scalajs.js.{Array => JArray}
import shipreq.webapp.base.sort.Sorter._

final class FusedSorters[Setup, Row](sorters: NonEmptyVector[Sorter[Setup, Row]]) extends Sorter[Setup, Row] {
  private[this] val ss       = sorters.whole
  private[this] val tSize    = ss.size + 1
  private[this] val rowIndex = tSize - 1

  // Unsafe and mutable.
  // [d₁, …, dₙ, row] where n = ss.size
  override type T = JArray[Any]

  override val prepFn: PrepFn[Setup, Row, T] =
    p => {
      val eachPrepFn: JArray[Row => Any] =
        JArray(ss.map(_ prepFn p): _*)

      row => {
        val t: T = new JArray[Any](tSize)
        var i = 0
        eachPrepFn.foreach { f =>
          t(i) = f(row)
          i = i + 1
        }
        t(i) = row
        t
      }
    }

  override val rowModFn: RowModFn[Setup, Row] =
    consolidateRowModFns(ss.iterator.map(_.rowModFn))

  private def eachSortFn: Vector[(T, T) => Int] =
    ss.zipWithIndex.map {
      case (s, i) =>
        val f = s.sortFn.f
        (as: T, bs: T) => {
          val a = as(i).asInstanceOf[s.T]
          val b = bs(i).asInstanceOf[s.T]
          f(a, b)
        }
    }

  override val sortFn: SortFn[T] =
    SortFn(eachSortFn.reduce { (s, t) =>
      val f = s
      val g = t
      (as: T, bs: T) => {
        val r = f(as, bs)
        if (r == 0) g(as, bs) else r
      }
    })

  def row(t: T): Row =
    t(rowIndex).asInstanceOf[Row]

  type Result = IterableOnce[Row] => MutableArray[Row]

  def result(setup: Setup): Result =
    _result(setup, rowModFn :: Nil)

  def result(setup: Setup, extraRowModFn: RowModFn[Setup, Row]): Result =
    _result(setup, extraRowModFn :: rowModFn :: Nil)

  private def _result(setup: Setup, rowModFns: IterableOnce[RowModFn[Setup, Row]]): Result = {
    val prepare = prepFn(setup)
    val rowMod  = Sorter.consolidateRowModFns(rowModFns)
    val rowEndo = rowMod.map(_(setup, KeepDir)).getOrElse((r: Row) => r)

    rows =>
      MutableArray(rows.iterator.map(r => prepare(rowEndo(r))))
        .sort(sortFn.toOrdering)
        .map(row)
  }
}

object FusedSorters {
  def apply[S, R](a: Sorter[S, R], b: Sorter[S, R]*): FusedSorters[S, R] =
    new FusedSorters(NonEmptyVector.varargs(a, b: _*))
}