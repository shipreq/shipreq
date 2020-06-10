package shipreq.webapp.base.sort

import japgolly.microlibs.stdlib_ext.MutableArray
import monocle.Optional
import scala.annotation.tailrec
import scalaz.std.option.optionInstance
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.ReqCode
import shipreq.webapp.base.text.PlainText

trait Sorter[Setup, Row] { self =>
  import Sorter._

  type T
  def prepFn  : PrepFn[Setup, Row, T]
  def sortFn  : SortFn[T]
  def rowModFn: RowModFn[Setup, Row]

  final def reverse: Sorter[Setup, Row] =
    new Sorter[Setup, Row] {
      override type T                              = self.T
      override def prepFn  : PrepFn[Setup, Row, T] = self.prepFn
      override val sortFn  : SortFn[T]             = self.sortFn.reverse
      override def rowModFn: RowModFn[Setup, Row]  = self.rowModFn.map(f => (s, dir) => f(s, dir.flip))
    }

  final def overrideWith(other: Sorter[Setup, Row])(useOther: Row => Boolean): Sorter[Setup, Row] =
    Sorter.merge(other, this)(useOther)
}

object Sorter {
  import SortMethod.{Asc, AscThenBlanks, BlanksThenAsc}

  /** Extracts, pre-processes and normalises data before sorting. */
  type PrepFn[-Setup, -Row, +A] = Setup => Row => A

  /** Sorts values in Expansion and MultiValues. */
  type RowModFn[Setup, Row] = Option[(Setup, Dir) => EndoFn[Row]]

  def apply[Setup, Row, A](prep: PrepFn[Setup, Row, A], sort: SortFn[A], rowMod: Sorter.RowModFn[Setup, Row] = None): Sorter[Setup, Row] =
    new Sorter[Setup, Row] {
      override type T       = A
      override val prepFn   = prep
      override val sortFn   = sort
      override val rowModFn = rowMod
    }

  private[Sorter] def merge[Setup, Row](s1: Sorter[Setup, Row], s2: Sorter[Setup, Row])(use1: Row => Boolean): Sorter[Setup, Row] =
    new Sorter[Setup, Row] {
      override type T = s1.T \/ s2.T

      override val prepFn: PrepFn[Setup, Row, T] =
        setup => {
          val p1 = s1.prepFn(setup)
          val p2 = s2.prepFn(setup)
          row => if (use1(row)) -\/(p1(row)) else \/-(p2(row))
        }

      override val sortFn: SortFn[T] =
        s1.sortFn ||| s2.sortFn

      override val rowModFn: RowModFn[Setup, Row] =
        (s1.rowModFn, s2.rowModFn) match {
          case (Some(f)   , Some(g)   ) => Some((x, y) => g(x, y) compose f(x, y))
          case (s@ Some(_), None      ) => s
          case (None      , s@ Some(_)) => s
          case (None      , None      ) => None
        }
    }

  class WithTypes[Setup, Row] {
    import shipreq.webapp.base.sort.{Sorter => S}

    final type Sorter        = S[Setup, Row]
    final type PrepFn[+T]    = S.PrepFn[Setup, Row, T]
    final type RowModFn      = S.RowModFn[Setup, Row]
    final type SorterForSMIB = S.SorterForSMIB[Setup, Row]
    final type SorterForSMCB = S.SorterForSMCB[Setup, Row]

    @inline final def sorter[A](prep: PrepFn[A], sort: SortFn[A], rowMod: RowModFn = None): Sorter = S(prep, sort, rowMod)
    @inline final def sorterForSMIB(s: Sorter) = S.SorterForSMIB(s)
    @inline final def sorterForSMCB(f: BlankPlacement => Sorter) = S.SorterForSMCB(f)
  }

  sealed trait BlankPlacement

  object BlankPlacement {

    val fromSortMethod: SortMethod.ConsiderBlanks => BlankPlacement = {
      case SortMethod.BlanksThenAsc
         | SortMethod.BlanksThenDesc => BlanksFirst
      case SortMethod.AscThenBlanks
         | SortMethod.DescThenBlanks => BlanksLast
    }

    @inline implicit def autoBlanksFirst(a: BlanksThenAsc.type): BlankPlacement = BlanksFirst
    @inline implicit def autoBlanksLast (a: AscThenBlanks.type): BlankPlacement = BlanksLast
  }

  case object BlanksFirst extends BlankPlacement
  case object BlanksLast  extends BlankPlacement

  sealed trait Dir {
    def flip: Dir
    def apply[A](a: A)(r: A => A): A
  }

  case object KeepDir extends Dir {
    override def flip = FlipDir
    override def apply[A](a: A)(r: A => A): A = a
  }

  case object FlipDir extends Dir {
    override def flip = KeepDir
    override def apply[A](a: A)(r: A => A): A = r(a)
  }

  // ===================================================================================================================
  // SortFn

  final case class SortFn[A](f: (A, A) => Int) {
    @inline def apply(x: A, y: A) = f(x, y)

    def &&&[B](next: SortFn[B]): SortFn[(A, B)] = {
      val g = next.f
      SortFn { (x, y) =>
        val a = f(x._1, y._1)
        if (a == 0)
          g(x._2, y._2)
        else
          a
      }
    }

    def |||[B](next: SortFn[B]): SortFn[A \/ B] = {
      val g = next.f
      SortFn { (x, y) =>
        (x, y) match {
          case (-\/(xa), -\/(ya)) => f(xa, ya)
          case (\/-(xb), \/-(yb)) => g(xb, yb)
          case (-\/(_) , \/-(_) ) => -1
          case (\/-(_) , -\/(_) ) =>  1
        }
      }
    }

    def option(bp: BlankPlacement): SortFn[Option[A]] =
      considerBlanksF[Option[A]](_.isEmpty)(_.get)(bp)

    def vector(bp: BlankPlacement): SortFn[Vector[A]] = {
      @tailrec def go(as: Vector[A], bs: Vector[A]): Int = {
        val ea = as.isEmpty
        val eb = bs.isEmpty
        if (ea) {
          if (eb) 0 else -1
        } else {
          if (eb) 1 else {
            val r = f(as.head, bs.head)
            if (r == 0) go(as.tail, bs.tail) else r
          }
        }
      }
      SortFn(go).considerBlanks(_.isEmpty)(bp)
    }

    def pair: SortFn[(A, A)] =
      this &&& this

    def byBlankPlacement[B](f: SortFn[A] => BlankPlacement => SortFn[B]): BlankPlacement => SortFn[B] = {
      val bf = f(this)(BlanksFirst)
      val bl = f(this)(BlanksLast)
      ;{
        case BlanksFirst => bf
        case BlanksLast  => bl
      }
    }

    def considerBlanks(isBlank: A => Boolean)(bp: BlankPlacement): SortFn[A] =
      considerBlanksF(isBlank)(identity)(bp)

    def considerBlanksF[B](isBlank: B => Boolean)(nonBlank: B => A)(bp: BlankPlacement): SortFn[B] = {
      val (headBlank, tailBlank) = bp match {
        case BlanksFirst => (-1, 1)
        case BlanksLast  => (1, -1)
      }
      SortFn { (x, y) =>
        val bx = isBlank(x)
        val by = isBlank(y)
        if (bx) {
          if (by) 0 else headBlank
        } else
          if (by) tailBlank else f(nonBlank(x), nonBlank(y))
      }
    }

    def contramap[B](g: B => A): SortFn[B] =
      SortFn((x, y) => f(g(x), g(y)))

    def applyDir(dir: Dir): SortFn[A] =
      dir(this)(_.reverse)

    def reverse: SortFn[A] =
      SortFn((x, y) => -f(x, y))

    def toOrdering: Ordering[A] =
      new Ordering[A] {
        def compare(x: A, y: A): Int = f(x, y)
      }
  }

  object SortFn {
    def fromOrdering[A](implicit o: Ordering[A]): SortFn[A] =
      SortFn(o.compare)

    val int: SortFn[Int] =
      SortFn(_ - _)

    val intPair: SortFn[(Int, Int)] =
      int.pair

    val intVector: BlankPlacement => SortFn[Vector[Int]] =
      int.byBlankPlacement(_.vector)

    val intPairVector: BlankPlacement => SortFn[Vector[(Int, Int)]] =
      intPair.byBlankPlacement(_.vector)

    val stringNonEmpty: SortFn[String] =
      SortFn(_ compareTo _)

    val string: BlankPlacement => SortFn[String] =
      stringNonEmpty.byBlankPlacement(_.considerBlanks(_.isEmpty))
  }

  // ===================================================================================================================
  // General

  type SorterForSMIB[Setup, Row] = SortMethod.IgnoreBlanks   => Sorter[Setup, Row]
  type SorterForSMCB[Setup, Row] = SortMethod.ConsiderBlanks => Sorter[Setup, Row]

  def SorterForSMIB[Setup, Row](s: Sorter[Setup, Row]): SorterForSMIB[Setup, Row] =
    SortMethod.resolverIB{ case Asc => s }(_.reverse)

  def SorterForSMCB[Setup, Row](f: BlankPlacement => Sorter[Setup, Row]): SorterForSMCB[Setup, Row] =
    SortMethod.resolverCB(f compose BlankPlacement.fromSortMethod)(_.reverse)

  private def tryModEndo[A, B](l: Optional[A, B])(mod: B => Option[B]): EndoFn[A] =
    a => l.modifyF[Option](mod)(a) getOrElse a

  def typicalRowModFn[Setup, Row, A, B](l: Optional[Row, Vector[A]], s: SortFn[B])(f: Setup => A => B): RowModFn[Setup, Row] =
    Some((setup, dir) => {
      val n = f(setup)
      val o = s.applyDir(dir).toOrdering
      def innerSort(i: Vector[A]): Option[Vector[A]] =
        if (i.isEmpty || i.tail.isEmpty)
          None
        else
          MutableArray(i).sortBySchwartzian(n)(o).iterator.toVector.some
      tryModEndo(l)(innerSort)
    })

  def consolidateRowModFns[Setup, Row](ss: IterableOnce[RowModFn[Setup, Row]]): RowModFn[Setup, Row] = {
    val fns = ss.iterator.flatten.toList
    if (fns.isEmpty)
      None
    else
      Some((setup, dir) => row => fns.foldLeft(row)((r, f) => f(setup, dir)(r)))
  }

  def reqCodeSorter[Setup, Row](optic: Optional[Row, Vector[ReqCode.Value]], bp: BlankPlacement) = {
    // TODO Sorting reqcodes by txt is inefficient. Trie => Vector[Int] would be better.
    val norm: ReqCode.Value => String = PlainText.reqCode
    apply[Setup, Row, String](
      rowMod = typicalRowModFn(optic, SortFn.stringNonEmpty)(_ => norm),
      prep   = _ => row => optic.getOption(row).flatMap(_.headOption).fold("")(norm),
      sort   = SortFn.string(bp))
  }
}