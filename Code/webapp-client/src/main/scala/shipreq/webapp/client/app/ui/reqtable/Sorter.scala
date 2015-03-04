package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import monocle.function.index
import monocle.std.mapIndex
import scala.annotation.tailrec
import scalaz.std.option.optionInstance

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.lib.Presentation
import shipreq.webapp.client.app.ui.reqtable.{SortMethod => SM, SortCriterion => SC, Column => C}
import SortMethod.{Asc, AscThenBlanks, BlanksThenAsc, AscHalf}

trait Sorter {
  type T
  def prepFn  : Sorter.PrepFn[T]
  def sortFn  : Sorter.SortFn[T]
  def rowModFn: Sorter.RowModFn
}

object Sorter {

  /** Extracts, pre-processes and normalises data before sorting. */
  type PrepFn[+T] = Setup => Row => T

  /** Sorts values in [[Expansion]] and [[MultiValues]]. */
  type RowModFn   = Option[(Setup, Dir) => EndoFn[Row]]

  def apply[A](prep: PrepFn[A], sort: SortFn[A], rowMod: Sorter.RowModFn = None): Sorter =
    new Sorter {
      override type T       = A
      override val prepFn   = prep
      override val sortFn   = sort
      override val rowModFn = rowMod
    }

  sealed trait BlankPlacement
  case object BlanksFirst extends BlankPlacement
  case object BlanksLast  extends BlankPlacement

  @inline implicit def autoBlanksFirst(a: BlanksThenAsc.type): BlankPlacement = BlanksFirst
  @inline implicit def autoBlanksLast (a: AscThenBlanks.type): BlankPlacement = BlanksLast

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

  object SortFn {
    val int: SortFn[Int] =
      SortFn((x, y) => if (x < y) -1 else if (x == y) 0 else 1)

    val intPair: SortFn[(Int, Int)] =
      int.pair

    val intList: BlankPlacement => SortFn[List[Int]] =
      int.byBlankPlacement(_.list)

    val intPairList: BlankPlacement => SortFn[List[(Int, Int)]] =
      intPair.byBlankPlacement(_.list)

    val stringNonEmpty: SortFn[String] =
      SortFn(_ compareTo _)

    val string: BlankPlacement => SortFn[String] =
      stringNonEmpty.byBlankPlacement(_.considerBlanks(_.isEmpty))
  }

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

    def option(bp: BlankPlacement): SortFn[Option[A]] =
      considerBlanksF[Option[A]](_.isEmpty)(_.get)(bp)

    def list(bp: BlankPlacement): SortFn[List[A]] = {
      @tailrec def go(as: List[A], bs: List[A]): Int = {
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

    def pair        : SortFn[(A, A)] = this &&& this
    def strengthL[B]: SortFn[(B, A)] = contramap(_._2)
    def strengthR[B]: SortFn[(A, B)] = contramap(_._1)

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

  // ===================================================================================================================
  // General

  type SorterForSMIB = SM.IgnoreBlanks   => Sorter
  type SorterForSMCB = SM.ConsiderBlanks => Sorter

  def reverseSorter(orig: Sorter): Sorter =
    new Sorter {
      override type T                  = orig.T
      override def prepFn  : PrepFn[T] = orig.prepFn
      override val sortFn  : SortFn[T] = orig.sortFn.reverse
      override def rowModFn: RowModFn  = orig.rowModFn.map(f => (s, dir) => f(s, dir.flip))
    }

  def SorterForSMIB[A](s: Sorter): SorterForSMIB =
    SM.resolverIB{ case Asc => s }(reverseSorter)

  def SorterForSMCB[A](f: BlankPlacement => Sorter): SorterForSMCB =
    SM.resolverCB({
      case b@ AscThenBlanks => f(b)
      case b@ BlanksThenAsc => f(b)
    })(reverseSorter)

  def tryModEndo[A, B](l: Optional[A, B])(mod: B => Option[B]): EndoFn[A] =
    a => l.modifyF[Option](mod)(a) getOrElse a

  def typicalRowModFn[A, B](l: Optional[Row, List[A]], s: SortFn[B])(f: Setup => A => B): RowModFn =
    Some((setup, dir) => {
      val n = f(setup)
      val o = s.applyDir(dir).strengthR[A].toOrdering
      val innerSort: List[A] => Option[List[A]] = {
        case Nil | _ :: Nil => None
        case as             => as.map(_ mapStrengthL n).sorted(o).map(_._2).some
      }
      tryModEndo(l)(innerSort)
    })

  // ===================================================================================================================
  // Specific

  /**
   * Project data prepared in a way that various sorts will use.
   */
  final class Setup(val p: Project) {

    val textNormalise = stringNormalise compose Presentation.textToString(p)

    lazy val reqTypesToMnemonicOrder: Map[ReqType.Id, Int] =
      p.reqTypes.map(_.tmap2(_.mnemonic.value, _.reqTypeId))
        .sortBy(_._1)
        .map(_._2)
        .zipWithIndex
        .toMap
        .withDefault(k => failedMust(0)("Unknown reqtype: " + k))

    lazy val tagOrder: Map[ApplicableTag.Id, Int] =
      p.tags.data.vstream(_.tag)
        .filterT[ApplicableTag]
        .map(_.tmap2(_.key.value |> stringNormalise, _.id))
        .sortBy(_._1)
        .map(_._2)
        .zipWithIndex
        .toMap
        .withDefault(k => failedMust(0)("Unknown tag: " + k))
  }

  def pubidNormaliser(setup: Setup): Pubid => (Int, Int) = {
    val reqTypeOrder = setup.reqTypesToMnemonicOrder
    p => {
      val o = reqTypeOrder(p.reqTypeId)
      (o, p.pos.value)
    }
  }

  val stringNormalise: EndoFn[String] =
    _.toLowerCase

  // RecCodeGroups are only displayed when sorting by code.
  // RecCodeGroups cannot have a blank code.
  // Therefore, RecCodeGroups cannot affect the conclusivity of a Pubid sort.
  val pubidSorter = Sorter[(Int, Int)](
    prep =
      setup => {
        val n = pubidNormaliser(setup)
        ;{ case r: GenericReqRow => n(r.req.pubId) }
      },
    sort = SortFn.intPair
  )

  def pubidListSorter(loc: Optional[Row, List[Pubid]]): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[List[(Int, Int)]](
        prep =
          setup => {
            val n = pubidNormaliser(setup)
            row => loc.getMaybe(row).cata(_ map n, Nil)
          },
        sort = SortFn.intPairList(bp),
        rowMod = typicalRowModFn(loc, SortFn.intPair)(pubidNormaliser)
    ))

  val reqTypeSorter = Sorter[Int](
    prep =
      setup => {
        val reqTypeOrder = setup.reqTypesToMnemonicOrder
        ;{ case r: GenericReqRow => reqTypeOrder(r.req.pubId.reqTypeId) }
      },
    sort = SortFn.int
  )

  def reqCodeSorter: SorterForSMCB =
    SorterForSMCB(bp =>
      // TODO Sorting reqcodes by txt is inefficient. Trie => List[Int] would be better.
      Sorter[String](
        // TODO headOption might not work in conjunction with rowModFn & reversing
        prep   = _ => row => Row._reqCodes.getMaybe(row).toOption.flatMap(_.headOption.map(_.txt)) getOrElse "",
        sort   = SortFn.string(bp),
        rowMod = typicalRowModFn(Row._reqCodes, SortFn.stringNonEmpty)(_ => _.txt)
    ))

  def tagSorter(loc: Optional[Row, List[ApplicableTag.Id]]): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[List[Int]](
        prep =
          setup => {
            val tagOrder = setup.tagOrder
            ;{ case r: GenericReqRow => r.mv.tags.map(tagOrder) }
          },
        sort = SortFn.intList(bp),
        rowMod = typicalRowModFn(loc, SortFn.int)(_.tagOrder.apply)
    ))

  def textSorter(f: Setup => Row => Text.Generic#OptionalText): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[String](
        prep =
          setup => {
            val g = f(setup)
            row => g(row) |> setup.textNormalise
          },
        sort = SortFn.string(bp)
      ))

  def customTextFieldSorter(id: CustomField.Text.Id): SorterForSMCB =
    textSorter { setup =>
      val data = setup.p.reqFieldData.data.text.getOrElse(id, Map.empty)
      ;{ case r: GenericReqRow => data.getOrElse(r.req.id, Nil) }
    }

  val descSorter: SorterForSMCB =
    textSorter(_ => { case r: GenericReqRow => r.req.desc })

  // ===================================================================================================================
  // Sort criteria

  val inconclusiveIB: C.SortInconclusive with C.NoBlanks => SorterForSMIB = {
    case C.ReqType => SorterForSMIB(reqTypeSorter)
  }

  val inconclusiveCB: C.SortInconclusive with C.HasBlanks => SorterForSMCB = {
    case C.CustomField(gid) =>
      gid match {
        case id: CustomField.Text       .Id => customTextFieldSorter(id)
        case id: CustomField.Tag        .Id => tagSorter(Row._cfTags ^|-? index(id))
        case id: CustomField.Implication.Id => pubidListSorter(Row._cfImps ^|-? index(id))
      }
    case C.Desc                             => descSorter
    case C.Code                             => reqCodeSorter
    case C.Tags                             => tagSorter(Row._tags)
    case C.ImplicationSrc                   => pubidListSorter(Row._implicationSrc)
    case C.ImplicationTgt                   => pubidListSorter(Row._implicationTgt)
  }

  val inconclusive: SC.Inconclusive => Sorter = {
    case sc: SC.InconclusiveCB => inconclusiveCB(sc.column)(sc.method)
    case sc: SC.InconclusiveIB => inconclusiveIB(sc.column)(sc.method)
  }

  def conclusive(sc: SC.Conclusive): Sorter = {
    val r: SorterForSMIB = sc.column match {
      case C.PubId => SorterForSMIB(pubidSorter)
    }
    r(sc.method)
  }

  // ===================================================================================================================
  final class FusedSorters(init: Vector[Sorter], last: Sorter) extends Sorter {
    import scalajs.js.{Array => JArray}

    private[this] val ss       = init :+ last
    private[this] val tSize    = ss.size + 1
    private[this] val rowIndex = tSize - 1

    // Unsafe and mutable.
    // [d₁, …, dₙ, row] where n = ss.size
    override type T = JArray[Any]

    override val prepFn: PrepFn[T] =
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

    override val rowModFn: RowModFn = {
      val fns = ss.flatMap(_.rowModFn.toVector)
      if (fns.isEmpty)
        None
      else
        Some((setup, dir) => row => fns.foldLeft(row)((r, f) => f(setup, dir)(r)))
    }

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
  }
}