package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import monocle.function.index
import monocle.std.mapIndex
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

  type SortFn[-A] = (A, A) => Int

  /** Sorts values in [[Expansion]] and [[MultiValues]]. */
  type RowModFn   = Option[(Setup, Boolean) => EndoFn[Row]] // Boolean indicates need to sort in reverse

  def apply[A](prep: PrepFn[A], rowMod: Sorter.RowModFn = None)(implicit sort: SortFn[A]): Sorter =
    new Sorter {
      override type T       = A
      override val prepFn   = prep
      override val sortFn   = sort
      override val rowModFn = rowMod
    }

  // ===================================================================================================================
  // General

  type SorterForSMIB = SM.IgnoreBlanks   => Sorter
  type SorterForSMCB = SM.ConsiderBlanks => Sorter

  sealed trait BlankPlacement
  case object BlanksFirst extends BlankPlacement
  case object BlanksLast  extends BlankPlacement

  @inline implicit def autoBlanksFirst(a: BlanksThenAsc.type): BlankPlacement = BlanksFirst
  @inline implicit def autoBlanksLast (a: AscThenBlanks.type): BlankPlacement = BlanksLast

  implicit val intSortFn: SortFn[Int] =
    Ordering.Int.compare

  implicit val intPairSortFn: SortFn[(Int, Int)] =
    sortFnCompose(intSortFn, intSortFn)

  implicit val intListFn: SortFn[List[Int]] =
    Ordering.Iterable[Int].compare

  implicit val intPairListSortFn: SortFn[List[(Int, Int)]] =
    Ordering.Iterable[(Int, Int)].compare

  implicit val stringSortFn: SortFn[String] =
    _ compareTo _

  def sortFnReverse[A](s: SortFn[A]): SortFn[A] =
    (a, b) => -s(a, b)

  @inline def sortFnReverseM[A](reverse: Boolean, s: SortFn[A]): SortFn[A] =
    if (reverse) sortFnReverse(s) else s

  def sortFnCompose[A, B](fa: SortFn[A], fb: SortFn[B]): SortFn[(A, B)] =
    (a, b) => {
      var         r = fa(a._1, b._1)
      if (r == 0) r = fb(a._2, b._2)
      r
    }

  def toOrdering[T](f: SortFn[T]): Ordering[T] =
    new Ordering[T] {
      def compare(x: T, y: T): Int = f(x, y)
    }

  def reverse(orig: Sorter): Sorter =
    new Sorter {
      override type T                  = orig.T
      override def prepFn  : PrepFn[T] = orig.prepFn
      override val sortFn  : SortFn[T] = sortFnReverse(orig.sortFn)
      override def rowModFn: RowModFn  = orig.rowModFn.map(f => (s, reverse) => f(s, !reverse))
    }

  def SorterForSMIB[A](s: Sorter): SorterForSMIB =
    SM.resolverIB{ case Asc => s }(reverse)

  def SorterForSMCB[A](f: BlankPlacement => Sorter): SorterForSMCB =
    SM.resolverCB({
      case b@ AscThenBlanks => f(b)
      case b@ BlanksThenAsc => f(b)
    })(reverse)

  //  def composeSorters(a: Sorter, b: Sorter): Sorter = {
  //    new Sorter {
  //      override type T = (a.T, b.T)
  //      override def prepare(p: Project): Row => T = {
  //        val fa = a prepare p
  //        val fb = b prepare p
  //        r => (fa(r), fb(r))
  //      }
  //      override val order: Ordering[T] =
  //        Ordering.Tuple2(a.order, b.order)
  //    }
  //  }

  def caterForBlanks[A, B, C](isBlank: A => Boolean, onBlank: B, onNonBlank: B, f: (B, A) => C)(bp: BlankPlacement): A => C = {
    def go(ib: B, nb: B): A => C =
      a => f(if (isBlank(a)) ib else nb, a)
    bp match {
      case BlanksFirst => go(onBlank, onNonBlank)
      case BlanksLast  => go(onNonBlank, onBlank)
    }
  }

  val caterForBlanksS: BlankPlacement => String => String =
    caterForBlanks[String, String, String](_.isEmpty, " ", "?", _ + _)

  val caterForBlanksOS: BlankPlacement => Option[String] => String =
    b => {
      val f = caterForBlanksS(b)
      o => f(o getOrElse "")
    }

  def stringSorter(prepFn: PrepFn[String]): Sorter =
    Sorter(prepFn(_) andThen stringNormalise)

  def stringSorterForSMIB(prep: PrepFn[String]): SorterForSMIB =
    SorterForSMIB(stringSorter(prep))

  def stringSorterForSMCB(prep: PrepFn[String]): SorterForSMCB =
    SorterForSMCB { bp =>
      val f = caterForBlanksS(bp)
      stringSorter(prep(_) andThen f)
    }

  val stringNormalise: EndoFn[String] =
    _.toLowerCase // search is case-insensitive

  def trySortList[A](o: Ordering[A]): List[A] => Option[List[A]] = {
      case Nil
         | _ :: Nil => None
      case as       => Some(as.sorted(o))
    }

  def trySortEndo[A, B](l: Optional[A, B])(mod: B => Option[B]): EndoFn[A] =
    a => l.modifyF[Option](mod)(a) getOrElse a

  def typicalRowModFn[A, B](l: Optional[Row, List[A]])(f: Setup => A => B)(implicit s: SortFn[B]): RowModFn =
    Some((setup, reverse) => {
      val ord = toOrdering(sortFnReverseM(reverse, s)).on(f(setup))
      trySortEndo(l)(trySortList(ord))
    })

  // ===================================================================================================================
  // Specific

  /**
   * Project data prepared in a way that various sorts will use.
   */
  class Setup(val p: Project) {

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

  // RecCodeGroups are only displayed when sorting by code.
  // RecCodeGroups cannot have a blank code.
  // Therefore, RecCodeGroups cannot affect the conclusivity of a Pubid sort.
  val pubidSorter = Sorter[(Int, Int)](
    setup => {
      val n = pubidNormaliser(setup)
      ;{ case r: GenericReqRow => n(r.req.pubId) }
    })

  def pubidListSorter(loc: Optional[Row, List[Pubid]]): SorterForSMCB =
    SorterForSMCB(bp => Sorter[List[(Int, Int)]](
      setup => {
        val n = pubidNormaliser(setup)
        row => loc.getMaybe(row).cata(_ map n, Nil)
      },
      typicalRowModFn(loc)(pubidNormaliser)
    ))

  val reqTypeSorter = Sorter[Int](
    setup => {
      val reqTypeOrder = setup.reqTypesToMnemonicOrder
      ;{ case r: GenericReqRow => reqTypeOrder(r.req.pubId.reqTypeId) }
    })

  // TODO Sorting reqcodes by txt is inefficient. Trie => List[Int] would be better.
  def reqCodeSorter: SorterForSMCB =
    SorterForSMCB(bp => Sorter[String](
      _ => {
        val cb = caterForBlanksOS(bp)
        row => {
          // TODO headOption might not work in conjunction with rowModFn & reversing
          val code = Row._reqCodes.getMaybe(row).toOption.flatMap(_.headOption.map(_.txt))
          cb(code)
        }
      },
      typicalRowModFn(Row._reqCodes)(_ => _.txt)
    ))

  def tagSorter(loc: Optional[Row, List[ApplicableTag.Id]]): SorterForSMCB =
    SorterForSMCB(bp => Sorter[List[Int]](
      setup => {
        val tagOrder = setup.tagOrder
        ;{ case r: GenericReqRow => r.mv.tags.map(tagOrder) }
      },
      typicalRowModFn(loc)(_.tagOrder.apply)
    ))

  def customTextFieldSorter(id: CustomField.Text.Id): SorterForSMCB =
    stringSorterForSMCB(setup => {
      val data  = setup.p.reqFieldData.data.text.getOrElse(id, Map.empty)
      val toStr = Presentation.textToString(setup.p)
      // Normalisation done in stringSorterForSMCB > stringSorter
      ;{
        case r: GenericReqRow => toStr(data.getOrElse(r.req.id, Nil))
      }
    })

  // ===================================================================================================================
  // Edge of the island

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
    case C.Desc                             => stringSorterForSMCB(_ => Row.desc)
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
        Some((setup, reverse) => row => fns.foldLeft(row)((r, f) => f(setup, reverse)(r)))
    }

    private def eachSortFn: Vector[SortFn[T]] =
      ss.zipWithIndex.map {
        case (s, i) =>
          val f = s.sortFn
          (as: T, bs: T) => {
            val a = as(i).asInstanceOf[s.T]
            val b = bs(i).asInstanceOf[s.T]
            f(a, b)
          }
      }

    override val sortFn: SortFn[T] =
      eachSortFn.reduce((f, g) =>
        (as: T, bs: T) => {
          val r = f(as, bs)
          if (r == 0) g(as, bs) else r
        })

    // method 2
    // val sortFnsReverse = eachSortFn.reverse.toArray
    // val starti = sortFnsReverse.length
    // val totalSortFn: F =
    //   (as, bs) => {
    //     var i = starti
    //     var r = 0
    //     while (r == 0 && i > 0) {
    //       i = i - 1
    //       r = sortFnsReverse(i)(as, bs)
    //     }
    //     r
    //   }

    def row(t: T): Row =
      t(rowIndex).asInstanceOf[Row]
  }
}