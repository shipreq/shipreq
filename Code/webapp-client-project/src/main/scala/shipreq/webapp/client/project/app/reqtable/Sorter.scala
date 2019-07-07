package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.Optional
import monocle.function.Index._
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaz.std.option.optionInstance
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.data.reqtable.{Column => C, SortCriterion => SC}
import shipreq.webapp.base.sort.SortMethod.{Asc, AscThenBlanks, BlanksThenAsc}
import shipreq.webapp.base.sort.{SortMethod => SM}
import shipreq.webapp.base.text.PlainText
import shipreq.base.util.{Applicable, NotApplicable}

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

    def pair: SortFn[(A, A)] = this &&& this

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

  def typicalRowModFn[A: ClassTag, B](l: Optional[Row, Vector[A]], s: SortFn[B])(f: Setup => A => B): RowModFn =
    Some((setup, dir) => {
      val n = f(setup)
      val o = s.applyDir(dir).toOrdering
      def innerSort(i: Vector[A]): Option[Vector[A]] =
        if (i.isEmpty || i.tail.isEmpty)
          None
        else
          MutableArray(i).sortBySchwartzian(n)(o).to[Vector].some
      tryModEndo(l)(innerSort)
    })

  // ===================================================================================================================
  // Specific

  type TagOrder = DataLogic.TagOrder

  /**
   * Project data prepared in a way that various sorts will use.
   */
  final class Setup(val p: Project, plainText: PlainText.ForProject.NoCtx) {

    def normalisedText(f: PlainText.ForProject.NoCtx => String) =
      stringNormalise(f(plainText))

    val applicability: Applicability[Column, Row] =
      Row.applicability(p.config.applicability)

    @inline def reqTypesToMnemonicOrder =
      p.config.reqTypes.order

    lazy val tagByNameOrder: TagOrder =
      DataLogic.tagOrderByName(p.config.tags.tree)

    lazy val tagByPosOrder: TagOrder =
      DataLogic.tagOrderByPos(p.config.tags)
  }

  def pubidNormaliser(setup: Setup): Pubid => (Int, Int) =
    DataLogic.pubidSortKeyFn(setup.p.config)

  @inline def stringNormalise: EndoFn[String] =
    DataLogic.normaliseStringForSorting

  // CodeGroups are only displayed when sorting by code.
  // CodeGroups cannot have a blank code.
  // Therefore, CodeGroups cannot affect the conclusivity of a Pubid sort.
  val pubidSorter = Sorter[(Int, Int)](
    prep =
      setup => {
        val n = pubidNormaliser(setup)
        val `n/a` = (-1, -1)
        ;{
          case r: Row.ForReq       => n(r.req.pubid)
          case r: Row.ForCodeGroup => `n/a`
        }
      },
    sort = SortFn.intPair
  )

  def pubidVectorSorter(loc: Optional[Row, Vector[Pubid]]): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[Vector[(Int, Int)]](
        rowMod = typicalRowModFn(loc, SortFn.intPair)(pubidNormaliser),
        prep =
          setup => {
            val n = pubidNormaliser(setup)
            row => loc.getOption(row).fold(Vector.empty[(Int, Int)])(_ map n)
          },
        sort = SortFn.intPairVector(bp)
    ))

  val reqTypeSorter = Sorter[Int](
    prep = setup => {
      case r: Row.ForReq       => setup.reqTypesToMnemonicOrder(r.req.pubid.reqTypeId)
      case r: Row.ForCodeGroup => -1
    },
    sort = SortFn.int
  )

  def reqCodeSorter: SorterForSMCB =
    SorterForSMCB { bp =>
      // TODO Sorting reqcodes by txt is inefficient. Trie => Vector[Int] would be better.
      val norm: ReqCode.Value => String = PlainText.reqCode
      Sorter[String](
        rowMod = typicalRowModFn(Row.reqCodesO, SortFn.stringNonEmpty)(_ => norm),
        prep   = _ => row => Row.reqCodes.get(row).headOption map norm getOrElse "",
        sort   = SortFn.string(bp))
    }

  def tagSorter(loc: Optional[Row, Vector[ApplicableTagId]], order: Setup => TagOrder): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[Vector[Int]](
        rowMod = typicalRowModFn(loc, SortFn.int)(order(_).apply),
        prep   = setup => loc.getOption(_).fold(Vector.empty[Int])(_ map order(setup)),
        sort   = SortFn.intVector(bp)
    ))

  def textSorterS(c: Column, f: Setup => PlainText.ForProject.NoCtx => Row => String): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[String](
        prep = setup => {
          val g = f(setup)
          val rowApplicability = setup.applicability.byField(c)
          (row: Row) => rowApplicability(row) match {
            case Applicable    => setup.normalisedText(g(_)(row))
            case NotApplicable => ""
          }
        },
        sort = SortFn.string(bp)
      ))

  def textSorter(c: Column, f: PlainText.ForProject.NoCtx => Row => String): SorterForSMCB =
    textSorterS(c, _ => f)

  def customTextFieldSorter(id: CustomField.Text.Id, c: Column): SorterForSMCB =
    textSorter(c, p => {
      case r: Row.ForReq       => p.customTextField(id)(r.req) getOrElse ""
      case r: Row.ForCodeGroup => ""
    })

  val titleSorter: SorterForSMCB =
    textSorter(C.Title, p => {
      case r: Row.ForReq       => p.reqTitle(r.req)
      case r: Row.ForCodeGroup => p.codeGroupTitle(r.group)
    })

  def deletionReasonSorter: SorterForSMCB =
    textSorterS(C.DeletionReason, s => pt => {
      case r: Row.ForReq       => pt.deleteReasonForReq(r.req) getOrElse ""
      case _: Row.ForCodeGroup => pt.deleteReasonForCodeGroup getOrElse ""
    })

  // ===================================================================================================================
  // Sort criteria

  val inconclusiveIB: C.SortInconclusiveNoBlanks => SorterForSMIB = {
    case C.ReqType => SorterForSMIB(reqTypeSorter)
  }

  val inconclusiveCB: C.SortInconclusiveHasBlanks => SorterForSMCB = {
    case c: C.CustomField =>
      c.id match {
        case id: CustomField.Text       .Id => customTextFieldSorter(id, c)
        case id: CustomField.Tag        .Id => tagSorter(Row.cfTags ^|-? index(id), _.tagByPosOrder)
        case id: CustomField.Implication.Id => pubidVectorSorter(Row.cfImps ^|-? index(id))
      }
    case C.Title                            => titleSorter
    case C.Code                             => reqCodeSorter
    case C.Tags                             => tagSorter(Row.tags, _.tagByNameOrder)
    case C.Implications(dir)                => pubidVectorSorter(Row.implications(dir))
    case C.DeletionReason                   => deletionReasonSorter
  }

  val inconclusive: SC.Inconclusive => Sorter = {
    case sc: SC.InconclusiveCB => inconclusiveCB(sc.column)(sc.method)
    case sc: SC.InconclusiveIB => inconclusiveIB(sc.column)(sc.method)
  }

  def conclusive(sc: SC.Conclusive): Sorter = {
    val r: SorterForSMIB = sc.column match {
      case C.Pubid => SorterForSMIB(pubidSorter)
    }
    r(sc.method)
  }

  /**
   * Sort visible data in [[Expansion]]/[[MultiValues]] that won't be sorted by [[SortCriteria]].
   */
  def sortUnspecified(view: View): RowModFn = {
    val fns =
      view.columns.whole
        .iterator
        .filterSubType[C.SortInconclusive]
        .filterNot(view.isOrdered)
        .map({
          case c: C.SortInconclusiveHasBlanks => inconclusiveCB(c)(SM.BlanksThenAsc)
          case c: C.SortInconclusiveNoBlanks  => inconclusiveIB(c)(SM.Asc)
        })
        .map(_.rowModFn)

    consolidateRowModFns(fns)
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

    override val rowModFn: RowModFn =
      consolidateRowModFns(ss.toStream.map(_.rowModFn))

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

  def consolidateRowModFns(ss: TraversableOnce[RowModFn]): RowModFn = {
    val fns = ss.toStream.flatMap(_.toStream)
    if (fns.isEmpty)
      None
    else
      Some((setup, dir) => row => fns.foldLeft(row)((r, f) => f(setup, dir)(r)))
  }
}