package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import monocle.function.index
import monocle.std.mapIndex
import scala.annotation.tailrec
import scalaz.std.option.optionInstance

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{PlainText, Text}
import shipreq.webapp.client.app.ui.reqtable.{SortMethod => SM, SortCriterion => SC, Column => C}
import SortMethod.{Asc, AscThenBlanks, BlanksThenAsc}

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

  def typicalRowModFn[A, B](l: Optional[Row, Vector[A]], s: SortFn[B])(f: Setup => A => B): RowModFn =
    Some((setup, dir) => {
      val n = f(setup)
      val o = s.applyDir(dir).strengthR[A].toOrdering
      def innerSort(i: Vector[A]): Option[Vector[A]] =
        if (i.isEmpty || i.tail.isEmpty)
          None
        else
          i.map(_ mapStrengthL n).sorted(o).map(_._2).some
      tryModEndo(l)(innerSort)
    })

  // ===================================================================================================================
  // Specific

  type TagOrder = Map[ApplicableTagId, Int]

  /**
   * Project data prepared in a way that various sorts will use.
   */
  final class Setup(val p: Project, plainText: PlainText.ForProject) {

    def normalisedText(f: PlainText.ForProject => String) =
      stringNormalise(f(plainText))

    def ordermap[A](name: String, as: Stream[A]): Map[A, Int] =
      as.zipWithIndex.toMap
      .withDefault(k => failedMust(0)(s"Unknown $name: " + k))

    val applicability = Applicability(p)

    lazy val reqTypesToMnemonicOrder: Map[ReqTypeId, Int] =
      ordermap("reqtype",
        p.reqTypes.map(_.tmap2(_.mnemonic.value, _.reqTypeId))
          .sortBy(_._1)
          .map(_._2)
      )

    lazy val tagByNameOrder: TagOrder =
      ordermap("tag",
        p.tags.data.vstream(_.tag)
          .filterT[ApplicableTag]
          .map(_.tmap2(_.key.value |> stringNormalise, _.id))
          .sortBy(_._1)
          .map(_._2)
      )

    lazy val tagByPosOrder: TagOrder =
      ordermap("tag",
        TagTree.flatten(p.tags.data)(_ => true, TagTree.FlatRow.FilterPolicy.OmitNothing)
          .toStream
          .map(_.id)
          .filterT[ApplicableTagId]
      )
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

  // ReqCodeGroups are only displayed when sorting by code.
  // ReqCodeGroups cannot have a blank code.
  // Therefore, ReqCodeGroups cannot affect the conclusivity of a Pubid sort.
  val pubidSorter = Sorter[(Int, Int)](
    prep =
      setup => {
        val n = pubidNormaliser(setup)
        val `n/a` = (-1, -1)
        ;{
          case r: GenericReqRow   => n(r.req.pubid)
          case r: ReqCodeGroupRow => `n/a`
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
      case r: GenericReqRow   => setup.reqTypesToMnemonicOrder(r.req.pubid.reqTypeId)
      case r: ReqCodeGroupRow => -1
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

  def textSorter(c: Column, f: PlainText.ForProject => Row => String): SorterForSMCB =
    SorterForSMCB(bp =>
      Sorter[String](
        prep = setup => setup.applicability(c).wrap((row: Row) => setup.normalisedText(f(_)(row)))(""),
        sort = SortFn.string(bp)
      ))

  def customTextFieldSorter(id: CustomField.Text.Id, c: Column): SorterForSMCB =
    textSorter(c, p => {
      case r: GenericReqRow   => p.customTextField(id)(r.req.id) getOrElse ""
      case r: ReqCodeGroupRow => ""
    })

  val titleSorter: SorterForSMCB =
    textSorter(C.Title, p => {
      case r: GenericReqRow   => p.reqTitle(r.req)
      case r: ReqCodeGroupRow => p.reqCodeGroupTitle(r.groupAndId)
    })

  // ===================================================================================================================
  // Sort criteria

  val inconclusiveIB: C.SortInconclusive with C.NoBlanks => SorterForSMIB = {
    case C.ReqType => SorterForSMIB(reqTypeSorter)
  }

  val inconclusiveCB: C.SortInconclusive with C.HasBlanks => SorterForSMCB = {
    case c: C.CustomField =>
      c.id match {
        case id: CustomField.Text       .Id => customTextFieldSorter(id, c)
        case id: CustomField.Tag        .Id => tagSorter(Row.cfTags ^|-? index(id), _.tagByPosOrder)
        case id: CustomField.Implication.Id => pubidVectorSorter(Row.cfImps ^|-? index(id))
      }
    case C.Title                            => titleSorter
    case C.Code                             => reqCodeSorter
    case C.Tags                             => tagSorter(Row.tags, _.tagByNameOrder)
    case C.ImplicationSrc                   => pubidVectorSorter(Row.implicationSrc)
    case C.ImplicationTgt                   => pubidVectorSorter(Row.implicationTgt)
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
  def sortUnspecified(vs: ViewSettings): RowModFn = {
    val fns =
      vs.columns.whole
        .filterT[C.SortInconclusive]
        .filterNot(vs.isOrdered)
        .map({
          case c: C.HasBlanks => inconclusiveCB(c)(SM.BlanksThenAsc)
          case c: C.NoBlanks  => inconclusiveIB(c)(SM.Asc)
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