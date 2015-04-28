package shipreq.webapp.client.app.ui.reqtable

import monocle.{Lens, Optional}
import monocle.function.index
import monocle.std.mapIndex
import monocle.macros.Lenses
import scalaz.{Equal, Semigroup, Monoid}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.TypeclassDerivation._

/**
 * Representation of a ReqCode when viewed like a tree.
 *
 * @see [[ViewSettings.viewReqCodesAsTree]]
 */
case class ReqCodeTreeItem(indent: Vector[ReqCodeTreeItem.Indent], suffix: ReqCode.Value)

object ReqCodeTreeItem {
  sealed trait Indent

  /**
   * Unit of indentation for when a ReqCode is a direct child of the one above.
   *
   * `a.b.c.d` after `a.b.c` would result in 3 of these with `.d` as the suffix.
   */
  case object IndentChild extends Indent

  /**
   * Unit of indentation that consumes a fixed number of spaces.
   *
   * @param length ≥ 1 The length of the common node (excluding the ".").
   */
  case class IndentSpace(length: Int) extends Indent

  implicit def indentEquality: UnivEq[Indent] = UnivEq.force
  implicit val itemEquality: UnivEq[ReqCodeTreeItem] = deriveUnivEq

}

// =====================================================================================================================

/**
 * Replacement values for a requirement at a specific row.
 *
 * Due to sorting criteria, the same requirement may appear multiple times with certain composite values
 * split across rows. This process is dubbed expansion and this class houses the different values its corresponding row
 * will display.
 *
 * Example: if a row is implied by two sources and the table is sorted by implication-source, then the row will
 * appear twice - once for each implicatee.
 */
// TODO Make imp naming consistent
@Lenses
case class Expansion(implicationSrc: Vector[Pubid],
                     implicationTgt: Vector[Pubid],
                     reqCodes      : Vector[ReqCode.Value],
                     reqCodeTree   : Vector[ReqCodeTreeItem],
                     cfImps        : Map[CustomField.Implication.Id, Vector[Pubid]],
                     cfTags        : Map[CustomField.Tag.Id,         Vector[ApplicableTag.Id]]) {

  def impsForCF(id: CustomField.Implication.Id): Vector[Pubid] =
    cfImps.getOrElse(id, Vector.empty)

  def tagsForCF(id: CustomField.Tag.Id): Vector[ApplicableTag.Id] =
    cfTags.getOrElse(id, Vector.empty)
}

object Expansion {
  val none = Expansion(Vector.empty, Vector.empty, Vector.empty, Vector.empty, UnivEq.emptyMap, UnivEq.emptyMap)

  implicit val equality: UnivEq[Expansion] = deriveUnivEq

  implicit val reqCodeTreeM: Monoid[Vector[ReqCodeTreeItem]] =
    scalaz.std.vector.vectorMonoid

  implicit def vectorUniqSemigroup[A: Equal]: Semigroup[Vector[A]] =
    new Semigroup[Vector[A]] {
      override def append(x: Vector[A], y: => Vector[A]) =
        y.foldLeft(x)((q, a) =>
          if (x.exists(Equal[A].equal(_, a))) q else q :+ a)
    }

  implicit val monoid: Monoid[Expansion] =
    new Monoid[Expansion] {
      override def zero = none
      override def append(a: Expansion, _b: => Expansion) = {
        val b = _b
        Expansion(
          a.implicationSrc |+| b.implicationSrc,
          a.implicationTgt |+| b.implicationTgt,
          a.reqCodes       |+| b.reqCodes,
          a.reqCodeTree    |+| b.reqCodeTree,
          a.cfImps         |+| b.cfImps,
          a.cfTags         |+| b.cfTags)
      }
    }
}

// =====================================================================================================================

/**
 * Sortable data (ie. lists) that are never expanded.
 */
@Lenses
case class MultiValues(tags: Vector[ApplicableTag.Id])

object MultiValues {
  implicit val equality: UnivEq[MultiValues] = deriveUnivEq

  import Expansion.vectorUniqSemigroup

  implicit val monoid: Monoid[MultiValues] =
    new Monoid[MultiValues] {
      override def zero = MultiValues(Vector.empty)
      override def append(a: MultiValues, _b: => MultiValues) = {
        val b = _b
        MultiValues(a.tags |+| b.tags)
      }
    }
}

// =====================================================================================================================

sealed trait Row {
  def fold[A](g: GenericReqRow => A): A
  def id: Row.Id
}

case class GenericReqRow(req: GenericReq, exp: Expansion, mv: MultiValues) extends Row {
  override def fold[A](g: GenericReqRow => A): A = g(this)
  override def id = req.id
  override def toString = s"\n$req\n$exp\n$mv\n"
}

//case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row

object Row {
  type Id = GenericReq.Id

  implicit val equalityG: UnivEq[GenericReqRow]   = deriveUnivEq
//  implicit val equalityC: UnivEq[ReqCodeGroupRow] = deriveUnivEq
  implicit val equality : UnivEq[Row]             = deriveUnivEq

  val expansion = Optional[Row, Expansion] {
    case r: GenericReqRow => Some(r.exp)
  }(e => {
    case GenericReqRow(r, _, m) => GenericReqRow(r, e, m)
  })

  val multiValues = Optional[Row, MultiValues] {
    case r: GenericReqRow => Some(r.mv)
  }(mv => {
    case GenericReqRow(r, e, _) => GenericReqRow(r, e, mv)
  })

  val implicationSrc = Row.expansion   ^|-> Expansion.implicationSrc
  val implicationTgt = Row.expansion   ^|-> Expansion.implicationTgt
  val reqCodes       = Row.expansion   ^|-> Expansion.reqCodes
  val reqCodeTree    = Row.expansion   ^|-> Expansion.reqCodeTree
  val cfImps         = Row.expansion   ^|-> Expansion.cfImps
  val cfTags         = Row.expansion   ^|-> Expansion.cfTags
  val tags           = Row.multiValues ^|-> MultiValues.tags

  private def mmLens[K, V](k: K): Lens[Map[K, Vector[V]], Vector[V]] =
    Lens[Map[K, Vector[V]], Vector[V]](_.getOrElse(k, Vector.empty))(vs => _.updated(k, vs))

  def cfImp(id: CustomField.Implication.Id) = cfImps ^|-> mmLens(id)
  def cfTag(id: CustomField.Tag        .Id) = cfTags ^|-> mmLens(id)
}
