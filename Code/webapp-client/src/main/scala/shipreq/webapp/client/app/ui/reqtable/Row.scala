package shipreq.webapp.client.app.ui.reqtable

import monocle.{Lens, Optional}
import monocle.function.index
import monocle.std.mapIndex
import monocle.macros.Lenses
import scalaz.{Equal, Semigroup, Monoid}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util.{UnivEq, Vector1}
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.base.TypeclassDerivation._

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
                     cfTags        : Map[CustomField.Tag.Id,         Vector[ApplicableTagId]]) {

  def impsForCF(id: CustomField.Implication.Id): Vector[Pubid] =
    cfImps.getOrElse(id, Vector.empty)

  def tagsForCF(id: CustomField.Tag.Id): Vector[ApplicableTagId] =
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
case class MultiValues(tags: Vector[ApplicableTagId])

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
  def id: Row.Id
}

case class GenericReqRow(req: GenericReq, exp: Expansion, mv: MultiValues) extends Row {
  override def id = Row.GenericReqRowId(req.id)
  override def toString = s"\n$req\n$exp\n$mv\n"
}

case class ReqCodeGroupRow(reqCodeId      : ReqCode.Id,
                           group          : ReqCodeGroup,
                           reqCode        : ReqCode.Value,
                           reqCodeTreeItem: Option[ReqCodeTreeItem]) extends Row {
  override def id = Row.ReqCodeGroupRowId(reqCodeId)
}

object Row {
  sealed trait Id
  case class GenericReqRowId(value: GenericReqId) extends Id
  case class ReqCodeGroupRowId(value: ReqCode.Id) extends Id

  implicit val idEqualityR : UnivEq[GenericReqRowId]   = deriveUnivEq
  implicit val idEqualityG : UnivEq[ReqCodeGroupRowId] = deriveUnivEq
  implicit val idEquality  : UnivEq[Id]                = deriveUnivEq
  implicit val rowEqualityR: UnivEq[GenericReqRow]     = deriveUnivEq
  implicit val rowEqualityG: UnivEq[ReqCodeGroupRow]   = deriveUnivEq
  implicit val rowEquality : UnivEq[Row]               = deriveUnivEq

  val expansion = Optional[Row, Expansion] {
    case r: GenericReqRow   => Some(r.exp)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case GenericReqRow(r, _, m) => GenericReqRow(r, nv, m)
    case r: ReqCodeGroupRow     => r
  })

  val multiValues = Optional[Row, MultiValues] {
    case r: GenericReqRow   => Some(r.mv)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case GenericReqRow(r, e, _) => GenericReqRow(r, e, nv)
    case r: ReqCodeGroupRow     => r
  })

  val reqCodes = Lens[Row, Vector[ReqCode.Value]] {
    case r: GenericReqRow   => r.exp.reqCodes
    case r: ReqCodeGroupRow => Vector1(r.reqCode)
  }(nv => {
    case GenericReqRow(r, e, m) => GenericReqRow(r, e.copy(reqCodes = nv), m)
    case r: ReqCodeGroupRow if nv.length == 1 => r.copy(reqCode = nv.head)
    case r: ReqCodeGroupRow if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })
  val reqCodesO = reqCodes.asOptional

  val reqCodeTree = Lens[Row, Vector[ReqCodeTreeItem]] {
    case r: GenericReqRow   => r.exp.reqCodeTree
    case r: ReqCodeGroupRow => r.reqCodeTreeItem.toVector
  }(nv => {
    case GenericReqRow(r, e, m) => GenericReqRow(r, e.copy(reqCodeTree = nv), m)
    case r: ReqCodeGroupRow if nv.length == 1 => r.copy(reqCodeTreeItem = Some(nv.head))
    case r: ReqCodeGroupRow if nv.length == 0 => r.copy(reqCodeTreeItem = None)
    case r: ReqCodeGroupRow if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })

  val implicationSrc = Row.expansion   ^|-> Expansion.implicationSrc
  val implicationTgt = Row.expansion   ^|-> Expansion.implicationTgt
  val cfImps         = Row.expansion   ^|-> Expansion.cfImps
  val cfTags         = Row.expansion   ^|-> Expansion.cfTags
  val tags           = Row.multiValues ^|-> MultiValues.tags

  private def mmLens[K, V](k: K): Lens[Map[K, Vector[V]], Vector[V]] =
    Lens[Map[K, Vector[V]], Vector[V]](_.getOrElse(k, Vector.empty))(vs => _.updated(k, vs))

  def cfImp(id: CustomField.Implication.Id) = cfImps ^|-> mmLens(id)
  def cfTag(id: CustomField.Tag        .Id) = cfTags ^|-> mmLens(id)
}
