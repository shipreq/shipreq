package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.{Lens, Optional}
import monocle.function.index
import monocle.std.mapIndex
import monocle.macros.Lenses
import scala.scalajs.js
import scalaz.{Equal, Semigroup, Monoid}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util.{UnivEq, Vector1}
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ReqCodeTreeItem
import UnivEq.Implicits._

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

  // Workaround for stupid https://issues.scala-lang.org/browse/SI-6391
  def copyReqCodes   (nv: Vector[ReqCode.Value]  ): Expansion = copy(reqCodes = nv)
  def copyReqCodeTree(nv: Vector[ReqCodeTreeItem]): Expansion = copy(reqCodeTree = nv)

  def impsForCF(id: CustomField.Implication.Id): Vector[Pubid] =
    cfImps.getOrElse(id, Vector.empty)

  def tagsForCF(id: CustomField.Tag.Id): Vector[ApplicableTagId] =
    cfTags.getOrElse(id, Vector.empty)
}

object Expansion {
  val none = Expansion(Vector.empty, Vector.empty, Vector.empty, Vector.empty, UnivEq.emptyMap, UnivEq.emptyMap)

  implicit def equality: UnivEq[Expansion] = UnivEq.derive

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
  implicit def equality: UnivEq[MultiValues] = UnivEq.derive

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
  val id      : Row.Id
  def sourceId: Row.SourceId
  def live    : Live
}

/**
 * @param instanceId An arbitrary number that, coupled with `req.id` serves to uniquely identify a row.
 *                   Reason is that the same GenericReq can appear in multiple rows.
 */
case class GenericReqRow(req: GenericReq, live: Live, exp: Expansion, mv: MultiValues, instanceId: Int) extends Row {
  override val id       = Row.GenericReqRowId(req.id, instanceId)
  override def sourceId = Row.GenericReqRowSourceId(req.id)
  override def toString = s"\n$req\n$exp\n$mv\n"
}

case class ReqCodeGroupRow(group: ReqCodeGroup, reqCode: ReqCode.Value, reqCodeTreeItem: Option[ReqCodeTreeItem]) extends Row {
  override val id       = Row.ReqCodeGroupRowId(reqCodeId)
  override def sourceId = Row.ReqCodeGroupRowSourceId(reqCodeId)
  override def live     = group.live
  def reqCodeId         = group.id
}

object Row {

  // ===================================================================================================================

  /**
   * Uniquely identifies a row, including distinguishing expansions of the same req.
   */
  sealed trait Id {
    /** A stable, unique value so that React can correctly identify each row. */
    def key: js.Any
  }

  /**
   * @param instanceId An arbitrary number that, coupled with `reqId` serves to uniquely identify a row.
   *                   Reason is that the same GenericReq can appear in multiple rows.
   */
  case class GenericReqRowId(reqId: GenericReqId, instanceId: Int) extends Id {
    override def key =
      if (instanceId == 0)
        reqId.value
      else
        reqId.value.toString + (' ' + instanceId).toChar.toString
  }

  case class ReqCodeGroupRowId(value: ReqCodeId) extends Id {
    override def key =
      "C" + value.value
  }

  implicit def idEqualityR  : UnivEq[GenericReqRowId]   = UnivEq.derive
  implicit def idEqualityG  : UnivEq[ReqCodeGroupRowId] = UnivEq.derive
  implicit def idEquality   : UnivEq[Id]                = UnivEq.derive
  implicit val idReusability: Reusability[Id]           = Reusability.byEqual

  // ===================================================================================================================

  /**
   * Uniquely identifies the source of a row, disregarding features such as expansions.
   */
  sealed trait SourceId
  case class GenericReqRowSourceId(reqId: GenericReqId) extends SourceId
  case class ReqCodeGroupRowSourceId(value: ReqCodeId) extends SourceId

  implicit def sourceIdEqualityR  : UnivEq[GenericReqRowSourceId]   = UnivEq.derive
  implicit def sourceIdEqualityG  : UnivEq[ReqCodeGroupRowSourceId] = UnivEq.derive
  implicit def sourceIdEquality   : UnivEq[SourceId]                = UnivEq.derive
  implicit val sourceIdReusability: Reusability[SourceId]           = Reusability.byEqual

  // ===================================================================================================================

  implicit def rowEqualityR  : UnivEq[GenericReqRow]     = UnivEq.derive
  implicit def rowEqualityG  : UnivEq[ReqCodeGroupRow]   = UnivEq.derive
  implicit def rowEquality   : UnivEq[Row]               = UnivEq.derive
  implicit val rowReusability: Reusability[Row]          = Reusability.byRefOrEqual

  val expansion = Optional[Row, Expansion] {
    case r: GenericReqRow   => Some(r.exp)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case GenericReqRow(r, l, _, m, i) => GenericReqRow(r, l, nv, m, i)
    case r: ReqCodeGroupRow           => r
  })

  val multiValues = Optional[Row, MultiValues] {
    case r: GenericReqRow   => Some(r.mv)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case GenericReqRow(r, l, e, _, i) => GenericReqRow(r, l, e, nv, i)
    case r: ReqCodeGroupRow           => r
  })

  val reqCodes = Lens[Row, Vector[ReqCode.Value]] {
    case r: GenericReqRow   => r.exp.reqCodes
    case r: ReqCodeGroupRow => Vector1(r.reqCode)
  }(nv => {
    case GenericReqRow(r, l, e, m, i)         => GenericReqRow(r, l, e.copyReqCodes(nv), m, i)
    case r: ReqCodeGroupRow if nv.length == 1 => r.copy(reqCode = nv.head)
    case r: ReqCodeGroupRow if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })
  val reqCodesO = reqCodes.asOptional

  val reqCodeTree = Lens[Row, Vector[ReqCodeTreeItem]] {
    case r: GenericReqRow   => r.exp.reqCodeTree
    case r: ReqCodeGroupRow => r.reqCodeTreeItem.toVector
  }(nv => {
    case GenericReqRow(r, l, e, m, i) => GenericReqRow(r, l, e.copyReqCodeTree(nv), m, i)
    case r: ReqCodeGroupRow if nv.length == 1 => r.copy(reqCodeTreeItem = Some(nv.head))
    case r: ReqCodeGroupRow if nv.length == 0 => r.copy(reqCodeTreeItem = None)
    case r: ReqCodeGroupRow if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })

  type OV[A]     = Optional[Row, Vector[A]]
  type OMV[K, V] = Optional[Row, Map[K, Vector[V]]]

  val implicationSrc: OV[Pubid]                                = Row.expansion   ^|-> Expansion.implicationSrc
  val implicationTgt: OV[Pubid]                                = Row.expansion   ^|-> Expansion.implicationTgt
  val cfImps        : OMV[CustomField.Implication.Id, Pubid]   = Row.expansion   ^|-> Expansion.cfImps
  val cfTags        : OMV[CustomField.Tag.Id, ApplicableTagId] = Row.expansion   ^|-> Expansion.cfTags
  val tags          : OV[ApplicableTagId]                      = Row.multiValues ^|-> MultiValues.tags

  private def mmLens[K, V](k: K): Lens[Map[K, Vector[V]], Vector[V]] =
    Lens[Map[K, Vector[V]], Vector[V]](_.getOrElse(k, Vector.empty))(vs => _.updated(k, vs))

  def cfImp(id: CustomField.Implication.Id): OV[Pubid]           = cfImps ^|-> mmLens(id)
  def cfTag(id: CustomField.Tag        .Id): OV[ApplicableTagId] = cfTags ^|-> mmLens(id)
}
