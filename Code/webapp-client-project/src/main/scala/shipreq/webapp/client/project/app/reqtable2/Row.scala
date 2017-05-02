package shipreq.webapp.client.project.app.reqtable2

import japgolly.scalajs.react.Key
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.{Lens, Optional}
import monocle.macros.Lenses
import scalaz.{Equal, Monoid, Semigroup}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.project.feature.EditorFeature.RowKey

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
case class ReqRow(req: Req, live: Live, exp: Expansion, mv: MultiValues, instanceId: Int) extends Row {
  override val id       = Row.ReqRowId(req.id, instanceId)
  override def sourceId = Row.ReqRowSourceId(req.id)
  override def toString = s"\n$req\n$exp\n$mv\n"
}

case class ReqCodeGroupRow(group: ReqCodeGroup, reqCode: ReqCode.Value, reqCodeTreeItem: Option[ReqCodeTreeItem]) extends Row {
  override val id       = Row.ReqCodeGroupRowId(reqCodeId)
  override def sourceId = Row.ReqCodeGroupRowSourceId(reqCodeId)
  override def live     = group.live
  def reqCodeId         = group.id
}

object Row {

  implicit object SubjectRow extends Applicability.Subject[Row] {
    override def applicable(row: Row, f: ReqTypeId => Applicable) =
      row match {
        case r: ReqRow          => Applicability.SubjectReq.applicable(r.req, f)
        case _: ReqCodeGroupRow => NotApplicable
      }
  }

  // ===================================================================================================================

  /**
   * Uniquely identifies a row, including distinguishing expansions of the same req.
   */
  sealed trait Id {
    /** A stable, unique value so that React can correctly identify each row. */
    def key: Key
  }

  /**
   * @param instanceId An arbitrary number that, coupled with `reqId` serves to uniquely identify a row.
   *                   Reason is that the same GenericReq can appear in multiple rows.
   */
  case class ReqRowId(reqId: ReqId, instanceId: Int) extends Id {
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

  implicit def idEqualityR  : UnivEq[ReqRowId]          = UnivEq.derive
  implicit def idEqualityG  : UnivEq[ReqCodeGroupRowId] = UnivEq.derive
  implicit def idEquality   : UnivEq[Id]                = UnivEq.derive
  implicit val idReusability: Reusability[Id]           = Reusability.byUnivEq

  // ===================================================================================================================

  /**
   * Uniquely identifies the source of a row, disregarding features such as expansions.
   */
  sealed trait SourceId
  case class ReqRowSourceId(reqId: ReqId) extends SourceId
  case class ReqCodeGroupRowSourceId(value: ReqCodeId) extends SourceId

  implicit def sourceIdEqualityR  : UnivEq[ReqRowSourceId]          = UnivEq.derive
  implicit def sourceIdEqualityG  : UnivEq[ReqCodeGroupRowSourceId] = UnivEq.derive
  implicit def sourceIdEquality   : UnivEq[SourceId]                = UnivEq.derive
  implicit val sourceIdReusability: Reusability[SourceId]           = Reusability.byUnivEq

  val SourceIdToEditorRow = Intersection[SourceId, RowKey] {
    case ReqRowSourceId         (id) => Some(RowKey.Req         (id))
    case ReqCodeGroupRowSourceId(id) => Some(RowKey.ReqCodeGroup(id))
  } {
    case RowKey.Req         (id) => Some(ReqRowSourceId         (id))
    case RowKey.ReqCodeGroup(id) => Some(ReqCodeGroupRowSourceId(id))
    case RowKey.UseCaseSteps     => None
  }

  // ===================================================================================================================

  implicit def rowEqualityR  : UnivEq[ReqRow]            = UnivEq.derive
  implicit def rowEqualityG  : UnivEq[ReqCodeGroupRow]   = UnivEq.derive
  implicit def rowEquality   : UnivEq[Row]               = UnivEq.derive
  implicit val rowReusability: Reusability[Row]          = Reusability.byRefOrUnivEq

  val expansion = Optional[Row, Expansion] {
    case r: ReqRow          => Some(r.exp)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case ReqRow(r, l, _, m, i) => ReqRow(r, l, nv, m, i)
    case r: ReqCodeGroupRow    => r
  })

  val multiValues = Optional[Row, MultiValues] {
    case r: ReqRow          => Some(r.mv)
    case _: ReqCodeGroupRow => None
  }(nv => {
    case ReqRow(r, l, e, _, i) => ReqRow(r, l, e, nv, i)
    case r: ReqCodeGroupRow    => r
  })

  val reqCodes = Lens[Row, Vector[ReqCode.Value]] {
    case r: ReqRow          => r.exp.reqCodes
    case r: ReqCodeGroupRow => Vector1(r.reqCode)
  }(nv => {
    case ReqRow(r, l, e, m, i)                => ReqRow(r, l, e.copyReqCodes(nv), m, i)
    case r: ReqCodeGroupRow if nv.length == 1 => r.copy(reqCode = nv.head)
    case r: ReqCodeGroupRow if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })
  val reqCodesO = reqCodes.asOptional

  val reqCodeTree = Lens[Row, Vector[ReqCodeTreeItem]] {
    case r: ReqRow          => r.exp.reqCodeTree
    case r: ReqCodeGroupRow => r.reqCodeTreeItem.toVector
  }(nv => {
    case ReqRow(r, l, e, m, i) => ReqRow(r, l, e.copyReqCodeTree(nv), m, i)
    case r: ReqCodeGroupRow => nv.length match {
      case 1 => r.copy(reqCodeTreeItem = Some(nv.head))
      case 0 => r.copy(reqCodeTreeItem = None)
      case _ => assert(false, s"Can't apply $nv to $r") ;r
    }
  })

  type OV[A]     = Optional[Row, Vector[A]]
  type OMV[K, V] = Optional[Row, Map[K, Vector[V]]]

  val implications: Direction => OV[Pubid] = Direction.memo {
    case Backwards => Row.expansion ^|-> Expansion.implicationSrc
    case Forwards  => Row.expansion ^|-> Expansion.implicationTgt
  }

  val cfImps        : OMV[CustomField.Implication.Id, Pubid]   = Row.expansion   ^|-> Expansion.cfImps
  val cfTags        : OMV[CustomField.Tag.Id, ApplicableTagId] = Row.expansion   ^|-> Expansion.cfTags
  val tags          : OV[ApplicableTagId]                      = Row.multiValues ^|-> MultiValues.tags

  private def mmLens[K, V](k: K): Lens[Map[K, Vector[V]], Vector[V]] =
    Lens[Map[K, Vector[V]], Vector[V]](_.getOrElse(k, Vector.empty))(vs => _.updated(k, vs))

  def cfImp(id: CustomField.Implication.Id): OV[Pubid]           = cfImps ^|-> mmLens(id)
  def cfTag(id: CustomField.Tag        .Id): OV[ApplicableTagId] = cfTags ^|-> mmLens(id)
}
