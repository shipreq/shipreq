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
import shipreq.webapp.client.project.widgets.ViewReq

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
@Lenses
case class Expansion(implications  : Direction.Values[Vector[Pubid]],
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
  val empty: Expansion =
    apply(Direction.Values both Vector.empty, Vector.empty, Vector.empty, UnivEq.emptyMap, UnivEq.emptyMap)

  implicit def equality: UnivEq[Expansion] = UnivEq.derive

  implicit val reqCodeTreeM: Monoid[Vector[ReqCodeTreeItem]] =
    scalaz.std.vector.vectorMonoid

  implicit def vectorUniqSemigroup[A](implicit e: Equal[A]): Semigroup[Vector[A]] =
    new Semigroup[Vector[A]] {
      override def append(x: Vector[A], y: => Vector[A]) =
        y.foldLeft(x)((q, a) =>
          if (x.exists(e.equal(_, a))) q else q :+ a)
    }

  implicit val monoid: Monoid[Expansion] =
    new Monoid[Expansion] {
      override def zero = empty
      override def append(a: Expansion, _b: => Expansion) = {
        val b = _b
        Expansion(
          a.implications |+| b.implications,
          a.reqCodes     |+| b.reqCodes,
          a.reqCodeTree  |+| b.reqCodeTree,
          a.cfImps       |+| b.cfImps,
          a.cfTags       |+| b.cfTags)
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

object Row {

  /**
   * @param instanceId An arbitrary number that, coupled with `req.id` serves to uniquely identify a row.
   *                   Reason is that the same GenericReq can appear in multiple rows.
   */
  final case class ForReq(req: Req, live: Live, exp: Expansion, mv: MultiValues, instanceId: Int) extends Row {
    override val id       = Row.Id.ForReq(req.id, instanceId)
    override def sourceId = Row.SourceId.ForReq(req.id)
    override def toString = s"\n$req\n$exp\n$mv\n"
  }

  final case class ForReqCodeGroup(group: ReqCodeGroup, reqCode: ReqCode.Value, reqCodeTreeItem: Option[ReqCodeTreeItem]) extends Row {
    override val id       = Row.Id.ForReqCodeGroup(reqCodeId)
    override def sourceId = Row.SourceId.ForReqCodeGroup(reqCodeId)
    override def live     = group.live
    def reqCodeId         = group.id
  }

  implicit def equalityR   : UnivEq[ForReq]               = UnivEq.derive
  implicit def equalityG   : UnivEq[ForReqCodeGroup]      = UnivEq.derive
  implicit def equality    : UnivEq[Row]                  = UnivEq.derive
  implicit val reusabilityR: Reusability[ForReq]          = Reusability.byRefOrUnivEq
  implicit val reusabilityG: Reusability[ForReqCodeGroup] = Reusability.byRefOrUnivEq
  implicit val reusability : Reusability[Row]             = Reusability.byRefOrUnivEq

  // ===================================================================================================================

  /**
   * Uniquely identifies a row, including distinguishing expansions of the same req.
   */
  sealed trait Id {
    /** A stable, unique value so that React can correctly identify each row. */
    def key: Key
  }

  object Id {

    /**
     * @param instanceId An arbitrary number that, coupled with `reqId` serves to uniquely identify a row.
     *                   Reason is that the same GenericReq can appear in multiple rows.
     */
    final case class ForReq(reqId: ReqId, instanceId: Int) extends Id {
      override def key =
        if (instanceId == 0)
          reqId.value
        else
          reqId.value.toString + (' ' + instanceId).toChar.toString
    }

    final case class ForReqCodeGroup(value: ReqCodeId) extends Id {
      override def key =
        "C" + value.value
    }

    implicit def equalityR  : UnivEq[ForReq]          = UnivEq.derive
    implicit def equalityG  : UnivEq[ForReqCodeGroup] = UnivEq.derive
    implicit def equality   : UnivEq[Id]              = UnivEq.derive
    implicit val reusability: Reusability[Id]         = Reusability.byUnivEq
  }

  // ===================================================================================================================

  /**
   * Uniquely identifies the source of a row, disregarding features such as expansions.
   */
  sealed trait SourceId

  object SourceId {
    final case class ForReq(reqId: ReqId) extends SourceId
    final case class ForReqCodeGroup(value: ReqCodeId) extends SourceId

    implicit def equalityR  : UnivEq[ForReq]          = UnivEq.derive
    implicit def equalityG  : UnivEq[ForReqCodeGroup] = UnivEq.derive
    implicit def equality   : UnivEq[SourceId]        = UnivEq.derive
    implicit val reusability: Reusability[SourceId]   = Reusability.byUnivEq

    val ToEditorRow = Intersection[SourceId, RowKey] {
      case ForReq         (id) => Some(RowKey.Req         (id))
      case ForReqCodeGroup(id) => Some(RowKey.ReqCodeGroup(id))
    } {
      case RowKey.Req         (id) => Some(ForReq         (id))
      case RowKey.ReqCodeGroup(id) => Some(ForReqCodeGroup(id))
      case RowKey.UseCaseSteps     => None
    }
  }

  // ===================================================================================================================

  implicit object SubjectRow extends Applicability.Subject[Row] {
    override def applicable(row: Row, f: ReqTypeId => Applicable) =
      row match {
        case r: ForReq          => Applicability.SubjectReq.applicable(r.req, f)
        case _: ForReqCodeGroup => NotApplicable
      }
  }

  val expansion = Optional[Row, Expansion] {
    case r: ForReq          => Some(r.exp)
    case _: ForReqCodeGroup => None
  }(nv => {
    case ForReq(r, l, _, m, i) => ForReq(r, l, nv, m, i)
    case r: ForReqCodeGroup    => r
  })

  val multiValues = Optional[Row, MultiValues] {
    case r: ForReq          => Some(r.mv)
    case _: ForReqCodeGroup => None
  }(nv => {
    case ForReq(r, l, e, _, i) => ForReq(r, l, e, nv, i)
    case r: ForReqCodeGroup    => r
  })

  val reqCodes = Lens[Row, Vector[ReqCode.Value]] {
    case r: ForReq          => r.exp.reqCodes
    case r: ForReqCodeGroup => Vector1(r.reqCode)
  }(nv => {
    case ForReq(r, l, e, m, i)                => ForReq(r, l, e.copyReqCodes(nv), m, i)
    case r: ForReqCodeGroup if nv.length == 1 => r.copy(reqCode = nv.head)
    case r: ForReqCodeGroup if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })
  val reqCodesO = reqCodes.asOptional

  val reqCodeTree = Lens[Row, Vector[ReqCodeTreeItem]] {
    case r: ForReq          => r.exp.reqCodeTree
    case r: ForReqCodeGroup => r.reqCodeTreeItem.toVector
  }(nv => {
    case ForReq(r, l, e, m, i) => ForReq(r, l, e.copyReqCodeTree(nv), m, i)
    case r: ForReqCodeGroup => nv.length match {
      case 1 => r.copy(reqCodeTreeItem = Some(nv.head))
      case 0 => r.copy(reqCodeTreeItem = None)
      case _ => assert(false, s"Can't apply $nv to $r") ;r
    }
  })

  type OV[A]     = Optional[Row, Vector[A]]
  type OMV[K, V] = Optional[Row, Map[K, Vector[V]]]

  val implications: Direction => OV[Pubid] =
    Direction.memo(Row.expansion ^|-> Expansion.implications ^|-> Direction.Values.lens(_))

  val cfImps: OMV[CustomField.Implication.Id, Pubid]   = Row.expansion   ^|-> Expansion.cfImps
  val cfTags: OMV[CustomField.Tag.Id, ApplicableTagId] = Row.expansion   ^|-> Expansion.cfTags
  val tags  : OV[ApplicableTagId]                      = Row.multiValues ^|-> MultiValues.tags
}
