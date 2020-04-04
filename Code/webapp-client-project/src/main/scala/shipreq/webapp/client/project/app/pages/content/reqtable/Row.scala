package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.Key
import japgolly.scalajs.react.Reusability
import monocle.{Lens, Optional}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.client.project.feature.EditorFeature.RowKey

sealed trait Row {
  val id        : Row.Id
  def sourceId  : Row.SourceId
  def live      : Live
  def fieldRules: FieldSetRules
}

object Row {

  /**
   * @param instanceId An arbitrary number that, coupled with `req.id` serves to uniquely identify a row.
   *                   Reason is that the same GenericReq can appear in multiple rows.
   */
  final case class ForReq(req        : Req,
                          live       : Live,
                          invalidTags: Set[ApplicableTagId],
                          exp        : Expansions,
                          fieldRules : FieldSetRules,
                          instanceId : Int) extends Row {
    override val id       = Row.Id.ForReq(req.id, instanceId)
    override def sourceId = Row.SourceId.ForReq(req.id)
    override def toString = s"$id\n$req\n$exp\n"
  }

  final case class ForCodeGroup(group: CodeGroup,
                                reqCode: ReqCode.Value,
                                reqCodeTreeItem: Option[ReqCodeTreeItem]) extends Row {
    override val id         = Row.Id.ForCodeGroup(reqCodeId)
    override def sourceId   = Row.SourceId.ForCodeGroup(reqCodeId)
    override def live       = group.live
    override def fieldRules = FieldSetRules.optional
    def reqCodeId           = group.id
  }

  implicit def equalityR   : UnivEq[ForReq]            = UnivEq.derive
  implicit def equalityG   : UnivEq[ForCodeGroup]      = UnivEq.derive
  implicit def equality    : UnivEq[Row]               = UnivEq.derive
  implicit val reusabilityR: Reusability[ForReq]       = Reusability.byRefOrUnivEq
  implicit val reusabilityG: Reusability[ForCodeGroup] = Reusability.byRefOrUnivEq
  implicit val reusability : Reusability[Row]          = Reusability.byRefOrUnivEq

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

    final case class ForCodeGroup(value: ReqCodeGroupId) extends Id {
      override def key =
        "C" + value.value
    }

    implicit def equalityR  : UnivEq[ForReq]       = UnivEq.derive
    implicit def equalityG  : UnivEq[ForCodeGroup] = UnivEq.derive
    implicit def equality   : UnivEq[Id]           = UnivEq.derive
    implicit val reusability: Reusability[Id]      = Reusability.byUnivEq
  }

  // ===================================================================================================================

  /**
   * Uniquely identifies the source of a row, disregarding features such as expansions.
   */
  sealed trait SourceId

  object SourceId {
    final case class ForReq(reqId: ReqId) extends SourceId
    final case class ForCodeGroup(value: ReqCodeGroupId) extends SourceId

    implicit def equalityR  : UnivEq[ForReq]        = UnivEq.derive
    implicit def equalityG  : UnivEq[ForCodeGroup]  = UnivEq.derive
    implicit def equality   : UnivEq[SourceId]      = UnivEq.derive
    implicit val reusability: Reusability[SourceId] = Reusability.byUnivEq

    val ToEditorRow = Intersection[SourceId, RowKey] {
      case ForReq      (id) => Some(id.foldReqId(RowKey.GenericReq, RowKey.UseCase))
      case ForCodeGroup(id) => Some(RowKey.CodeGroup(id))
    } {
      case RowKey.GenericReq  (id) => Some(ForReq      (id))
      case RowKey.UseCase     (id) => Some(ForReq      (id))
      case RowKey.CodeGroup   (id) => Some(ForCodeGroup(id))
      case RowKey.ManualIssues
         | RowKey.UseCaseSteps     => None
    }
  }

  // ===================================================================================================================

  def applicability(a: ProjectApplicability.Default): ProjectApplicability[Column, Row] =
    Column.applicabilityForReq(a).mapDataFn[Column, Row]((col, forReq) => {
      case r: Row.ForReq       => forReq(r.req.reqTypeId)
      case _: Row.ForCodeGroup => Column.applicabilityForCodeGroup((), col)
    }).memoiseByField

  val expansion = Optional[Row, Expansions] {
    case r: ForReq       => Some(r.exp)
    case _: ForCodeGroup => None
  }(nv => {
    case ForReq(r, l, c, _, f, i) => ForReq(r, l, c, nv, f, i)
    case r: ForCodeGroup          => r
  })

  val reqCodes = Lens[Row, Vector[ReqCode.Value]] {
    case r: ForReq       => r.exp.reqCodes
    case r: ForCodeGroup => Vector1(r.reqCode)
  }(nv => {
    case ForReq(r, l, c, e, f, i)          => ForReq(r, l, c, e.copyReqCodes(nv), f, i)
    case r: ForCodeGroup if nv.length == 1 => r.copy(reqCode = nv.head)
    case r: ForCodeGroup if nv.length != 1 => assert(false, s"Can't apply $nv to $r") ;r
  })
  val reqCodesO = reqCodes.asOptional

  val reqCodeTree = Lens[Row, Vector[ReqCodeTreeItem]] {
    case r: ForReq       => r.exp.reqCodeTree
    case r: ForCodeGroup => r.reqCodeTreeItem.toVector
  }(nv => {
    case ForReq(r, l, c, e, f, i) => ForReq(r, l, c, e.copyReqCodeTree(nv), f, i)
    case r: ForCodeGroup => nv.length match {
      case 1 => r.copy(reqCodeTreeItem = Some(nv.head))
      case 0 => r.copy(reqCodeTreeItem = None)
      case _ => assert(false, s"Can't apply $nv to $r") ;r
    }
  })

  type OV[A]     = Optional[Row, Vector[A]]
  type OMV[K, V] = Optional[Row, Map[K, Vector[V]]]

  val implications: Direction => OV[Pubid] =
    Direction.memo(Row.expansion ^|-> Expansions.implications ^|-> Direction.Values.lens(_))

  val cfImps   : OMV[CustomField.Implication.Id, Pubid]   = Row.expansion ^|-> Expansions.cfImps
  val cfTags   : OMV[CustomField.Tag.Id, ApplicableTagId] = Row.expansion ^|-> Expansions.cfTags
  val otherTags: OV[ApplicableTagId]                      = Row.expansion ^|-> Expansions.otherTags
  val allTags  : OV[ApplicableTagId]                      = Row.expansion ^|-> Expansions.allTags

  private def mmLens[K, V](k: K): Lens[Map[K, Vector[V]], Vector[V]] =
    Lens[Map[K, Vector[V]], Vector[V]](_.getOrElse(k, Vector.empty))(vs => _.updated(k, vs))

  def cfImp(id: CustomField.Implication.Id): OV[Pubid]           = cfImps ^|-> mmLens(id)
  def cfTag(id: CustomField.Tag        .Id): OV[ApplicableTagId] = cfTags ^|-> mmLens(id)
}
