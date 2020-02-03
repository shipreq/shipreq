package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.vdom.html_<^.VdomTag
import scala.collection.immutable.SortedSet
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.client.project.feature.{EditorFeature, RenderFeature}
import ViewReq._

/**
  * Easy means to view/render a requirement.
  */
final case class ViewReq[A](data           : Data,
                            pt             : ProjectText[ProjectText.Context, A],
                            fmtReqTypeShort: Boolean) {

  def reqType: A = {
    val id = data.req.reqTypeId
    if (fmtReqTypeShort)
      pt.reqTypeShort(id)
    else
      pt.reqTypeFull(id)
  }

  def codes: A =
    pt.reqCodes(data.codes)

  def imps(dir: Direction): A = {
    val imps      = data.generalImps(dir)
    val mandatory = Mandatory.when(data.impsAreMandatory && dir.is(Backwards))
    pt.implicationList(imps, data.live, mandatory)
  }

  def imps(id: CustomField.Implication.Id): A = {
    val imps      = data.customImps(id)
    val mandatory = Mandatory.when(data.mandatoryFields.contains(id))
    pt.implicationList(imps, data.live, mandatory)
  }

  def imps(scope: ImplicationScope): A =
    scope.fold(imps(_), imps(_))

  def deletionReason: IfApplicable[A] =
    pt.deleteReasonForReq(data.req)

  def pastPubids: A =
    pt pastPubids data.pastPubids

  private val tagValidity: ApplicableTagId => Validity =
    Invalid when data.conflictingTags.contains(_)

  def tags: A =
    pt.tagList(data.generalTags, data.live, Mandatory.Not, tagValidity)

  def tags(id: CustomField.Tag.Id): A = {
    val tags      = data.customTags(id)
    val mandatory = Mandatory.when(data.mandatoryFields.contains(id))
    pt.tagList(tags, data.live, mandatory, tagValidity)
  }

  def tags(id: Option[CustomField.Tag.Id]): A =
    id.fold(tags)(tags(_))

  def text(id: CustomField.Text.Id): A =
    pt.customTextField(id, data.req, data.live, Mandatory.when(data.mandatoryFields.contains(id)))

  def title: A =
    pt.reqTitle(data.req)

  val customField: CustomFieldId => A = {
    case id: CustomField.Implication.Id => imps(id)
    case id: CustomField.Tag        .Id => tags(id)
    case id: CustomField.Text       .Id => text(id)
  }

  val render: RenderFeature.FieldKey.ForSomeReq => A = {
    case RenderFeature.FieldKey.CustomTextField(field) => text(field)
    case RenderFeature.FieldKey.Tags           (field) => tags(field)
    case RenderFeature.FieldKey.Implications   (scope) => imps(scope)
    case RenderFeature.FieldKey.Codes                  => codes
    case RenderFeature.FieldKey.Title                  => title
    case RenderFeature.FieldKey.ReqType                => reqType
  }

  val editable: EditorFeature.FieldKey.ForSomeReq => A =
    k => render(k.forRender)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ViewReq {

  type ToVdom = ViewReq[VdomTag]

  final case class Data(req             : Req,
                        live            : Live,
                        codes           : Traversable[ReqCode.Value],
                        generalTags     : Vector[ApplicableTagId],
                        customTags      : CustomField.Tag.Id => Vector[ApplicableTagId],
                        conflictingTags : Set[ApplicableTagId],
                        generalImps     : Direction => Vector[Pubid],
                        customImps      : CustomField.Implication.Id => Vector[Pubid],
                        pastPubids      : SortedSet[ExternalPubid],
                        impsAreMandatory: Boolean,
                        mandatoryFields : CustomField.Lists) {

    def apply[A](pt: ProjectText[ProjectText.Context, A]): ViewReq[A] =
      ViewReq(this, pt, true)
  }

  object Data {

    def fromProject(id: ReqId, project: Project, filterDead: FilterDead): Data = {
      val req = project.content.reqs.need(id)
      fromProject(req, project, filterDead)
    }

    def fromProject(req: Req, project: Project, filterDead: FilterDead): Data = {
      import req.id

      val cfg             = project.config
      val pubidSortKeyFn  = project.dataLogic.pubidSortKeyFn
      val customImpLookup = project.dataLogic.customFieldImps(filterDead)
      val tagDist         = project.dataLogic.tagFieldDist(filterDead)
      val tagLookup       = project.dataLogic.tagLookup(filterDead)
      val tagOrderByName  = project.dataLogic.tagOrderByName
      val tagOrderByPos   = project.dataLogic.tagOrderByPos
      val impFilter       = cfg.reqFilter(filterDead)
      val generalTagSet   = DataLogic.generalTags(tagDist, tagLookup)(id)
      val generalTags     = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).iterator.to[Vector]

      val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
        Memo { fid =>
          def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(id)
          MutableArray(tagSet).sortBy(tagOrderByPos.apply).iterator.to[Vector]
        }

      def sortPubids(pubids: TraversableOnce[Pubid]): Vector[Pubid] =
        MutableArray(pubids)
          .sortBySchwartzian(pubidSortKeyFn)
          .iterator
          .to[Vector]

      val codes: List[ReqCode.Value] =
        MutableArray(project.content.reqCodes.activeReqCodesByReqId(id))
          .sortBySchwartzian(PlainText.reqCode)
          .iterator
          .to[List]

      val generalImps: Direction => Vector[Pubid] =
        Direction.memo(dir =>
          sortPubids(
            project.content.implications(dir)(id)
              .iterator
              .map(project.content.reqs.need)
              .filter(impFilter)
              .map(_.pubid)))

      val pastPubids: SortedSet[ExternalPubid] = {
        val b = SortedSet.newBuilder[ExternalPubid]
        b ++= req.pubid.pastExternals(project)
        for (pubid <- req.pastPubids(project.content.reqs.pubids)) {
          b += pubid.external(project)
          b ++= pubid.pastExternals(project)
        }
        b.result()
      }

      Data(
        req              = req,
        live             = req.live(cfg.reqTypes),
        codes            = codes,
        generalTags      = generalTags,
        customTags       = customTags,
        conflictingTags  = project.conflictingTagsPerReq(id),
        generalImps      = generalImps,
        customImps       = fid => sortPubids(customImpLookup(fid)(id)),
        pastPubids       = pastPubids,
        impsAreMandatory = cfg.reqTypes.idsRequiringImplication.contains(req.reqTypeId),
        mandatoryFields  = cfg.mandatoryLiveCustomFields)
    }

  }
}
