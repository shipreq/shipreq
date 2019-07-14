package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.SortedSet
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.feature.EditorFeature
import ViewReq._

/**
  * Easy means to view/render a requirement.
  */
final case class ViewReq(data           : Data,
                         pw             : ProjectWidgets.AnyCtx,
                         fmtReqTypeShort: Boolean) {

  def reqType: VdomElement =
    (if (fmtReqTypeShort) pw.reqTypeShort else pw.reqTypeFull)(data.req.reqTypeId)

  def codes: VdomElement =
    <.div(pw.reqCodes(data.codes))

  def imps(dir: Direction): VdomElement = {
    val imps      = data.generalImps(dir)
    val mandatory = Mandatory.when(data.impsAreMandatory && dir.is(Backwards))
    pw.implicationList(imps, data.live, mandatory)
  }

  def imps(id: CustomField.Implication.Id): VdomElement = {
    val imps      = data.customImps(id)
    val mandatory = Mandatory.when(data.mandatoryFields.contains(id))
    pw.implicationList(imps, data.live, mandatory)
  }

  def imps(scope: ImplicationScope): VdomElement =
    scope.fold(imps(_), imps(_))

  def deletionReason: IfApplicable[VdomTag] =
    pw.deleteReasonForReq(data.req)

  def pastPubids: VdomElement =
    pw pastPubids data.pastPubids

  private val tagValidity: ApplicableTagId => Validity =
    Invalid when data.conflictingTags.contains(_)

  def tags: VdomElement =
    pw.tagList(data.generalTags, data.live, Mandatory.Not, tagValidity)

  def tags(id: CustomField.Tag.Id): VdomElement = {
    val tags      = data.customTags(id)
    val mandatory = Mandatory.when(data.mandatoryFields.contains(id))
    pw.tagList(tags, data.live, mandatory, tagValidity)
  }

  def tags(id: Option[CustomField.Tag.Id]): VdomElement =
    id.fold(tags)(tags(_))

  def text(id: CustomField.Text.Id): VdomElement =
    pw.customTextField(id)(data.req).getOrElse[VdomTag] {
      if (data.live.is(Live) && data.mandatoryFields.contains(id))
        ProjectWidgets.blankButMandatory
      else
        ProjectWidgets.emptySpan
    }

  def title: VdomElement =
    pw.reqTitle(data.req)

  val customField: CustomFieldId => VdomElement = {
    case id: CustomField.Implication.Id => imps(id)
    case id: CustomField.Tag        .Id => tags(id)
    case id: CustomField.Text       .Id => text(id)
  }

  val editable: EditorFeature.FieldKey.ForSomeReq => VdomElement = {
    case EditorFeature.FieldKey.CustomTextField(field) => text(field)
    case EditorFeature.FieldKey.Tags           (field) => tags(field)
    case EditorFeature.FieldKey.Implications   (scope) => imps(scope)
    case EditorFeature.FieldKey.Codes                  => codes
    case EditorFeature.FieldKey.GenericReqTitle
       | EditorFeature.FieldKey.UseCaseTitle           => title
    case EditorFeature.FieldKey.ReqType                => reqType
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ViewReq {

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

    def apply(pw: ProjectWidgets.AnyCtx): ViewReq =
      ViewReq(this, pw, true)
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
      val generalTags     = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).to[Vector]

      val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
        Memo { fid =>
          def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(id)
          MutableArray(tagSet).sortBy(tagOrderByPos.apply).to[Vector]
        }

      def sortPubids(pubids: TraversableOnce[Pubid]): Vector[Pubid] =
        MutableArray(pubids)
          .sortBySchwartzian(pubidSortKeyFn)
          .to[Vector]

      val codes: List[ReqCode.Value] =
        MutableArray(project.content.reqCodes.activeReqCodesByReqId(id))
          .sortBySchwartzian(PlainText.reqCode)
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
