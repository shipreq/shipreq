package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.feature.EditorFeature
import ProjectWidgets.emptySpan
import ViewReq._

/**
  * Easy means to view/render a requirement.
  */
final case class ViewReq(data: Data, pw: ProjectWidgets, fmtReqTypeShort: Boolean = true) {

  def reqType: VdomElement =
    (if (fmtReqTypeShort) pw.reqTypeShort else pw.reqTypeFull)(data.req.reqTypeId)

  def codes: VdomElement =
    <.div(pw.reqCodes(data.codes))

  def imps(dir: Direction): VdomElement =
    pw.implicationList(data.generalImps(dir))

  def imps(id: CustomField.Implication.Id): VdomElement =
    pw.implicationList(data.customImps(id))

  def imps(scope: ImplicationScope): VdomElement =
    scope.fold(imps(_), imps(_))

  /** None means N/A */
  def deletionReason: Option[VdomTag] =
    ProjectWidgets.DeletionReason.forReq(data.req)(data.reqTypes, pw)

  def pastPubids: VdomElement =
    pw pastPubids data.pastPubids

  def tags: VdomElement =
    pw.tagList(data.generalTags)

  def tags(id: CustomField.Tag.Id): VdomElement =
    pw.tagList(data.customTags(id))

  def tags(id: Option[CustomField.Tag.Id]): VdomElement =
    id.fold(tags)(tags(_))

  def text(id: CustomField.Text.Id): VdomElement =
    pw.customTextField(id)(data.req) getOrElse[VdomTag] emptySpan

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

  case class Data(req        : Req,
                  codes      : Traversable[ReqCode.Value],
                  generalTags: Vector[ApplicableTagId],
                  customTags : CustomField.Tag.Id => Vector[ApplicableTagId],
                  generalImps: Direction => Vector[Pubid],
                  customImps : CustomField.Implication.Id => Vector[Pubid],
                  pastPubids : Vector[Pubid],
                  reqTypes   : ReqTypes) {

    def apply(pw: ProjectWidgets): ViewReq =
      ViewReq(this, pw)
  }

  object Data {

    def fromProject(id: ReqId, project: Project, filterDead: FilterDead): Data = {
      val req             = project.reqs.need(id)
      val pubidSortKeyFn  = DataLogic.pubidSortKeyFn(project.config)
      val impFilter       = DataLogic.impValueFilter(project.config, filterDead)
      val customImpLookup = DataLogic.customFieldImps(project, impFilter)
      val tagDist         = DataLogic.tagFieldDist(project.config, filterDead, _ => true)
      val tagLookup       = DataLogic.tagLookup(project, filterDead)
      val tagOrderByName  = DataLogic.tagOrderByName(project.config.tags)
      val tagOrderByPos   = DataLogic.tagOrderByPos(project.config.tags)
      val generalTagSet   = DataLogic.generalTags(tagDist, tagLookup)(req.id)
      val generalTags     = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).to[Vector]

      val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
        Memo { fid =>
          def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(req.id)
          MutableArray(tagSet).sortBy(tagOrderByPos.apply).to[Vector]
        }

      def sortPubids(pubids: TraversableOnce[Pubid]): Vector[Pubid] =
        MutableArray(pubids)
          .sortBySchwartzian(pubidSortKeyFn)
          .to[Vector]

      val codes: List[ReqCode.Value] =
        MutableArray(project.reqCodes.activeReqCodesByReqId(id))
          .sortBySchwartzian(PlainText.reqCode)
          .to[List]

      val generalImps: Direction => Vector[Pubid] =
        Direction.memo(dir =>
          sortPubids(
            project.implications(dir)(id)
              .iterator
              .map(project.reqs.need)
              .filter(impFilter)
              .map(_.pubid)))

      val customImps: CustomField.Implication.Id => Vector[Pubid] =
        fid => sortPubids(customImpLookup(fid)(id))

      val pastPubids: Vector[Pubid] =
        MutableArray(req.pastPubids(project.reqs.pubids))
          .sortBySchwartzian(pubidSortKeyFn)
          .to[Vector]

      Data(
        req,
        codes,
        generalTags,
        customTags,
        generalImps,
        customImps,
        pastPubids,
        project.config.reqTypes)
    }

  }
}
