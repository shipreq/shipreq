package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.vdom.PackageBase._
import scala.collection.immutable.SortedSet
import shipreq.base.util._
import shipreq.webapp.base.data.FieldReqTypeRules.Resolution
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.client.project.feature.{EditorFeature, RenderFeature}
import shipreq.webapp.client.project.widgets.ViewReq._

/**
  * Easy means to view/render a requirement.
  */
final class ViewReq[A](data           : Data,
                       pt             : ProjectText[ProjectText.Context, A],
                       viewTags       : ViewTags.ForReq[A],
                       fmtReqTypeShort: Boolean) {

  def withFullReqTypeFmt: ViewReq[A] =
    new ViewReq(data, pt, viewTags, false)

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

  def imps(id: CustomField.Implication.Id): IfApplicable[A] = {
    val imps = data.customImps(id)
    data.fieldRules.imp(id) match {
      case Resolution.Optional      => \/-(pt.implicationList(imps, data.live, Optional))
      case Resolution.Mandatory     => \/-(pt.implicationList(imps, data.live, Mandatory))
      case Resolution.NotApplicable => NotApplicable.left
      case Resolution.DefaultTo(x)  => x.impossible
    }
  }

  def imps(scope: ImplicationScope): IfApplicable[A] =
    scope match {
      case \/-(dir) => \/-(imps(dir))
      case -\/(id)  => imps(id)
    }

  def deletionReason: IfApplicable[A] =
    pt.deleteReasonForReq(data.req)

  def pastPubids: A =
    pt pastPubids data.pastPubids

  def otherTags: A =
    viewTags.vector(data.otherTags, viewTags.other)

  def allTags: A =
    viewTags.vector(data.allTags, viewTags.all)

  def fieldTags(fid: CustomField.Tag.Id): IfApplicable[A] = {
    val tags = data.customTags(fid)
    if (data.fieldRules.tag(fid).isNA)
      NotApplicable.left
    else if (tags.isEmpty && data.live.is(Live) && data.fieldRules.tag(fid).isMandatory)
      \/-(pt.whenBlankButMandatory)
    else
      \/-(viewTags.vector(tags, viewTags.inField(fid)))
  }

  def text(id: CustomField.Text.Id): IfApplicable[A] =
    data.fieldRules.text(id) match {
      case Resolution.Optional      => \/-(pt.customTextField(id, data.req, data.live, Optional))
      case Resolution.Mandatory     => \/-(pt.customTextField(id, data.req, data.live, Mandatory))
      case Resolution.NotApplicable => NotApplicable.left
      case Resolution.DefaultTo(x)  => x.impossible
    }

  def title: A =
    pt.reqTitle(data.req)

  val customField: CustomFieldId => IfApplicable[A] = {
    case id: CustomField.Implication.Id => imps(id)
    case id: CustomField.Tag        .Id => fieldTags(id)
    case id: CustomField.Text       .Id => text(id)
  }

  val render: RenderFeature.FieldKey.ForSomeReq => IfApplicable[A] = {
    case RenderFeature.FieldKey.CustomTextField(field) => text(field)
    case RenderFeature.FieldKey.CustomFieldTags(field) => fieldTags(field)
    case RenderFeature.FieldKey.Implications   (scope) => imps(scope)
    case RenderFeature.FieldKey.Codes                  => \/-(codes)
    case RenderFeature.FieldKey.Title                  => \/-(title)
    case RenderFeature.FieldKey.ReqType                => \/-(reqType)
    case RenderFeature.FieldKey.OtherTags              => \/-(otherTags)
    case RenderFeature.FieldKey.AllTags                => \/-(allTags)
  }

  val editable: EditorFeature.FieldKey.ForSomeReq => IfApplicable[A] =
    k => render(k.forRender)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ViewReq {

  type ToVdom = ViewReq[VdomTag]

  final case class Data(req             : Req,
                        filterDead      : FilterDead,
                        live            : Live,
                        codes           : Iterable[ReqCode.Value],
                        otherTags       : Vector[ApplicableTagId],
                        allTags         : Vector[ApplicableTagId],
                        customTags      : CustomField.Tag.Id => Vector[ApplicableTagId],
                        invalidTags     : Set[ApplicableTagId],
                        generalImps     : Direction => Vector[Pubid],
                        customImps      : CustomField.Implication.Id => Vector[Pubid],
                        pastPubids      : SortedSet[ExternalPubid],
                        impsAreMandatory: Boolean,
                        fieldRules      : FieldSetRules,
                       ) {

    def apply(pw: ProjectWidgets.AnyCtx): ToVdom =
      apply(pw, pw.viewTags.forReq(filterDead)(req.id))

    def apply[A](pt: ProjectText[ProjectText.Context, A], viewTags: ViewTags.ForReq[A]): ViewReq[A] =
      new ViewReq(this, pt, viewTags, true)
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
      val impFilter       = cfg.reqFilter(filterDead)
      val tags            = project.virtualTags(id, filterDead)

      def sortPubids(pubids: IterableOnce[Pubid]): Vector[Pubid] =
        MutableArray(pubids)
          .sortBySchwartzian(pubidSortKeyFn)
          .iterator()
          .to(Vector)

      val generalImps: Direction => Vector[Pubid] =
        Direction.memo(dir =>
          sortPubids(
            project.content.implications(dir)(id)
              .iterator
              .map(project.content.reqs.need)
              .filter(impFilter)
              .map(_.pubid)))

      val codes: List[ReqCode.Value] =
        MutableArray(project.content.reqCodes.activeReqCodesByReqId(id))
          .sortBySchwartzian(PlainText.reqCode)
          .iterator()
          .to(List)

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
        filterDead       = filterDead,
        live             = req.live(cfg.reqTypes),
        codes            = codes,
        otherTags        = tags.otherOrdered,
        allTags          = tags.allOrdered,
        customTags       = tags.fieldOrdered,
        invalidTags      = project.invalidTagsPerReq(id),
        generalImps      = generalImps,
        customImps       = fid => sortPubids(customImpLookup(fid).getPubids(id)),
        pastPubids       = pastPubids,
        impsAreMandatory = cfg.reqTypes.idsRequiringImplication.contains(req.reqTypeId),
        fieldRules       = cfg.fieldRules(filterDead)(req.reqTypeId),
      )
    }
  }
}
