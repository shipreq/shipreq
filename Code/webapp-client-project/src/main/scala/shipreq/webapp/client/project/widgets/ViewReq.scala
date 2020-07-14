package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.vdom.html_<^.VdomTag
import scala.collection.immutable.SortedSet
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data.FieldReqTypeRules.Resolution
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.client.project.feature.{EditorFeature, RenderFeature}
import shipreq.webapp.client.project.widgets.ViewReq._

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

  private val tagValidity: ApplicableTagId => Validity =
    Invalid when data.invalidTags.contains(_)

  def otherTags: A =
    pt.tagList(data.otherTags, data.live, Optional, tagValidity)

  def allTags: A =
    pt.tagList(data.allTags, data.live, Optional, tagValidity)

  def fieldTags(id: CustomField.Tag.Id): IfApplicable[A] = {
    val tags = data.customTags(id)
    data.fieldRules.tag(id) match {
      case Resolution.Optional      => \/-(pt.tagList(tags, data.live, Optional, tagValidity))
      case Resolution.Mandatory     => \/-(pt.tagList(tags, data.live, Mandatory, tagValidity))
      case Resolution.NotApplicable => NotApplicable.left
      case Resolution.DefaultTo(d)  =>
        val t = if (tags.isEmpty) Vector1(d) else tags
        \/-(pt.tagList(t, data.live, Optional, tagValidity))
    }
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
      val reqTags         = tagLookup(id)
      val tagOrderByName  = project.dataLogic.tagOrderByName
      val tagOrderByPos   = project.dataLogic.tagOrderByPos
      val impFilter       = cfg.reqFilter(filterDead)
      val otherTagSet     = DataLogic.otherTags(tagDist, tagLookup)(id)
      val otherTags       = MutableArray(otherTagSet).sortBy(tagOrderByName.apply).iterator().to(Vector)
      val allTags         = MutableArray(reqTags.all).sortBy(tagOrderByName.apply).iterator().to(Vector)

      val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
        Memo { fid =>
          def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(id)
          MutableArray(tagSet).sortBy(tagOrderByPos.apply).iterator().to(Vector)
        }

      def sortPubids(pubids: IterableOnce[Pubid]): Vector[Pubid] =
        MutableArray(pubids)
          .sortBySchwartzian(pubidSortKeyFn)
          .iterator()
          .to(Vector)

      val codes: List[ReqCode.Value] =
        MutableArray(project.content.reqCodes.activeReqCodesByReqId(id))
          .sortBySchwartzian(PlainText.reqCode)
          .iterator()
          .to(List)

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
        otherTags        = otherTags,
        allTags          = allTags,
        customTags       = customTags,
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
