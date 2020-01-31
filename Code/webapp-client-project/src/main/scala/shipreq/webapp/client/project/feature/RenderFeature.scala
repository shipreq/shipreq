package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.{Reusability, Reusable, ~=>}
import japgolly.scalajs.react.vdom.VdomElement
import scala.reflect.ClassTag
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText.{Context => PCtx}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ViewReqCache}

object RenderFeature {

  type FieldKey = editor.FieldKey
  val  FieldKey = editor.FieldKey

  def prepare[Ctx <: PCtx](project     : Project,
                           viewReqCache: ViewReqCache.ToVdom[Ctx],
                           pw          : ProjectWidgets[Ctx]): FilterDead => ForProject[Ctx] =
    FilterDead.memo(ForProject(project, _, viewReqCache, pw))

  final case class ForProject[Ctx <: PCtx](private[RenderFeature] project     : Project,
                                           private[RenderFeature] filterDead  : FilterDead,
                                           private[RenderFeature] viewReqCache: ViewReqCache.ToVdom[Ctx],
                                           private[RenderFeature] pw          : ProjectWidgets[Ctx]) {

    private val reusableSelf = Reusable.explicitly(this)(reusabilityForProject[Ctx])
    private val viewReq      = viewReqCache(filterDead)
    private val useCases     = project.content.reqs.useCases

    private def forData0[FK <: FieldKey](render: FK => VdomElement) =
      ForData[Ctx, FK](reusableSelf.withValue(render))

    private def forData1[A: Reusability : ClassTag, FK <: FieldKey](a: A)(render: FK => VdomElement) =
      ForData[Ctx, FK](reusableSelf.tuple(Reusable.implicitly(a)).withValue(render))

    def forCodeGroup(rcg: CodeGroup): ForCodeGroup[Ctx] = {
      lazy val code = project.content.reqCodes.reqCode(rcg.id)
      forData1[ReqCodeGroupId, FieldKey.ForCodeGroup](rcg.id) {
        case FieldKey.CodeGroupTitle => pw.codeGroupTitle(rcg)
        case FieldKey.Code           => pw.reqCode(code)
      }
    }

    def forGenericReq(id: GenericReqId): ForGenericReq[Ctx] =
      forReq(id)

    def forReq(id: ReqId): ForReq[Ctx] =
      forData1[ReqId, FieldKey.ForSomeReq](id)(viewReq(id).editable)

    def forUseCase(id: UseCaseId): ForUseCase[Ctx] =
      forReq(id)

    val forUseCaseSteps: ForUseCaseSteps[Ctx] =
      forData0[FieldKey.UseCaseStep](fk => {
        val focus = useCases.focusStep(fk.id)
        pw.useCaseStepTextAndFlow(focus, filterDead)
      })

    val forManualIssue: ForManualIssue[Ctx] =
      forData0[FieldKey.ManualIssue] { fk =>
        val issue = project.manualIssues.imap.need(fk.id)
        pw.manualIssue(issue.text)
      }
  }

  final case class ForData[Ctx <: PCtx, -FK <: FieldKey](renderFn: FK ~=> VdomElement) {
    @inline def apply(fk: FK): VdomElement = renderFn(fk)
  }

  type ForCodeGroup   [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForCodeGroup ]
  type ForGenericReq  [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForGenericReq]
  type ForReq         [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForSomeReq   ]
  type ForUseCase     [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForUseCase   ]
  type ForUseCaseSteps[Ctx <: PCtx] = ForData[Ctx, FieldKey.UseCaseStep  ]
  type ForManualIssue [Ctx <: PCtx] = ForData[Ctx, FieldKey.ManualIssue  ]

  sealed trait TypeHelpers[Ctx <: PCtx] {
    final type ForProject                = RenderFeature.ForProject     [Ctx]
    final type ForField[FK <: FieldKey]  = RenderFeature.ForData        [Ctx, FK]
    final type ForCodeGroup              = RenderFeature.ForCodeGroup   [Ctx]
    final type ForGenericReq             = RenderFeature.ForGenericReq  [Ctx]
    final type ForReq                    = RenderFeature.ForReq         [Ctx]
    final type ForUseCase                = RenderFeature.ForUseCase     [Ctx]
    final type ForUseCaseSteps           = RenderFeature.ForUseCaseSteps[Ctx]
    final type ForManualIssue            = RenderFeature.ForManualIssue [Ctx]
  }

  object AnyCtx extends TypeHelpers[PCtx]
  object NoCtx extends TypeHelpers[PCtx.None]

  implicit def reusabilityForProject[Ctx <: PCtx]: Reusability[ForProject[Ctx]] =
    Reusability.byRef || Reusability.derive

  implicit def reusabilityForData[Ctx <: PCtx, FK <: FieldKey]: Reusability[ForData[Ctx, FK]] =
    Reusability.byRef || Reusability.derive
}
