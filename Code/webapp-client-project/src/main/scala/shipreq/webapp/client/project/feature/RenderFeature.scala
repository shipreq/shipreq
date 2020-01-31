package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.{Reusability, Reusable, ~=>}
import scala.reflect.ClassTag
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.base.text.ProjectText.{Context => PCtx}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ViewReqCache

object RenderFeature {

  type FieldKey = editor.FieldKey
  val  FieldKey = editor.FieldKey

  def prepare[Ctx <: PCtx, Out](project     : Project,
                                viewReqCache: ViewReqCache[Ctx, Out],
                                pt          : ProjectText[Ctx, Out]): FilterDead => ForProject[Ctx, Out] =
    FilterDead.memo(ForProject(project, _, viewReqCache, pt))

  final case class ForProject[Ctx <: PCtx, Out](private[RenderFeature] project     : Project,
                                                private[RenderFeature] filterDead  : FilterDead,
                                                private[RenderFeature] viewReqCache: ViewReqCache[Ctx, Out],
                                                private[RenderFeature] pt          : ProjectText[Ctx, Out]) {

    private val reusableSelf = Reusable.explicitly(this)(reusabilityForProject[Ctx, Out])
    private val viewReq      = viewReqCache(filterDead)
    private val useCases     = project.content.reqs.useCases

    private def forData0[FK <: FieldKey](render: FK => Out) =
      ForFields[Ctx, FK, Out](reusableSelf.withValue(render))

    private def forData1[A: Reusability : ClassTag, FK <: FieldKey](a: A)(render: FK => Out) =
      ForFields[Ctx, FK, Out](reusableSelf.tuple(Reusable.implicitly(a)).withValue(render))

    def forCodeGroup(rcg: CodeGroup): ForCodeGroup[Ctx, Out] = {
      lazy val code = project.content.reqCodes.reqCode(rcg.id)
      forData1[ReqCodeGroupId, FieldKey.ForCodeGroup](rcg.id) {
        case FieldKey.CodeGroupTitle => pt.codeGroupTitle(rcg)
        case FieldKey.Code           => pt.reqCode(code)
      }
    }

    def forGenericReq(id: GenericReqId): ForGenericReq[Ctx, Out] =
      forReq(id)

    def forReq(id: ReqId): ForReq[Ctx, Out] =
      forData1[ReqId, FieldKey.ForSomeReq](id)(viewReq(id).editable)

    def forUseCase(id: UseCaseId): ForUseCase[Ctx, Out] =
      forReq(id)

    val forUseCaseSteps: ForUseCaseSteps[Ctx, Out] =
      forData0[FieldKey.UseCaseStep](fk => {
        val focus = useCases.focusStep(fk.id)
        pt.useCaseStepTextAndFlow(focus, filterDead)
      })

    val forManualIssue: ForManualIssue[Ctx, Out] =
      forData0[FieldKey.ManualIssue] { fk =>
        val issue = project.manualIssues.imap.need(fk.id)
        pt.manualIssue(issue.text)
      }
  }

  final case class ForFields[Ctx <: PCtx, -FK <: FieldKey, Out](renderFn: FK ~=> Out) {
    @inline def apply(fk: FK): Out = renderFn(fk)
  }

  type ForCodeGroup   [Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForCodeGroup , Out]
  type ForGenericReq  [Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForGenericReq, Out]
  type ForReq         [Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForSomeReq   , Out]
  type ForUseCase     [Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForUseCase   , Out]
  type ForUseCaseSteps[Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.UseCaseStep  , Out]
  type ForManualIssue [Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ManualIssue  , Out]

  sealed trait TypeHelpers[Ctx <: PCtx, Out] {
    final type ForProject                = RenderFeature.ForProject     [Ctx, Out]
    final type ForField[FK <: FieldKey]  = RenderFeature.ForFields        [Ctx, FK, Out]
    final type ForCodeGroup              = RenderFeature.ForCodeGroup   [Ctx, Out]
    final type ForGenericReq             = RenderFeature.ForGenericReq  [Ctx, Out]
    final type ForReq                    = RenderFeature.ForReq         [Ctx, Out]
    final type ForUseCase                = RenderFeature.ForUseCase     [Ctx, Out]
    final type ForUseCaseSteps           = RenderFeature.ForUseCaseSteps[Ctx, Out]
    final type ForManualIssue            = RenderFeature.ForManualIssue [Ctx, Out]
  }

  object ToVdom {
    import japgolly.scalajs.react.vdom.html_<^.VdomTag

    object AnyCtx extends TypeHelpers[PCtx, VdomTag]
    object NoCtx extends TypeHelpers[PCtx.None, VdomTag]
  }

  implicit def reusabilityForProject[Ctx <: PCtx, Out]: Reusability[ForProject[Ctx, Out]] =
    Reusability.byRef || Reusability.derive

  implicit def reusabilityForData[Ctx <: PCtx, FK <: FieldKey, Out]: Reusability[ForFields[Ctx, FK, Out]] =
    Reusability.byRef || Reusability.derive
}
