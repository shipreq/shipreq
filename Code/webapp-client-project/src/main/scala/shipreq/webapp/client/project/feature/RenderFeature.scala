package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.{Reusability, Reusable, ~=>}
import japgolly.scalajs.react.vdom.VdomElement
import scala.reflect.ClassTag
import shipreq.base.util.Direction
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText.{Context => PCtx}
import shipreq.webapp.base.text.UseCaseStepFlowText.TextAndFlow
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ViewReqCache}

object RenderFeature {

  type FieldKey = editor.FieldKey
  val  FieldKey = editor.FieldKey

  def prepare[Ctx <: PCtx](project     : Project,
                           viewReqCache: ViewReqCache[Ctx],
                           pw          : ProjectWidgets[Ctx]): FilterDead => ForProject[Ctx] =
    FilterDead.memo(ForProject(project, _, viewReqCache, pw))

  final case class ForProject[Ctx <: PCtx](private[RenderFeature] project     : Project,
                                           private[RenderFeature] filterDead  : FilterDead,
                                           private[RenderFeature] viewReqCache: ViewReqCache[Ctx],
                                           private[RenderFeature] pw          : ProjectWidgets[Ctx]) {

    private val reusableSelf = Reusable.explicitly(this)(reusabilityForProject[Ctx])
    private val viewReq      = viewReqCache(filterDead)
    private val useCases     = project.content.reqs.useCases

    // TODO Use Reusable#withValue once available

    private def forData0[FK <: FieldKey](render: FK => VdomElement) =
      ForData[Ctx, FK](reusableSelf.map(_ => render))

    private def forData1[A: Reusability : ClassTag, FK <: FieldKey](a: A)(render: FK => VdomElement) =
      ForData[Ctx, FK](reusableSelf.tuple(Reusable.implicitly(a)).map(_ => render))

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
        val tf    = TextAndFlow(focus.titleA, Direction.Values(focus.flow(_, filterDead)))
        pw.useCaseStepTextAndFlow(tf, focus.live)
      })
  }

  final case class ForData[Ctx <: PCtx, -FK <: FieldKey](renderFn: FK ~=> VdomElement) {
    @inline def apply(fk: FK): VdomElement = renderFn(fk)
  }

  type ForCodeGroup   [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForCodeGroup ]
  type ForGenericReq  [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForGenericReq]
  type ForReq         [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForSomeReq   ]
  type ForUseCase     [Ctx <: PCtx] = ForData[Ctx, FieldKey.ForUseCase   ]
  type ForUseCaseSteps[Ctx <: PCtx] = ForData[Ctx, FieldKey.UseCaseStep  ]

  sealed trait TypeHelpers[Ctx <: PCtx] {
    final type ForProject      = RenderFeature.ForProject     [Ctx]
    final type ForCodeGroup    = RenderFeature.ForCodeGroup   [Ctx]
    final type ForGenericReq   = RenderFeature.ForGenericReq  [Ctx]
    final type ForReq          = RenderFeature.ForReq         [Ctx]
    final type ForUseCase      = RenderFeature.ForUseCase     [Ctx]
    final type ForUseCaseSteps = RenderFeature.ForUseCaseSteps[Ctx]
  }

  object NoCtx extends TypeHelpers[PCtx.None]

  implicit def reusabilityForProject[Ctx <: PCtx]: Reusability[ForProject[Ctx]] =
    Reusability.byRef || Reusability.derive

  implicit def reusabilityForData[Ctx <: PCtx, FK <: FieldKey]: Reusability[ForData[Ctx, FK]] =
    Reusability.byRef || Reusability.derive
}
