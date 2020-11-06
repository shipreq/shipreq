package shipreq.webapp.client.project.feature.render

import japgolly.scalajs.react.{Reusability, Reusable, ~=>}
import scala.reflect.ClassTag
import shipreq.base.util.{IfApplicable, NotApplicable}
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.ViewReqCache
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.ProjectText
import shipreq.webapp.member.project.text.ProjectText.{Context => PCtx}

object Feature {

  final case class ForProject[+Ctx <: PCtx, V, Out](private[Feature] val project     : Project,
                                                    private[Feature] val filterDead  : FilterDead,
                                                    private[Feature] val viewReqCache: ViewReqCache[Ctx, V],
                                                    private[Feature] val pt          : ProjectText[Ctx, V],
                                                    private[Feature] val render      : V ~=> Out,
                                                    private[Feature] val renderIfApp : IfApplicable[V] ~=> Out,
                                                   ) {

    private val reusableSelf = Reusable.explicitly(this)(reusabilityForProject[Ctx, V, Out])
    private val viewReq      = viewReqCache(filterDead)
    private val useCases     = project.content.reqs.useCases

    private def forFields0[FK <: FieldKey](r: FK => V) =
      ForFields[Ctx, FK, Out](reusableSelf.withValue(render compose r))

    private def forFields1[A: Reusability : ClassTag, FK <: FieldKey](a: A)(r: FK => V) =
      ForFields[Ctx, FK, Out](reusableSelf.tuple(Reusable.implicitly(a)).withValue(render compose r))

    private def forFieldsIfApp1[A: Reusability : ClassTag, FK <: FieldKey](a: A)(r: FK => IfApplicable[V]) =
      ForFields[Ctx, FK, Out](reusableSelf.tuple(Reusable.implicitly(a)).withValue(renderIfApp compose r))

    def forCodeGroup(rcg: CodeGroup): ForCodeGroup[Ctx, Out] = {
      lazy val code = project.content.reqCodes.reqCode(rcg.id)
      forFields1[ReqCodeGroupId, FieldKey.ForCodeGroup](rcg.id) {
        case FieldKey.CodeGroupTitle => pt.codeGroupTitle(rcg)
        case FieldKey.Code           => pt.reqCode(code)
      }
    }

    def forCodeGroupId(id: ReqCodeGroupId): ForCodeGroup[Ctx, Out] =
      project.content.reqCodes.needById(id) match {
        case ReqCode.ActiveGroup(group, _)               => forCodeGroup(group)
        case ReqCode.Inactive(Some(deadGroup), _)        => forCodeGroup(deadGroup)
        case ReqCode.ActiveReq(_, _, Some(deadGroup), _) => forCodeGroup(deadGroup)
        case ReqCode.Inactive(None, _)
           | ReqCode.ActiveReq(_, _, None, _)            => ForFields.const(Reusable.byRef(NotApplicable.left).map(renderIfApp))
      }

    def forGenericReq(id: GenericReqId): ForGenericReq[Ctx, Out] =
      forReq(id)

    def forReq(id: ReqId): ForReq[Ctx, Out] =
      forFieldsIfApp1[ReqId, FieldKey.ForSomeReq](id)(viewReq(id).render)

    def forUseCase(id: UseCaseId): ForUseCase[Ctx, Out] =
      forReq(id)

    lazy val forUseCaseSteps: ForUseCaseSteps[Ctx, Out] =
      forFields0[FieldKey.UseCaseStep](fk => {
        val focus = useCases.focusStep(fk.id)
        pt.useCaseStepTextAndFlow(focus, filterDead)
      })

    lazy val forManualIssues: ForManualIssues[Ctx, Out] =
      forFields0[FieldKey.ManualIssue] { fk =>
        val issue = project.manualIssues.imap.need(fk.id)
        pt.manualIssue(issue.text)
      }

    // Disabled because it creates a new ViewReqCache. One should be passed in in a parent context and reused, rather
    // than creating a local cache here.
    //
    //def withCtx[Ctx2 <: ProjectText.Context](newCtx: Ctx2): ForProject[Ctx2, Out] = {
    //  val pt2 = pt.withCtx(newCtx)
    //  if (pt eq pt2)
    //    this.asInstanceOf[ForProject[Ctx2, Out]]
    //  else
    //    ForProject(
    //      project,
    //      filterDead,
    //      ViewReqCache(viewReqCache.dataCache, pt2),
    //      pt2,
    //    )
    //}
  }

  implicit def reusabilityForProject[Ctx <: PCtx, V, Out]: Reusability[ForProject[Ctx, V, Out]] =
    Reusability.byRef || Reusability.derive

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ForFields {
    def const[Ctx <: PCtx, FK <: FieldKey, Out](value: Reusable[Out]): ForFields[Ctx, FK, Out] =
      apply(value.map(v => _ => v))
  }

  final case class ForFields[+Ctx <: PCtx, -FK <: FieldKey, Out](renderFn: FK ~=> Out) {
    @inline def apply(fk: FK): Out = renderFn(fk)

    def map[B](f: Out => B): ForFields[Ctx, FK, B] =
      ForFields(renderFn.map(_.andThen(f)))

    def some: ForFields[Ctx, FK, Option[Out]] =
      map(Some(_))
  }

  implicit class ForFieldsInvariantExt[Ctx <: PCtx, FK <: FieldKey, Out](private val self: ForFields[Ctx, FK, Out]) extends AnyVal {
    def widen[W >: FK <: FieldKey](fallback: Out)(implicit t: FieldKey.Type[FK]): ForFields[Ctx, W, Out] =
      ForFields[Ctx, W, Out](self.renderFn.map(f => t.widenFn[W, Out](f)(fallback)))
  }

  implicit def reusabilityForFields[Ctx <: PCtx, FK <: FieldKey, Out]: Reusability[ForFields[Ctx, FK, Out]] =
    Reusability.byRef || Reusability.derive

  type ForCodeGroup   [+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForCodeGroup , Out]
  type ForGenericReq  [+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForGenericReq, Out]
  type ForReq         [+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForSomeReq   , Out]
  type ForUseCase     [+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ForUseCase   , Out]
  type ForUseCaseSteps[+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.UseCaseStep  , Out]
  type ForManualIssues[+Ctx <: PCtx, Out] = ForFields[Ctx, FieldKey.ManualIssue  , Out]

}
