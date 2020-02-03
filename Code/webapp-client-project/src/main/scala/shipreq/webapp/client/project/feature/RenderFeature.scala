package shipreq.webapp.client.project.feature

import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.base.text.ProjectText.{Context => PCtx}
import shipreq.webapp.client.project.widgets.ViewReqCache

/** Provides the ability to render for display, parts of a project using row & field keys.
  *
  *  == Usage ==
  *
  * 1. Call [[.prepare()]] and store it for reuse. In practice, this is going to be inside a `Px` because it's derived
  *    from dependencies that are all going to be inside `Px`s too.
  *
  * 2. Apply the current [[FilterDead]] setting to get a [[RenderFeature.ForProject]]
  *
  * 3. Pass the [[RenderFeature.ForProject]] to downstream components for them to use.
  */
object RenderFeature {

  type FieldKey = render.FieldKey
  val  FieldKey = render.FieldKey

  type RowKey = render.RowKey
  val  RowKey = render.RowKey

  type ForProject[+Ctx <: PCtx, Out] = render.Feature.ForProject[Ctx, Out]
  val  ForProject                    = render.Feature.ForProject

  type ForFields[+Ctx <: PCtx, -FK <: FieldKey, Out] = render.Feature.ForFields[Ctx, FK, Out]
  val  ForFields                                     = render.Feature.ForFields

  sealed trait TypeHelpers[Ctx <: PCtx, Out] {
    final type ForProject                = render.Feature.ForProject     [Ctx, Out]
    final type ForField[FK <: FieldKey]  = render.Feature.ForFields      [Ctx, FK, Out]
    final type ForCodeGroup              = render.Feature.ForCodeGroup   [Ctx, Out]
    final type ForGenericReq             = render.Feature.ForGenericReq  [Ctx, Out]
    final type ForReq                    = render.Feature.ForReq         [Ctx, Out]
    final type ForUseCase                = render.Feature.ForUseCase     [Ctx, Out]
    final type ForUseCaseSteps           = render.Feature.ForUseCaseSteps[Ctx, Out]
    final type ForManualIssues           = render.Feature.ForManualIssues[Ctx, Out]
  }

  object ToVdom {
    import japgolly.scalajs.react.vdom.html_<^.VdomTag

    object AnyCtx extends TypeHelpers[PCtx, VdomTag]
    object NoCtx extends TypeHelpers[PCtx.None, VdomTag]
  }

  def prepare[Ctx <: PCtx, Out](project     : Project,
                                viewReqCache: ViewReqCache[Ctx, Out],
                                projectText : ProjectText[Ctx, Out]): FilterDead => ForProject[Ctx, Out] =
    FilterDead.memo(ForProject(project, _, viewReqCache, projectText))

}
