package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.Reusable
import japgolly.scalajs.react.vdom.html_<^.VdomTag
import scalaz.\/-
import shipreq.base.util.IfApplicable
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

  type ForProject[+Ctx <: PCtx, Out] = render.Feature.ForProject[Ctx, _, Out]
  val  ForProject                    = render.Feature.ForProject

  type ForFields[+Ctx <: PCtx, -FK <: FieldKey, Out] = render.Feature.ForFields[Ctx, FK, Out]
  val  ForFields                                     = render.Feature.ForFields

  // ===================================================================================================================

  object Helpers {

    sealed trait First[Out] {
      object AnyCtx extends Helpers.WithCtx[PCtx     , Out]
      object ReqCtx extends Helpers.WithCtx[PCtx.Req , Out]
      object NoCtx  extends Helpers.WithCtx[PCtx.None, Out]
    }

    sealed trait WithCtx[Ctx <: PCtx, Out] {
      object IfApplicable     extends Last[Ctx, Out, IfApplicable[Out]](\/-(_), identity)
      object ApplicableOption extends Last[Ctx, Out, Option[Out]      ](Some(_), _.toOption)
    }

    sealed abstract class Last[Ctx <: PCtx, V, Out](f1: V => Out, f2: IfApplicable[V] => Out) {
      final type ForProject                = render.Feature.ForProject     [Ctx, V, Out]
      final type ForField[FK <: FieldKey]  = render.Feature.ForFields      [Ctx, FK, Out]
      final type ForCodeGroup              = render.Feature.ForCodeGroup   [Ctx, Out]
      final type ForGenericReq             = render.Feature.ForGenericReq  [Ctx, Out]
      final type ForReq                    = render.Feature.ForReq         [Ctx, Out]
      final type ForUseCase                = render.Feature.ForUseCase     [Ctx, Out]
      final type ForUseCaseSteps           = render.Feature.ForUseCaseSteps[Ctx, Out]
      final type ForManualIssues           = render.Feature.ForManualIssues[Ctx, Out]

      private val rf1 = Reusable.byRef(f1)
      private val rf2 = Reusable.byRef(f2)

      final def prepare(project     : Project,
                        viewReqCache: ViewReqCache[Ctx, V],
                        projectText : ProjectText[Ctx, V]): FilterDead => ForProject =
        FilterDead.memo(fd =>
          ForProject(project, fd, viewReqCache, projectText, rf1, rf2))
    }
  }

  object ToText extends Helpers.First[String]
  object ToVdom extends Helpers.First[VdomTag]
}
