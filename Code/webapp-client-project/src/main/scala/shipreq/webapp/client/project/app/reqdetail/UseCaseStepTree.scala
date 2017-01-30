package shipreq.webapp.client.project.app.reqdetail

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalajs.js.{Array => JArray}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.data._
import shipreq.webapp.client.base.feature.AsyncActionFeature
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import UseCaseStepFlowText.TextAndFlow

object UseCaseStepTree {

  final case class StepData(row: Row.UseCaseSteps, steps: UseCaseSteps, treeFilter: Range) {
    @inline def field = row.field
    val mdt = steps.tree.maxDepthTree
  }

  type RenderBodyFn = (UseCaseStepId, Live, TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]]) => TagMod

  final case class Props(uc        : UseCase,
                         stepData  : StepData,
                         filterDead: FilterDead,
                         flow      : UseCases.StepFlow,
                         renderBody: RenderBodyFn, // TODO <------------------ prevents Reuse. Underlying fn uses state.
                         asyncState: AsyncActionFeature.D1.State.ReadOnly[Cell, Any],
                         runCmd    : Cell ~=> (UpdateContentCmd ~=> Callback)) {
    @inline def render = Component(this)
  }

  val Component = ReactComponentB[Props]("UseCaseStepTree")
    .render_P(render)
    .build

  private val stepBodyBase = <.div(*.body, TestMarker.useCaseStepText.tagMod)
  private val tailStepBase = <.div(*.container, ^.key := "TS", TestMarker.useCaseTailStep.tagMod)

  private val stepFilterM: FilterDead => VectorTree.PartialLocation => Boolean =
    FilterDead.memo(_.filterFnBy(Live whenValid _.validity))

  private def render(p: Props): ReactElement = {
    import p._
    import stepData._

    val pos        = uc.pubid.pos
    val stepFilter = stepFilterM(filterDead)

    val results = new JArray[ReactNode]

    steps.tree.subtreeLocAndValueIterator(treeFilter, (loc, step) => {
      val partialLoc = steps.partialLocs.forward(loc)
      if (stepFilter(partialLoc)) {
        val id        = step.id
        val live      = UseCaseStep.live(uc, partialLoc)
        val fullLabel = field.stepLabel(pos, partialLoc, mnemonicPrefix = false)

        def text =
          stepBodyBase(
            renderBody(id, live, TextAndFlow(step.titleA(uc), flow(_)(id))))

        def ctrls =
          uc.liveUC match {
            case Live =>
              val cellCtrls = Cell.UseCaseStepCtrls(id)
              val cellAdd   = Cell.AddUseCaseStep(id)
              UseCaseStepRow.LiveControls.Props(
                uc.id, field, id, fullLabel, live, loc,
                field.canShiftRight(loc, steps.locValidity, stepData.mdt),
                asyncState(cellCtrls), runCmd(cellCtrls),
                asyncState(cellAdd  ), runCmd(cellAdd  )
              ).render

            case Dead =>
              UseCaseStepControls.renderStepWhenUseCaseDead
          }

        results.push(
          <.div(*.container,
            ^.key := fullLabel,
            UseCaseStepRow.Label.Props(field, fullLabel, partialLoc).render,
            text,
            ctrls))
      }
    }).drain()

    if (row.tailStep && uc.liveUC :: Live) {
      val loc   = VectorTree.Location(steps.tree.children.count(_.value.liveExplicitly :: Live))
      val ploc  = VectorTree.PartialLocation(loc, Valid)
      val lbl   = field.stepLabel(pos, ploc, false)
      def cmd   = UpdateContentCmd.AddUseCaseStep(uc.id, field, VectorTree.ParentLocation.Empty)
      val cell  = Cell.AddUseCaseTailStep(row)
      val cb    = runCmd(cell)(cmd)
      val as    = asyncState(cell)
      val bd    = UseCaseStepControls.ButtonDesc(cb, "Create " + lbl)
      val ctrls = UseCaseStepControls.renderTailStep(bd, as)
      results push tailStepBase(ctrls)
    }

    <.div(results)
  }
}
