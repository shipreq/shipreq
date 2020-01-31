package shipreq.webapp.client.project.app.reqdetail

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.lib.DataReusability._
import UseCaseStepFlowText.TextAndFlow

object UseCaseStepTree {

  final case class StepData(row: Row.UseCaseSteps, steps: UseCaseSteps, treeFilter: Range) {
    @inline def field = row.field
    val mdt = steps.tree.maxDepthTree
  }

  final case class RenderArgs(base       : VdomTag,
                              id         : UseCaseStepId,
                              live       : Live,
                              textAndFlow: () => TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]])

  type RenderBodyFn = RenderArgs => VdomElement

  final case class Props(uc          : UseCase,
                         stepData    : StepData,
                         filterDead  : FilterDead,
                         useCases    : UseCases,
                         renderBody  : RenderBodyFn, // TODO <------------------ prevents Reuse. Underlying fn uses state.
                         cmdRunner   : AsyncFeature.Runner.D1[Cell, UpdateContentCmd.ForUseCaseStep, Any],
                         addCmdRunner: AsyncFeature.Runner.D1[Cell, UpdateContentCmd.AddUseCaseStep, Any]) {
    @inline def render = Component(this)
  }

  val Component = ScalaComponent.builder[Props]("UseCaseStepTree")
    .render_P(render)
    .build

  private val stepBodyBase =
    <.div(
      *.body,
      TestMarker.useCaseStepText.tagMod,
      ^.tabIndex := -1,
      TableNavigationFeature.newRow)

  private val tailStepBase =
    <.div(
      *.container,
      ^.key := "TS",
      TestMarker.useCaseTailStep.tagMod)

  private val stepFilterM: FilterDead => VectorTree.PartialLocation => Boolean =
    FilterDead.memo(_.filterFnBy(Live whenValid _.validity))

  private def render(p: Props): VdomElement = {
    import p._
    import stepData._

    val pos        = uc.pubid.pos
    val stepFilter = stepFilterM(filterDead)

    val results = VdomArray.empty()

    steps.tree.subtreeLocAndValueIterator(treeFilter, (loc, step) => {
      val partialLoc = steps.partialLocs.forward(loc)
      if (stepFilter(partialLoc)) {
        val id        = step.id
        val focus     = useCases.focusStep(id)
        val live      = focus.live
        val fullLabel = field.stepLabel(pos, partialLoc, UseCaseStepLabelFmt.`N.m`)

        def text =
          renderBody(
            RenderArgs(
              stepBodyBase,
              id,
              live,
              () => focus.textAndFlow(filterDead)))

        def ctrls: VdomElement =
          uc.liveUC match {
            case Live =>
              val cellCtrls = Cell.UseCaseStepCtrls(id)
              val cellAdd   = Cell.AddUseCaseStep(id)
              UseCaseStepRow.LiveControls.Props(
                uc.id, field, id, fullLabel, live, loc,
                canShiftRight = field.canShiftRight(loc, steps.locValidity, stepData.mdt),
                runCtrl       = cmdRunner(cellCtrls),
                runAdd        = addCmdRunner(cellAdd),
              ).render

            case Dead =>
              UseCaseStepControls.renderStepWhenUseCaseDead
          }

        results +=
          <.div(*.container,
            ^.key := fullLabel,
            UseCaseStepRow.Label.Props(field, fullLabel, partialLoc).render,
            text,
            ctrls)
      }
    }).drain()

    if (row.tailStep && uc.liveUC.is(Live)) {
      val loc   = VectorTree.Location(steps.tree.children.count(_.value.liveExplicitly is Live))
      val ploc  = VectorTree.PartialLocation(loc, Valid)
      val lbl   = field.stepLabel(pos, ploc, UseCaseStepLabelFmt.`N.m`)
      def cmd   = UpdateContentCmd.AddUseCaseStep(uc.id, field, VectorTree.ParentLocation.Empty)
      val cell  = Cell.AddUseCaseTailStep(row)
      val cb    = addCmdRunner(cell).run(cmd)
      val as    = addCmdRunner.asyncState(cell)
      val bd    = UseCaseStepControls.ButtonDesc(cb, "Create " + lbl)
      val ctrls = UseCaseStepControls.renderTailStep(bd, as)
      results  += tailStepBase(ctrls)
    }

    <.div(results)
  }
}
