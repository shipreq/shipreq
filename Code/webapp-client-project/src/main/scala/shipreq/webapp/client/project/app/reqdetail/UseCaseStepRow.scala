package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.client.base.feature.AsyncActionFeature
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.lib.DataReusability._
import VectorTree.{LocationOps, PartialLocation}

object UseCaseStepRow {

  object Label {
    final case class Props(field     : StaticField.UseCaseStepTree,
                           fullLabel : String,
                           partialLoc: PartialLocation) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    private val baseTag =
      <.div(TestMarker.useCaseStepLabel.tagMod)

    private def render(p: Props) =
      p.partialLoc.validity match {
        case Valid =>
          val depth = p.partialLoc.value.length // ≥ 1
          val short = if (depth == 1)
            p.fullLabel
          else {
            // Last node asserted to be ≥ 0 in PartialLocation
            val i = p.partialLoc.value.last
            p.field.stepLabelsPerLevel(depth - 1).label(i)
          }

          baseTag(
            *.header(depth - 1),
            ^.title := p.fullLabel,
            short + ".")

        case Invalid =>
          val badInd = p.partialLoc.value.whole.indexWhere(_ < 0)
          var label = p.fullLabel
          if (badInd !=* 0)
            label = label.dropWhile(_ !=* WebappConfig.useCaseStepsDeadNode)
          label += "."

          baseTag(
            *.header(badInd),
            ^.title := p.fullLabel,
            <.span(
              *.deadStepLabel,
              label))
      }

    val Component = ScalaComponent.build[Props]("UseCaseStep.Label")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object LiveControls {
    final case class Props(ucId           : UseCaseId,
                           field          : StaticField.UseCaseStepTree,
                           id             : UseCaseStepId,
                           label          : String,
                           live           : Live,
                           loc            : VectorTree.Location,
                           canShiftRight  : Permission,
                           ctrlsAsyncState: AsyncActionFeature.D0.State[Any],
                           runCtrl        : UpdateContentCmd.ForUseCaseStep ~=> Callback,
                           addAsyncState  : AsyncActionFeature.D0.State[Any],
                           runAdd         : UpdateContentCmd.AddUseCaseStep ~=> Callback) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    private def render(p: Props) = {
      import UpdateContentCmd._
      import UseCaseStepControls.{ButtonDesc, CurStepButtons}
      import p._

      // TODO Hover text: Instead describing current state, describe what the future state will be if pressed.
      // Eg. "Shift 3.4.a left" → "Shift 3.4.a left to become 3.5"
      // Eg. "Insert after 3.4.a" → "Create 3.4.b"

      val curStepButtons: CurStepButtons =
        live match {
          case Live =>
            CurStepButtons.WhenLive(
              delete     = field.canDelete(loc).option(ButtonDesc(runCtrl(DeleteUseCaseStep(id)), "Delete " + label)),
              shiftLeft  = field.canShiftLeft(loc).option(ButtonDesc(runCtrl(ShiftUseCaseStepLeft(id)), "Unindent " + label)),
              shiftRight = canShiftRight.option(ButtonDesc(runCtrl(ShiftUseCaseStepRight(id)), "Indent " + label)))
          case Dead =>
            CurStepButtons.WhenDead(
              ButtonDesc(runCtrl(RestoreUseCaseStep(id)), "Restore " + label))
        }

      val addButton = field.canInsertAfter(loc).option(
        ButtonDesc(runAdd(AddUseCaseStep(ucId, field, loc.asParentLoc)), "Insert after " + label))

      UseCaseStepControls.renderStep(curStepButtons, ctrlsAsyncState, addButton, addAsyncState)
    }

    val Component = ScalaComponent.build[Props]("UseCaseStep.LiveCtrls")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
