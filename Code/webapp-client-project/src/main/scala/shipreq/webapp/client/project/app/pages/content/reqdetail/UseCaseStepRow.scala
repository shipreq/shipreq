package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.VectorTree.{LocationOps, PartialLocation}
import shipreq.base.util._
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.member.UiText
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.protocol.websocket.UpdateContentCmd

object UseCaseStepRow {

  object Label {
    final case class Props(field     : StaticField.UseCaseStepTree,
                           fullLabel : String,
                           partialLoc: PartialLocation) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.derive

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

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object LiveControls {
    final case class Props(ucId         : UseCaseId,
                           field        : StaticField.UseCaseStepTree,
                           id           : UseCaseStepId,
                           label        : String,
                           live         : Live,
                           loc          : VectorTree.Location,
                           canShiftRight: Permission,
                           runCtrl      : AsyncFeature.Runner.D0[UpdateContentCmd.ForUseCaseStep, Any],
                           runAdd       : AsyncFeature.Runner.D0[UpdateContentCmd.AddUseCaseStep, Any]) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.derive

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
            val canShift: LeftRight => Permission = {
              case LeftRight.Left  => field.canShiftLeft(loc)
              case LeftRight.Right => canShiftRight
            }
            CurStepButtons.WhenLive(
              delete = field.canDelete(loc).option(ButtonDesc(runCtrl.run(DeleteUseCaseStep(id)), "Delete " + label)),
              shift = LeftRight.Values(d => canShift(d).option(
                ButtonDesc(runCtrl.run(ShiftUseCaseStep(id, d)), UiText.useCaseStepShift(d) + " " + label))))

          case Dead =>
            CurStepButtons.WhenDead(
              restore = ButtonDesc(runCtrl.run(RestoreUseCaseStep(id)), "Restore " + label))
        }

      val addButton = field.canInsertAfter(loc).option(
        ButtonDesc(runAdd.run(AddUseCaseStep(ucId, field, loc.asParentLoc)), "Insert after " + label))

      UseCaseStepControls.renderStep(
        curStepButtons = curStepButtons,
        curStepAsync   = runCtrl.asyncState,
        insertButton   = addButton,
        insertAsync    = runAdd.asyncState)
    }

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
