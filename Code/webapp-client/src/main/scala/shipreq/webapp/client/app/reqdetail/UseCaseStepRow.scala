package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.AppConsts
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.client.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.feature._
import shipreq.webapp.client.lib.DataReusability._
import VectorTree.{PartialLocation, LocationOps}

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
      <.div(ReactAttr.devOnly("data-step-label") := 1)

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
            label = label.dropWhile(_ !=* AppConsts.useCaseStepsDeadNode)
          label += "."

          baseTag(
            *.header(badInd),
            ^.title := p.fullLabel,
            <.span(
              *.deadStepLabel,
              label))
      }

    val Component = ReactComponentB[Props]("UseCaseStep.Label")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object LiveControls {
    final case class Props(ucId           : UseCaseId,
                           field          : StaticField.UseCaseStepTree,
                           id             : UseCaseStepId,
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
      import UseCaseStepControls.{Props => P}
      import p._

      val self = {
        val self0: P.Self =
          live match {
            case Live =>
              val d  = field.canDelete(loc).option(runCtrl(DeleteUseCaseStep(id)))
              val sl = field.canShiftLeft(loc).option((P.ShiftLeft, runCtrl(ShiftUseCaseStepLeft(id))))
              val sr = canShiftRight.option((P.ShiftRight, runCtrl(ShiftUseCaseStepRight(id))))
              P.WhenLive(d, sl, sr)

            case Dead =>
              P.WhenDead(runCtrl(RestoreUseCaseStep(id)))
          }
        Some((self0, ctrlsAsyncState))
      }

      val add = field.canAdd(loc).option((runAdd(AddUseCaseStep(ucId, field, loc.asParentLoc)), addAsyncState))

      P(self, add).render
    }

    val Component = ReactComponentB[Props]("UseCaseStep.LiveCtrls")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
