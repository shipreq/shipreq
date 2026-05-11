package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.member.jsfacade.MomentJs
import shipreq.webapp.member.project.util.DataReusability._

object TimeAgo {

  type Props = MomentJs

  final class Backend($: BackendScope[Props, String]) extends TimerSupport {

    val updateState: Callback =
      $.props >>= (p => $.setState(p.ago()))

    lazy val scheduleUpdates: Callback =
      $.props.flatMap { m =>
        val diffInMin = Math.abs(m.toEpochSecondD - MomentJs.now().toEpochSecondD) / 60
        val updateInSec =
               if (diffInMin < 2.5)         2 // 0    - 2.5m: update 2 seconds
          else if (diffInMin < 45)         15 // 2.5m -  45m: update 15 seconds
          else if (diffInMin < 24*60) 15 * 60 // 45m  -  24h: update every 15 minutes
          else                        60 * 60 // 24h+       : update every hour

        // println(s"scheduleUpdate - ${m.ago()} - Δ=${diffInMin}min >> ${updateInSec}s")
        setTimeoutMs(updateState >> scheduleUpdates, updateInSec * 1000)
      }

    def onPropUpdate(currentProps: Props, nextProps: Props): Callback =
      Callback.unless(currentProps ~=~ nextProps)(
        unmount >> $.setState(nextProps.ago()))

    def render(p: Props, state: String): VdomElement =
      <.time(
        ^.cursor.help,
        ^.dateTime := p.formatIso8601,
        ^.title    := p.formatHuman,
        state)
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(_.ago())
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(TimerSupport.install)
    .componentDidMount(_.backend.scheduleUpdates)
    .componentDidUpdate(i => i.backend.onPropUpdate(i.prevProps, i.currentProps))
    .build
}
