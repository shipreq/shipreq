package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.member.ui.BaseStyles.{layout => *}

object MemberLayout {

  final case class Props(nav: MemberNavBar.Props, main: TagMod => VdomElement) {
    @inline def render = Component(this)
  }

  private def render(p: Props): VdomElement =
    <.div(*.root,
      p.nav.render,
      p.main(*.main))

  val Component = ScalaFnComponent(render)
}
