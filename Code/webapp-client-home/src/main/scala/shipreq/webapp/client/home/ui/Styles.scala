package shipreq.webapp.client.home.ui

import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.ui.BaseStyles

object Styles extends StyleSheet.Inline {
  import dsl._

  val createProjectCont = style(
    marginTop(7 rem))

  val createProjectInput = style(
    width(42 ex),
    &.focus(borderColor(c"#68ca7c").important))

  val noProjects = style(
    marginTop(3 rem))

  val projectList = style(
    marginTop(3 rem))
}