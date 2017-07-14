package shipreq.webapp.client.home.ui

import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.ui.BaseStyles

object Styles extends StyleSheet.Inline {
  import dsl._

  val createProjectContainer = style(
    marginBottom(BaseStyles.projectItems.vspace))
}