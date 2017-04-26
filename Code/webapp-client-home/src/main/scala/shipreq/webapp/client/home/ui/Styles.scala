package shipreq.webapp.client.home.ui

import shipreq.webapp.client.base.CssSettings._
import shipreq.webapp.client.base.ui.BaseStyles

object Styles extends StyleSheet.Inline {
  import dsl._

  val createProjectContainer = style(
    marginBottom(BaseStyles.projectItems.vspace))
}