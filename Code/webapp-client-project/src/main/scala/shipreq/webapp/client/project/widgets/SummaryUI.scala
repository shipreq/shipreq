package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.base.ui.semantic.Icon

/** UI to quickly and concisely communicate to the user, a summary of content.
  */
object SummaryUI {

  final case class SummaryIcon(icon: Icon, title: String, mod: TagMod = TagMod.empty) {
    val vdom = icon.tag(^.title := title, ^.marginRight := "0", mod)
  }

  object SummaryIcon {
    @inline implicit def toVdom(i: SummaryIcon) = i.vdom

    val reqs     = apply(Icon.Cubes        , "requirements", ^.marginLeft := "0.1ex")
    val rcgs     = apply(Icon.FolderOutline, UiText.codeGroups.toLowerCase)
    val delete   = apply(Icon.TrashOutline , "deleted")
    val filter   = apply(Icon.Filter       , "excluded by the filter")
    val selected = apply(Icon.CheckmarkBox , "selected")
    val config   = apply(Icon.Setting      , "configuration")
    val issue    = apply(Icon.WarningSign  , "issue")
    val loose    = apply(Icon.Keyboard     , UiText.Issues.loose)
    def reappearances(title: String) = apply(Icon.Copy, title)
  }

  val fakeLine = <.span("_", ^.visibility.hidden)

  def selected(n: Int): Option[TagMod] =
    Option.when(n > 0)(
      TagMod(n, SummaryIcon.selected.vdom))
}

final class SummaryUI {
  private var parts = Vector.empty[TagMod]
  var beginning = true

  def add(as: TagMod*): Unit = {
    parts ++= as
    beginning = false
  }

  def addUnlessZero(n: Int, icon: SummaryUI.SummaryIcon): Unit =
    if (n != 0) {
      val txt: String =
        if (beginning)
          n.toString
        else if (n < 0)
          " - " + (-n)
        else
          " + " + n
      add(txt, icon.vdom)
    }

  def result: TagMod =
    TagMod.Composite(parts)

  def resultNonEmpty: Option[TagMod] =
    Option.when(parts.nonEmpty)(result)

  /** eg. "Showing 3 issues: <result>" */
  def prefixWithShowing(total: Int, unit: String) =
    TagMod(s"Showing ${total.unitsOf(unit)}: ", result)
}
