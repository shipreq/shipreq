package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.microlibs.stdlib_ext.MutableArray
import org.scalajs.dom.html
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.CommonObs._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.config.Buttons

object IssueConfigObs {

  private lazy val selRightOn             = Style.widgets.splitScreenCrud.rightOn.selector
  private lazy val selOtherSources        = Style.issueConfig.otherSources       .selector
  private lazy val selOtherSourcesContent = Style.issueConfig.otherSourcesContent.selector
  private lazy val selEditorButtons       = Style.tagConfig.editorButtons        .selector

  final class OtherSources($: DomZipperJs) {
    val sources = $.collect1n(selOtherSourcesContent).map(new OtherSource(_))

    lazy val consolidation: String =
      MutableArray(sources).sortBy(_.header).iterator().map(s =>
        s.header + "\n" + s.items.iterator.map("  * " + _).mkString("\n")
      ).mkString("\n\n")
  }

  final class OtherSource($: DomZipperJs) {
    val header = $.child("div", 1 of 2).innerText.trim
    val items = $.collect0n("li").innerTexts.map(_.trim)
  }

  final class IssueList($: DomZipperJs) {
    val rows: Vector[IssueListRow] =
      $("tbody").children1n("tr").map(new IssueListRow(_))

    def apply(key: String): IssueListRow =
      rows.find(_.key ==* key).getOrElse(throw new RuntimeException("Issue type not found: " + key))
  }

  final class IssueListRow($: DomZipperJs) {
    val rowDom = $.domAsHtml
    val key    = $.child("td", 1 of 3).innerText
    val desc   = $.child("td", 2 of 3).innerText
    val usage  = new Link($.child("td", 3 of 3)("a"))
  }

  final class Editor($: DomZipperJs) {
    val editorTitle = $.child("h2").innerText.trim

    private val form = $(".ui.form")
    private val f1   = form.child(".field", 1 of 2)
    private val f2   = form.child(".field", 2 of 2)

    val key      = new Input(f1("input"))
    val keyError = f1.children01("span").innerTexts
    val desc     = new TextArea(f2("textarea"))

    lazy val editables =
      collectSemanticUi($, Enabled).doms
  }

}

// =====================================================================================================================

final class IssueConfigObs($: DomZipperJs) {
  import IssueConfigObs._

  private val _right = $.child("section", 2 of 2)

  val left  = $.child("section", 1 of 2)
  val right = _right.child(selRightOn)

  val filterDeadButton = left.child("div", 2 of 2).child("div", 2 of 2)("button").dom
  val filterDead       = ShowDead.when(filterDeadButton.classList.contains("red"))

  val list = new IssueList(left)

  val otherSources: Option[OtherSources] =
    right.collect01(selOtherSources).map(new OtherSources(_))

  val isEditorOpen: Boolean =
    otherSources.isEmpty

  val buttonDoms: Buttons[html.Button] =
    if (isEditorOpen)
      Buttons.obs(right(selEditorButtons))
    else
      Buttons.none

  val buttonsEnabled: Buttons[Enabled] =
    buttonDoms.map(Disabled when _.disabled)

  val editor: Option[Editor] =
    Option.when(isEditorOpen)(new Editor(right.child("div")))

  val newButton =
    left.collect1n(".ui.button").filter(_.innerText.trim == "New").singleton.domAsHtml
}
