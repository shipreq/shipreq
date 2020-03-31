package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.test.CommonObs._

object ReqTypeConfigObs {

  lazy val selRightOn           = Style.widgets.splitScreenCrud.rightOn   .selector
  lazy val selEmptyRight        = Style.widgets.splitScreenCrud.emptyRight.selector
  lazy val selEditorButtons     = Style.tagConfig.editorButtons           .selector
  lazy val selPreEditorMessage  = Style.reqTypeConfig.preEditorMessage    .selector

  final class ReqTypeList($: DomZipperJs) {
    val rows: Vector[ReqTypeListRow] =
      $("tbody").children1n("tr").map(new ReqTypeListRow(_))

    def apply(mnemonic: String): ReqTypeListRow =
      rows.find(_.mnemonic ==* mnemonic).getOrElse(throw new RuntimeException("Req type not found: " + mnemonic))
  }

  final class ReqTypeListRow($: DomZipperJs) {
    val rowDom   = $.domAsHtml
    val mnemonic = $.child("td", 1 of 4).innerText
    val usage    = new Link($.child("td", 4 of 4)("a"))
  }

  final class Editor($: DomZipperJs) {

    private val form = $ //(".ui.form")
    private val f1   = form.child(".field,.fields", 1 of 4)
    private val f2   = form.child(".field,.fields", 2 of 4)
    private val f3   = form.child(".field,.fields", 3 of 4)
    private val f4   = form.child(".field,.fields", 4 of 4)

    val mnemonic = new Input(f1("input"))
    val name     = new Input(f2("input"))
    val imp      = new Dropdown(f3(".ui.dropdown"))
    val desc     = new TextArea(f4("textarea"))

    val pastMnemonics: Option[String] =
      Option.when(f1.exists(".field"))(f1(".field", 2 of 2)("div").innerText.trim)

    val mnemonicError = f1.children01("span").innerTexts
    val nameError     = f2.children01("span").innerTexts

    lazy val editables =
      collectSemanticUi($, Enabled).doms
  }

}

// =====================================================================================================================

final class ReqTypeConfigObs($: DomZipperJs) {
  import ReqTypeConfigObs._

  val left  = $.child("section", 1 of 2)
  val right = $.child("section", 2 of 2).child(selRightOn)

  val filterDeadButton = left.child("div").child("div", 2 of 2)("button").dom
  val filterDead       = ShowDead.when(filterDeadButton.classList.contains("red"))

  val list = new ReqTypeList(left)

  val isEditorOpen: Boolean =
    !right.exists(selEmptyRight)

  val buttonDoms: Buttons[html.Button] =
    if (isEditorOpen)
      Buttons.obs(right(selEditorButtons))
    else
      Buttons.none

  val buttonsEnabled: Buttons[Enabled] =
    buttonDoms.map(Disabled when _.disabled)

  val editorTitle: Option[String] =
    Option.when(isEditorOpen)(right.child("div").child("h2").innerText.trim)

  val editor: Option[Editor] =
    Option.when(isEditorOpen)(new Editor(right.child("div").child(s"div:not($selPreEditorMessage)", 1 of 2)))

  val newButton =
    left.collect1n(".ui.button").filter(_.innerText.trim == "New").singleton.domAsHtml
}
