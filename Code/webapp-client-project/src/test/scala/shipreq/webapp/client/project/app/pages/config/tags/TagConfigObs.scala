package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.scalajs.dom.html
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

object TagConfigObs {

  lazy val selUsage         = Style.tagConfig.usage                   .selector
  lazy val selDeadGroup     = Style.tagConfig.group(Dead)             .selector
  lazy val selRightOn       = Style.widgets.splitScreenCrud.rightOn   .selector
  lazy val selEmptyRight    = Style.widgets.splitScreenCrud.emptyRight.selector
  lazy val selEditorButtons = Style.tagConfig.editorButtons           .selector
  lazy val selEditorRelRow  = Style.tagConfig.editorRelRow            .selector

  final class TagTreeOL($: DomZipperJs, val depth: Int) {
    val lis: Vector[TagTreeLI] =
      $.children0n("li").map(new TagTreeLI(_, depth))

    def liIterator(): Iterator[TagTreeLI] =
      lis.iterator.flatMap(li => Iterator.single(li) ++ li.subtree.iterator.flatMap(_.liIterator()))

    def text: String =
      lis
        .flatMap(li => li.text :: li.subtree.map(_.text.indent("- ")).toList)
        .mkString("\n")
  }

  final class TagTreeLI($: DomZipperJs, val depth: Int) {
    val subtree: Option[TagTreeOL] =
      $.children01("ol").map(new TagTreeOL(_, depth + 1))

    val rowDom =
      if (subtree.isEmpty)
        $.domAsHtml
      else
        $(">div").domAsHtml

    private def innerTextWithoutUsage($$: DomZipperJs): String = {
      val t = $$.innerText.trim
      $$.collect01(selUsage).innerTexts.map(_.trim) match {
        case Some(u) if t.endsWith(u) => t.dropRight(u.length).trim
        case _                        => t
      }
    }

    val title: String =
      if (subtree.isDefined)
        innerTextWithoutUsage($.child("div"))
      else
        innerTextWithoutUsage($).replace(DragToReorderFeature.dragHamburger, "")

    // println(">>> " + $.outerHTML)

    val text: String =
      if (subtree.isDefined) {
        val dead     = $(">div").exists(selDeadGroup)
        val selected = $.domAsHtml.get(TagTreeView.selected) == "true"
        var t = title
        if (dead) t += " [DEAD]"
        if (selected) t += " [SELECTED]"
        t
      } else {
        val selected  = $.domAsHtml.get(TagTreeView.selected) == "true"
        val draggable = $.exists("[draggable=true]")
        val dead      = $.exists(".ui.label.grey")
        var t = innerTextWithoutUsage($)
        t = t.replace(DragToReorderFeature.dragHamburger, if (draggable) "[=] " else "")
        if (dead) t += " [DEAD]"
        if (selected) t += " [SELECTED]"
        t
      }
  }

  final class RelTree($: DomZipperJs) {
    val items: Vector[RelTreeItem] =
      $.collect01("ol").zippers.map(_.collect0n("li").map(new RelTreeItem(_))).getOrElse(Vector.empty)

    def text =
      items.iterator.map(_.text).mkString("\n")

    private val addBtn = $(".dropdown.button")

    private val addBtnItems = addBtn.collect0n(".item").map(new RelTreeAddItem(_))

    def addItem(title: String): Option[RelTreeAddItem] =
      addBtnItems.find(_.title == title)
  }

  final class RelTreeItem($: DomZipperJs) {
    val title = $("span").innerText

    val text = {
      val dead      = $.exists(".ui.label.grey") || $.exists(selDeadGroup)
      val draggable = $.exists("[draggable=true]")
      var t = title
      if (dead) t += " [DEAD]"
      if (draggable) t = "[=] " + t
      t
    }
  }

  final class RelTreeAddItem($: DomZipperJs) {
    val dom = $.domAsHtml
    val title = $.innerText.trim
  }
}

final class TagConfigObs($: DomZipperJs) {
  import TagConfigObs._

  val left  = $.child("section", 1 of 2)
  val right = $.child("section", 2 of 2).child(selRightOn)

  val tagTree = new TagTreeOL(left.child("ol"), 0)

  //println("="*40); println(tagTree.text)

  def tagTreeLI(tagName: String): TagTreeLI =
    tagTree.liIterator().filter(_.title == tagName).toList match {
      case li :: Nil => li
      case Nil       => throw new RuntimeException(s"tagTreeLI($tagName) not found")
      case _         => throw new RuntimeException(s"tagTreeLI($tagName) is ambiguous")
    }

  val filterDeadButton = left.child("div").child("div", 2 of 2)("button").dom
  val filterDead       = ShowDead.when(filterDeadButton.classList.contains("red"))

  val isEditorOpen: Boolean =
    !right.exists(selEmptyRight)

  val buttonDoms: Buttons[html.Button] =
    if (isEditorOpen) {
      val $$ = right(selEditorButtons)
      var bs = Buttons.none: Buttons[html.Button]
      for (b <- $$.collect0n("button").zippers) {
        b.innerText.trim match {
          case "Update" | "Create" => bs = bs.copy(save    = Some(b.domAs[html.Button]))
          case "Cancel"            => bs = bs.copy(cancel  = Some(b.domAs[html.Button]))
          case "Close"             => bs = bs.copy(close  = Some(b.domAs[html.Button]))
          case "Delete"            => bs = bs.copy(delete  = Some(b.domAs[html.Button]))
          case "Restore"           => bs = bs.copy(restore = Some(b.domAs[html.Button]))
        }
      }
      bs
    } else
      Buttons.none

  val buttonsEnabled: Buttons[Enabled] =
    buttonDoms.map(Disabled when _.disabled)

  private def relTree(name: String): Option[RelTree] =
    $.collect01(selEditorRelRow + " > div")
      .filter(_(".header").innerText == name)
      .zippers
      .map(new RelTree(_))

  val editorParents  = relTree("Parents")
  val editorChildren = relTree("Children")
}
