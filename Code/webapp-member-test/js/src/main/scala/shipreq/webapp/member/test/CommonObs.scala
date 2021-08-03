package shipreq.webapp.member.test

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Invalid, Valid}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.semantic.JQuery
import shipreq.webapp.member.feature.EditControlsFeature
import shipreq.webapp.member.feature.PreviewFeature.Position
import shipreq.webapp.member.ui.BaseStyles

object CommonObs {

  private object Selectors {
    val previewButtons             = BaseStyles.previewToggleWrapper2.selector + " button"
    val fullscreen                 = BaseStyles.fullscreen.selector
    val editorInvalid              = ".pointing.red.label"
    val textEditorLeftPreviewRight = EditControlsFeature.Mode.values.map(BaseStyles.textEditorLeftPreviewRight(_).selector).mkString(",")
  }

  // ===================================================================================================================

  final class Input($: DomZipperJs) {
    val dom = $.domAs[html.Input]
    val value = dom.value
    def setValue(v: String) = SimEvent.Change(v).simulate(dom)
    def setValueDirect(v: String): Unit = dom.value = v
  }

  // ===================================================================================================================

  final class TextArea($: DomZipperJs) {
    val dom = $.domAs[html.TextArea]
    val value = dom.value
    def setValue(v: String) = SimEvent.Change(v).simulate(dom)
  }

  // ===================================================================================================================

  final class Link($: DomZipperJs) {
    val dom = $.domAs[html.Anchor]
    val label = $.innerText.trim
    def click() = Simulate.click(dom)
  }

  // ===================================================================================================================

  final class Dropdown($: DomZipperJs) {

    private val $$ = {
      val d = $.domAsHtml
      if (d.classList.contains("dropdown"))
        $
      else
        $(".ui.dropdown")
    }

    val dom = $$.domAsHtml

    val hasError = $.domAsHtml.classList.contains("error")

    val text = dom.children(0).innerText

    val selected = $$.collect01(".text").innerTexts.map(_.trim)

    val items = $$.collect0n(".menu .item").map(new DropdownItem(_))

    def select(name: String): Unit = {
      val itemDoms = items.iterator.filter(_.text == name).map(_.dom).toVector
      if (itemDoms.length == 1) {
        val itemDom = itemDoms.head

        if (itemDom.onclick ne null)
          Simulate.click(itemDom)
        else
          Option(itemDom.getAttribute("data-value")).filter(_.nonEmpty) match {
            case Some(value) => JQuery(dom).dropdown("set selected", value)
            case None        => throw new RuntimeException(s"Don't know how to select item in:\n\n${dom.outerHTML}")
          }
      } else if (itemDoms.nonEmpty)
        throw new RuntimeException(itemDoms.map(_.innerText).mkString("Multiple candidates: ", ", ", "."))
      else if (items.isEmpty)
        throw new RuntimeException(s"No items found. DOM =\n${dom.outerHTML}")
      else
        throw new RuntimeException(items.map(_.text).mkString("Item not found. Found = ", ", ", "."))
    }
  }

  object Dropdown {
    final class TestDsl[R, O, S](val * : Dsl[Id, R, O, S, String], name: String)(getObs: O => Dropdown) {
      protected implicit def autoObs(o: O): Dropdown = getObs(o)
      val text                     = *.focus(s"$name: text").value(_.obs.text)
      val selected                 = *.focus(s"$name: selected").option(_.obs.selected)
      val items                    = *.focus(s"$name: items").collection(_.obs.items.map(_.text))
      def select(itemName: String) = *.action(s"Select $name ${itemName.quote}")(_.obs.select(itemName))
    }
  }

  final class DropdownItem($: DomZipperJs) {
    val dom  = $.domAsHtml
    val text = dom.innerText.trim
  }

//  object DropdownItem {
//    final class TestDsl[R, O, S](val * : Dsl[Id, R, O, S, String], name: String)(getObs: O => DropdownItem) {
//      protected implicit def autoObs(o: O): DropdownItem = getObs(o)
//      val text = *.focus(s"$name text")
//    }
//  }

  final class DropdownButton($: DomZipperJs) {
    val dropdown      = new Dropdown($(".ui.dropdown"))
    val buttonDom     = $(".ui.button").domAsHtml
    val disabled      = $.classes.contains("disabled")
    def click(): Unit = Simulate click buttonDom
  }

  object DropdownButton {
    final class TestDsl[R, O, S](val * : Dsl[Id, R, O, S, String], name: String)(getObs: O => DropdownButton) {
      protected implicit def autoObs(o: O): DropdownButton = getObs(o)

      val disabled = *.focus(s"$name button is disabled").value(_.obs.disabled)
      val enabled  = *.focus(s"$name button is enabled").value(!_.obs.disabled)
      val dropdown = new Dropdown.TestDsl(*, s"$name dropdown")(_.dropdown)
      val click    = *.action(s"Click $name button")(_.obs.click())
    }
  }

  // ===================================================================================================================

  final class Message($: DomZipperJs) {
    val header = $.collect01(".header").innerTexts.fold("")(_.trim)

    lazy val body = {
      var t = $(".content").innerText
      if (t.startsWith(header))
        t = t.drop(header.length)
      t.trim
    }
  }

  // ===================================================================================================================

  class Editor(val $: DomZipperJs) {
    final val dom                        = $.domAsHtml
    final val text                       = $.innerText
    final val editor                     = $.editables01.domsAsHtml
    final def editing                    = editor.isDefined
    final val editorValue                = editor.map(editableDomValue)
    final val editorValidity             = Invalid when $.exists(Selectors.editorInvalid)
    final val editorError                = $.collect01(Selectors.editorInvalid).innerTexts
    final val fullscreenButton           = $.collect01("i.icon.maximize").domsAsHtml
    final val hasEnabledFullscreenButton = fullscreenButton.isDefined
    final val isFullscreen               = $.exists(Selectors.fullscreen)
    final val isSpinning                 = $.exists(".loading")
    final val hasPreview                 = $.exists(".ui.segments")
    final val previewIsOnRight           = $.exists(Selectors.textEditorLeftPreviewRight)
    final val previewPosition            = Option.when[Position](hasPreview)(if (previewIsOnRight) Position.Right else Position.Under)

    private final val previewButtonZippers = $.collect0n(Selectors.previewButtons).zippers
    private final def previewButton(sel: String) = previewButtonZippers.find(_.exists(sel)).map(_.domAsButton)

    final lazy val previewButtons = Editor.PreviewButtons(
      show  = previewButton(".icon.restore"),
      hide  = previewButton(".icon.close"),
      right = previewButton(".icon.right"),
      down  = previewButton(".icon.down"))

    final lazy val previewButtonExistence =
      previewButtons.map(_.isDefined)
  }

  object Editor {
    def apply($: DomZipperJs): Editor =
      new Editor($)

    final case class PreviewButtons[+A](show : A,
                                        hide : A,
                                        down : A,
                                        right: A) {

      def map[B](f: A => B): PreviewButtons[B] =
        PreviewButtons(
          show = f(show),
          hide = f(hide),
          down = f(down),
          right = f(right),
        )

      def asStr(implicit ev: A <:< Boolean): String = {
        var s = ""
        s += (if (show)  "s" else "-")
        s += (if (hide)  "h" else "-")
        s += (if (down)  "d" else "-")
        s += (if (right) "r" else "-")
        s
      }
    }

    object PreviewButtons {
      implicit def univEq[A: UnivEq]: UnivEq[PreviewButtons[A]] = UnivEq.derive
    }

    class TestDsl[R, O, S](final val * : Dsl[Id, R, O, S, String], val field: String)(getObs: O => Editor) {
      protected implicit def autoObs(o: O): Editor = getObs(o)

      final val cellDom                    = *.focus(field + " cell dom"                            ).value(_.obs.dom)
      final val text                       = *.focus(field + " editor text"                         ).value(_.obs.text)
      final val editing                    = *.focus(field + " editing"                             ).value(_.obs.editing)
      final val editorDom                  = *.focus(field + " editor dom"                          ).option(_.obs.editor)
      final val editorValue                = *.focus(field + " editor value"                        ).option(_.obs.editorValue)
      final val editorError                = *.focus(field + " editor error"                        ).option(_.obs.editorError)
      final val editorValidity             = *.focus(field + " editor validity"                     ).value(_.obs.editorValidity)
      final val hasEnabledFullscreenButton = *.focus(field + " editor has enabled fullscreen button").value(_.obs.hasEnabledFullscreenButton)
      final val isFullscreen               = *.focus(field + " editor is fullscreen"                ).value(_.obs.isFullscreen)
      final val isSpinning                 = *.focus(field + " editor is spinning"                  ).value(_.obs.isSpinning)
      final val hasPreview                 = *.focus(field + " editor has preview"                  ).value(_.obs.hasPreview)
      final val previewPosition            = *.focus(field + " editor preview position"             ).option(_.obs.previewPosition)
      final val previewButtons             = *.focus(field + " editor preview buttons"              ).value(_.obs.previewButtonExistence)
      final val previewButtonsStr          = *.focus(field + " editor preview buttons"              ).value(_.obs.previewButtonExistence.asStr)
    //final val editorSelectionRange       = *.focus(field + " editor selection range"              ).option(_.obs.editor.map(_.domCast[html.TextArea].selectionRange))
    // ^^ doesn't work with PhantomJS

      final def focusCell =
        *.action("Focus cell")(_.obs.dom.focus())

      final def focusEditor =
        *.action("Focus editor")(_.obs.editor.get.focus())

      final def setEditorSelectionRange(start: Int, end: Int) =
        *.action(s"setEditorSelectionRange($start, $end)")(_.obs.editor.get.domCast[html.TextArea].setSelectionRange(start, end))

      final def doubleClick: *.Actions =
        *.action("Double-click " + field)(Simulate doubleClick _.obs.dom)

      final def setEditorValue(value: String): *.Actions =
        *.action(s"Set $field editor to ${value.quote}")(
          SimEvent.Change(value) simulate _.obs.editor.get)

      final def pressKeyInEditor(k: SimEvent.Keyboard): *.Actions =
        *.action(s"Press $k in $field editor")(
          k.simulationKeyDownPressUp run _.obs.editor.get)

      final def commit: *.Actions =
        *.action(s"Commit $field editor")(KB.Enter.ctrl simulateKeyDown _.obs.editor.get) +>
          editing.assert.beforeAndAfter(true, false)

      final def abort: *.Actions =
        *.action(s"Abort $field editor")(KB.Escape simulateKeyDown _.obs.editor.get)

      final def set(newValue: String): *.Actions =
        (doubleClick >> setEditorValue(newValue) >> commit).group(s"Set $field field to '$newValue'")

      final def change(fromTo: (String, String)): *.Actions =
        (doubleClick
          +> editorValue.assert.some(fromTo._1)
          >> setEditorValue(fromTo._2)
          >> commit
          ).group(s"Change $field field from '${fromTo._1}' to '${fromTo._2}'")

      final def change(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
        (text.assert(textFromTo._1)
          +> doubleClick
          +> editorValue.assert.some(editorFromTo._1)
          >> setEditorValue(editorFromTo._2)
          >> commit
          +> text.assert(textFromTo._2)
          ).group(s"Change $field field from '${textFromTo._1}' to '${textFromTo._2}'")

      final def changeToAndBack(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
        change(editorFromTo, textFromTo) >> change(editorFromTo.swap, textFromTo.swap)

      final def changeToAndBack(fromTo: (String, String)): *.Actions =
        changeToAndBack(fromTo, fromTo)

      final def unfocusEditor: *.Actions =
        *.action("Unfocus editor in " + field)(Simulate blur _.obs.editor.get)

      final def toggleFullscreen: *.Actions =
        *.action("Toggle fullscreen editing in " + field)(Simulate click _.obs.fullscreenButton.get)

      final def clickPreviewShow: *.Actions =
        *.action(s"Click $field preview show")(Simulate click _.obs.previewButtons.show.get)

      final def clickPreviewHide: *.Actions =
        *.action(s"Click $field preview hide")(Simulate click _.obs.previewButtons.hide.get)

      final def clickPreviewDown: *.Actions =
        *.action(s"Click $field preview down")(Simulate click _.obs.previewButtons.down.get)

      final def clickPreviewRight: *.Actions =
        *.action(s"Click $field preview right")(Simulate click _.obs.previewButtons.right.get)

      final def modifyEditorValue(mod: String => String, desc: String = "Modify editor value") =
        *.chooseAction(desc + ".")(i => {
          val value1 = editorValue.run(i).get
          val value2 = mod(value1)
          setEditorValue(value2)
        })

      final def testValid  (text: String) = setEditorValue(text).rename(s"Enter valid value: ${text.quote}")   +> editorValidity.assert(Valid)
      final def testInvalid(text: String) = setEditorValue(text).rename(s"Enter invalid value: ${text.quote}") +> editorValidity.assert(Invalid)

      final def debugPrintInnerHTML: *.Actions =
        *.action("debugPrintInnerHTML")(x => println(s"\n${x.obs.dom.innerHTML}\n"))

      final def debugPrintOuterHTML: *.Actions =
        *.action("debugPrintOuterHTML")(x => println(s"\n${x.obs.dom.outerHTML}\n"))
    }
  }
}
