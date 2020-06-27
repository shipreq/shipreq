package shipreq.webapp.base.test

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.feature.PreviewFeature.Position
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.JQuery

object CommonObs {

  private object Selectors {
    val previewButtons = BaseStyles.previewToggleWrapper2.selector + " button"
    val fullscreen     = BaseStyles.fullscreen.selector
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
      } else
        throw new RuntimeException(itemDoms.map(_.innerText).mkString("Multiple candidates: ", ", ", ""))
    }
  }

  final class DropdownItem($: DomZipperJs) {
    val dom  = $.domAsHtml
    val text = dom.innerText.trim
  }

  final class DropdownButton($: DomZipperJs) {
    val dropdown      = new Dropdown($(".ui.dropdown"))
    val buttonDom     = $(".ui.button").domAsHtml
    def click(): Unit = Simulate click buttonDom
  }

  // ===================================================================================================================

  final class Message($: DomZipperJs) {
    val header = $(".header").innerText.trim

    lazy val body = {
      var t = $(".content").innerText
      if (t.startsWith(header))
        t = t.drop(header.length)
      t.trim
    }
  }

  // ===================================================================================================================

  final case class Editor($: DomZipperJs) {
    val dom                        = $.dom
    val text                       = $.innerText
    val editor                     = $.editables01.domsAsHtml
    val editorValue                = editor.map(editableDomValue)
    val fullscreenButton           = $.collect01("i.icon.maximize").domsAsHtml
    val hasEnabledFullscreenButton = fullscreenButton.isDefined
    val isFullscreen               = $.exists(Selectors.fullscreen)
    val isSpinning                 = $.exists(".loading")
    val hasPreview                 = $.exists(".ui.segments")
    val previewIsOnRight           = $.exists(Selector.textEditorLeftPreviewRight)
    val previewPosition            = Option.when[Position](hasPreview)(if (previewIsOnRight) Position.Right else Position.Under)

    private val previewButtonZippers = $.collect0n(Selectors.previewButtons).zippers
    private def previewButton(sel: String) = previewButtonZippers.find(_.exists(sel)).map(_.domAsButton)

    lazy val previewButtons = Editor.PreviewButtons(
      show  = previewButton(".icon.restore"),
      hide  = previewButton(".icon.close"),
      right = previewButton(".icon.right"),
      down  = previewButton(".icon.down"))

    lazy val previewButtonExistence =
      previewButtons.map(_.isDefined)
  }

  object Editor {
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
    }

    object PreviewButtons {
      implicit def univEq[A: UnivEq]: UnivEq[PreviewButtons[A]] = UnivEq.derive
    }

    class TestDsl[R, O, S](final val * : Dsl[Id, R, O, S, String], field: String)(getObs: O => Editor) {
      protected implicit def autoObs(o: O): Editor = getObs(o)

      val text                       = *.focus("Editor text"                         ).value(_.obs.text)
      val hasEditor                  = *.focus("Editor exists"                       ).value(_.obs.editorValue.isDefined)
      val editorValue                = *.focus("Editor value"                        ).option(_.obs.editorValue)
      val hasEnabledFullscreenButton = *.focus("Editor has enabled fullscreen button").value(_.obs.hasEnabledFullscreenButton)
      val isFullscreen               = *.focus("Editor is fullscreen"                ).value(_.obs.isFullscreen)
      val isSpinning                 = *.focus("Editor is spinning"                  ).value(_.obs.isSpinning)
      val hasPreview                 = *.focus("Editor has preview"                  ).value(_.obs.hasPreview)
      val previewPosition            = *.focus("Editor preview position"             ).option(_.obs.previewPosition)
      val previewButtons             = *.focus("Editor preview buttons"              ).value(_.obs.previewButtonExistence)

      def doubleClick: *.Actions =
        *.action("Double-click " + field)(Simulate doubleClick _.obs.dom)

      def setEditorValue(value: String): *.Actions =
        *.action(s"Set $field editor to ${quoteString(value)}")(
          SimEvent.Change(value) simulate _.obs.editor.get)

      def commitEditor: *.Actions =
        *.action(s"Commit $field editor")(KB.Enter.ctrl simulateKeyDown _.obs.editor.get) +>
          hasEditor.assert.beforeAndAfter(true, false)

      def abortEditor: *.Actions =
        *.action(s"Abort $field editor")(KB.Escape simulateKeyDown _.obs.editor.get) +>
          hasEditor.assert.beforeAndAfter(true, false)

      def change(fromTo: (String, String)): *.Actions =
        (doubleClick
          +> editorValue.assert.contains(fromTo._1)
          >> setEditorValue(fromTo._2)
          >> commitEditor
          ).group(s"Change $field field from '${fromTo._1}' to '${fromTo._2}'")

      def change(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
        (text.assert(textFromTo._1)
          +> doubleClick
          +> editorValue.assert.contains(editorFromTo._1)
          >> setEditorValue(editorFromTo._2)
          >> commitEditor
          +> text.assert(textFromTo._2)
          ).group(s"Change $field field from '${textFromTo._1}' to '${textFromTo._2}'")

      def changeToAndBack(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
        change(editorFromTo, textFromTo) >> change(editorFromTo.swap, textFromTo.swap)

      def toggleFullscreen: *.Actions =
        *.action("Toggle fullscreen editing in " + field)(Simulate click _.obs.fullscreenButton.get)

      def clickPreviewShow: *.Actions =
        *.action(s"Click $field preview show")(Simulate click _.obs.previewButtons.show.get)

      def clickPreviewHide: *.Actions =
        *.action(s"Click $field preview hide")(Simulate click _.obs.previewButtons.hide.get)

      def clickPreviewDown: *.Actions =
        *.action(s"Click $field preview down")(Simulate click _.obs.previewButtons.down.get)

      def clickPreviewRight: *.Actions =
        *.action(s"Click $field preview right")(Simulate click _.obs.previewButtons.right.get)
    }
  }
}
