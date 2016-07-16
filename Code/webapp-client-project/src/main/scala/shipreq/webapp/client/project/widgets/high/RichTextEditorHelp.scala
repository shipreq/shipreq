package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.client.base.ui.semantic.{Accordion, Modal}
import shipreq.webapp.client.project.app.Style.{help => *}

object RichTextEditorHelp {

  val modal = Modal("Rich Text Editor Help", content)

  private def content: ReactElement = {

    type Group = Accordion.Item

    def group(title: ReactNode)(e1: Example, en: Example*): Group = {
      val content = <.table(*.examplesTable, <.tbody(e1 +: en: _*))
      Accordion.Item(title, content)
    }

    type Example = ReactTagOf[html.TableRow]

    def example(desc: TagMod, samplePlainText: String): Example =
      <.tr(
        <.td(*.exampleDesc, desc),
        <.td(*.exampleSample, samplePlainText))

    val code = <.code(*.exampleDescCode)

    def groups: NonEmptyVector[Group] =
      NonEmptyVector(
        refs,
        issuesAndTags,
        multiline,
        other)

    def refs = group("References")(
      example("Requirements can be referenced by putting the ID inside square brackets.", "[FR-3]"),
      example("Use case steps can be referenced by putting the use case ID and step number, inside square brackets.", "[UC-2.0.3.a]"),
      example("Codes can be referenced by putting the code inside square brackets.", "[backend.backup.times]"))

    def issuesAndTags = group("Issues & Tags")(
      example(TagMod("Issues can be declared in text by typing a hash (", code("#"), ") followed by the issue."), "#TBD"),
      example(TagMod("Tags can be declared in text by typing a hash (", code("#"), ") followed by the tag."), "#uat"))

    def multiline = group("Multiline")(
      example(TagMod("A list of bullet points can be created by starting new lines with an asterisk (", code("*"), ") followed by a space."),
        "* item 1\n* item 2"))

    def other = group("Other")(
      example("URLs are detected automatically, and presented as links.", "https://google.com"),
      example("Emails are detected automatically, and presented as links.", "bob.loblaw@ad-law-firm.com"),
      example(TagMod(
        "Mathematical expressions can be entered in TeX format, by surrounding in ", code("<math>...</math>"), ".",
        <.br, <.br,
        "For more detail, see ",
        <.a(^.target := "_blank", ^.href := "https://khan.github.io/KaTeX/", "KaTeX"), " or ",
        <.a(^.target := "_blank", ^.href := "http://utensil-site.github.io/available-in-katex/", "Symbols and Functions in KaTeX"),
        "."
      ), "<math>{1 \\over n} + x^2</math>"))

    Accordion.Component(groups)
  }
}
