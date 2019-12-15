package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.text.Grammar.texTag
import HelpModal._

object RichTextEditorHelp {

  val modal = HelpModal("Rich Text Editor Help", Groups(

    Group("References")(
      Example("Requirements can be referenced by putting the ID inside square brackets.")("[FR-3]"),
      Example("Use case steps can be referenced by putting the use case ID and step number, inside square brackets.")("[UC-2.0.3.a]"),
      Example("Codes can be referenced by putting the code inside square brackets.")("[backend.backup.times]")),

    Group("Issues & Tags")(
      Example("Issues can be declared in text by typing a hash (", code("#"), ") followed by the issue.")("#TBD"),
      Example("Tags can be declared in text by typing a hash (", code("#"), ") followed by the tag.")("#uat")),

    Group("Multiline")(
      Example(
        "A list of bullet points can be created by starting new lines with an asterisk (", code("*"), ") followed by a space.")(
        "* item 1", "* item 2")),

    Group("Other")(
      Example("URLs are detected automatically, and presented as links.")("https://google.com"),
      Example("Emails are detected automatically, and presented as links.")("bob.loblaw@ad-law-firm.com"),
      Example(
          "Mathematical expressions can be entered in TeX format, by surrounding in ", <.br, code(s"<$texTag>…</$texTag>"), ".",
          <.br, <.br,
          "For more detail, see ",
          <.a(^.target := "_blank", ^.href := "https://khan.github.io/KaTeX/", "KaTeX"), " or ",
          <.a(^.target := "_blank", ^.href := "http://utensil-site.github.io/available-in-katex/", "Symbols and Functions in KaTeX"),
          ".")(
        s"<$texTag>{1 \\over n} + x^2</$texTag>"))))
}
