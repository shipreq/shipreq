package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.text.Atom.TypeGroup
import shipreq.webapp.base.text.Grammar.texTag
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.semantic.Modal

object RichTextEditorHelp {
  import HelpModal._

  private val references =
    Group("References")(
      Row(
        "Requirements can be referenced by putting the ID inside square brackets.")(
        "[FR-3]"),

      Row(
        "Use case steps can be referenced by putting the use case ID and step number, inside square brackets.")(
        "[UC-2.0.3.a]"),

      Row(
        "Codes can be referenced by putting the code inside square brackets.")(
        "[backend.backup.times]"))

  private val issues =
    Group("Issues")(
      Row(
        "Issues can be declared in text by typing a hash (", code("#"), ") followed by the issue.")(
        "#TODO"),

      Row(
        "More information may be attached to an issue by appending", <.br,
        code("{"), ", the text, and a final ", code("}"), " to the issue tag."
      )(
        "#PENDING{ Tina to come back from leave }"))

  private val tags =
    Group("Tags")(
      Row(
        "Tags can be declared in text by typing a hash (", code("#"), ") followed by the tag.")(
        "#uat"))

  private val lists =
    Group("Lists")(
      Row(
        "A list of bullet points can be created by starting new lines with an asterisk (", code("*"), ") followed by a space."
      )(
        "* item 1",
        "* item 2"),
      Row(
        "A numbered list can be created by starting new lines with a number, followed by a dot and a space.",
        <.br,
        "The numbers themselves don't matter; they'll be automatically replaced to reflect their order when you save."
      )(
        "1. item 1",
        "1. item 2",
        "1. item 3"),
    )

  private val styling =
    Group("Styling")(
      Row("To make text bold, wrap it in ", code("**"))("**this is bold**"),
      Row("To make text italic, wrap it in ", code("//"))("//this is italic//"),
      Row("To underline text, wrap it in ", code("__"))("__this is underlined__"),
      Row("To strikethrough text, wrap it in ", code("~~"))("~~this is strikethrough~~"),
    )

  private val headings = {
    def eg(n: Int) = {
      val h = "#" * n
      Row("Begin a line with ", code(h + " "), s" to create a level $n heading.")(h + " Heading level " + n)
    }
    Group("Headings")(
      eg(1), (2 to 6).iterator.map(eg).toSeq: _*
    )
  }

  private val codeBlocks =
    Group("Code blocks")(
      Row(
        "To create a block of code, wrap it between lines of ", code("```"), ".",
      )(
        <.div(
          ^.whiteSpace.pre,
          List(
            "```",
            "\\",
            "  \\",
            "    This is plain text",
            "  /",
            "/",
            "```",
          ).mkString("\n")
        )
      ),

      Row(
        "If you'd like to use syntax highlighting, specify the code language after the first ", code("```"), "."
      )(
        <.div(
          ^.whiteSpace.pre,
          """
            |```javascript
            |const Y =
            |  g => g( () => Y(g) )
            |```
            |""".stripMargin.trim
        )
      ),
    )

  private val useCaseFlow =
    Group("Use Case flow")(
      Row(
        "Flow between use case steps can be specified by adding arrows ", code("-->"), " and/or ", code("<--"),
        " to the end of a step, followed by the target use case steps.",
        <.br, <.br,
        "When specifying use case steps, you can omit the initial number representing the use case itself.",
        " Eg. if you're in UC-4, you can specify 4.1.2 or just .1.2 for short.",
        <.br, <.br,
        "Currently use case steps can only flow to other steps within the same use case, and not steps in other use cases.",
        " This may change in future."
      )(
        "--> .0.1",
        "--> 3.0.1 <-- 3.3, 3.4"))

  private val other =
    Group("Other")(
      Row(
        "To mark part of a line as monospace or code, wrap it with ", code("`"), ".",
      )("`like_this`"),

      Row(
        "URLs are detected automatically, and presented as links.")(
        "https://google.com"),

      Row(
        "Emails are detected automatically, and presented as links.")(
        "bob.loblaw@ad-law-firm.com"),

      Row(
        "Mathematical expressions can be entered in TeX format, by surrounding in ", <.br, code(s"<$texTag>…</$texTag>"), ".",
        <.br, <.br,
        "For more detail, see ",
        <.a.toNewWindow("https://khan.github.io/KaTeX/")("KaTeX"), " or ",
        <.a.toNewWindow("http://utensil-site.github.io/available-in-katex/")("Symbols and Functions in KaTeX"),
        "."
      )(
        s"<$texTag>{1 \\over n} + x^2</$texTag>"))

  private def create(t: Text.Generic): Modal = {

    def customise(group: Group, criteria: Text.Generic => Boolean): Group = {
      if (criteria(t))
        group
      else {
        val explanation = TagMod(
          "This is only supported in the following locations:",
          <.ul(
            Text.values
              .iterator
              .filter(criteria)
              .flatMap(UiText.RichText.descPlural)
              .toList
              .distinct
              .sorted
              .map(<.li(_)): _*))
        group.notApplicable(explanation)
      }
    }

    val groups = NonEmptyVector[Group](
      customise(issues,      _.supports(TypeGroup.Issue)),
      customise(lists,       _.supports(TypeGroup.ListMarkup)),
      customise(references,  _.supports(TypeGroup.ContentRef)),
      customise(tags,        _.supports(TypeGroup.TagRef)),
      customise(styling,     _.supports(TypeGroup.PlainTextMarkup)),
      customise(headings,    _.supports(TypeGroup.Headings)),
      customise(codeBlocks,  _.supports(TypeGroup.CodeBlock)),
      customise(other,       _.supports(TypeGroup.PlainTextMarkup)),
      customise(useCaseFlow, _ ==* Text.UseCaseStep),
    )

    HelpModal("Rich Text Editor Help", groups)
  }




  private val lookup: Map[Text.Generic, Modal] =
    Text.values.iterator.map(t => t -> create(t)).toMap

  val allRendered: TagMod =
    TagMod(lookup.valuesIterator.map(_.render).toList: _*)

  def modalFor(text: Text.Generic): Modal =
    lookup(text)
}
