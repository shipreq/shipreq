package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import HelpModal._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.base.issue.IssueCategory

object FilterHelp {

  private def issueCatBadData     = FilterAst.issueCategoryToStr(IssueCategory.BadData)
  private def issueCatMissingData = FilterAst.issueCategoryToStr(IssueCategory.MissingData)
  private def issueCatFutility    = FilterAst.issueCategoryToStr(IssueCategory.Futility)
  private def issueCatUserDef     = FilterAst.issueCategoryToStr(IssueCategory.UserDefined)

  private val issueCatLIs =
    IssueCategory.values
      .sortBy(FilterAst.issueCategoryToStr)
      .iterator
      .toTagMod(c =>
        <.li(
          code(FilterAst.issueCategoryToStr(c)),
          " for issues of type: ", UiText.Issues.category(c).toLowerCase()))

  val modal = HelpModal("Filter Help", Groups(

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Text")(

      Example(
        "To find requirements that contain a certain word somewhere in its text, just type the word you're looking for." +
          " This is case-insensitive, so it doesn't matter whether text is upper-case or lower-case."
      )("dividend"),

      Example(
        "To search for a phrase or sequence of words, surround the search phrase with one of the following:",
        <.ol(
          <.li("quotation marks (", code("\""), ")"),
          <.li("single quotation marks (", code("'"), ")"),
          <.li("backticks (", code("`"), ")")),
        "As above, this is case-insensitive, so it doesn't matter whether text is upper-case or lower-case."
      )("\"next day\"", "'next day'", "`next day`"),

      Example(
        "Advanced users can also use ",
        <.a(^.href := "http://www.regular-expressions.info/", "regular expressions"),
        " to filter requirement text. Unlike above, this is case-sensitive unless you specify the case-insensitivity flag: ",
        code("(?i)")
      )("/.*next[ -]*day.*/", "/(?i).*next[ -]*day.*/"),

    ),

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Requirements & Types")(

      Example(
        "To filter by the type of requirements, enter the type's mnemonic (in upper-case)."
      )("FR", "UC"),

      Example(
        "To select a specific requirement, enter its ID (in upper-case).",
        <.br,
        <.br, "Note: If you want to specify a requirements ID as text, either use lower-case or wrap it as a phrase.",
      )("FR-30", "UC-6"),

      Example(
        "To specify a number of specific requirements of the same type, surround the numbers with braces ", code("{…}"),
        " and separate by commas. You can also use a dash for an inclusive range."
      )("FR-{1,3,5,7}", "FR-{10-20}"),

    ),

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Issues & Tags")(

      Example(
        "To find requirements that have a certain user-defined issue or tag, enter ", code("#"),
        " followed by the issue or tag."
      )("#draft"),

      Example(
        "To find requirements that simply have any tag, or have any kind of issue, enter ", code("has:"),
        " followed by either:",
        <.ul(
          <.li(code("issue")),
          <.li(code("tag"))),
        // "As above, this is case-insensitive, so it doesn't matter whether text is upper-case or lower-case."
      )("has:issue", "has:tag"),

      Example(
        "You can also search for requirements that have specific types of issues using ", code("has:issue:"),
        " and then some of the following separated by a comma (with no spaces in between):",
        <.ul(issueCatLIs),
        <.br,
        "Similarly by using ", code("has:issue:-"), " you can find requirements that have issues ", <.em("other than"),
        " the types specified."
      )(
        s"has:issue:$issueCatBadData",
        s"has:issue:$issueCatBadData,$issueCatUserDef",
        s"has:issue:-$issueCatMissingData",
      ),

    ),

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Implication")(

      Example(
        "To filter requirements by their implications, start with ", code("implies:"), " or ", code("impliedBy:"),
        ", then enter a list of requirements seperated by a comma (with no spaces in between)."
      )("impliedBy:MF-3", "implies:CO-3,FR-20"),

      Example(
        "To specify a type of subject (eg. show me everything implied by BR business rules), just enter the req-type."
      )("impliedBy:BR"),

      Example(
        "To specify a number of subjects of the same type, surround the numbers with braces ", code("{…}"),
        " and separate by commas. You can also use a dash for an inclusive range."
      )("implies:FR-{1,3,5,7}", "implies:FR-{10-20}"),

      Example(
        "All means of specifying subsets of requirements, can be combined by separating with commas."
      )("impliedBy:MF,FR-{1-10,20,24},CO-7"),

      Example(
        "When multiple requirements are specified, the filter matches when ", <.em("any"),
        " parts match. To require that ", <.em("all"), " parts match instead of ", <.em("any"), ", use multiple terms.",
        <.br,
        <.br, "For example, ",
        code("impliedBy:MF-{1,2}"), " will match anything implied by either MF-1 or MF-2 where as ",
        code("impliedBy:MF-1 impliedBy:MF-2"), " will match anything implied by ", <.em("both"), " MF-1 and MF-2."
      )("impliedBy:MF-1 impliedBy:MF-2"),

    ),

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Negation & Multiple Filters")(

      Example(
        "To negate a clause, prefix it with a minus ", code("-"), ".", <.br,
        "This works for anything, including text searches."
      )("-dividend", "-'next day'", "-#draft", "-has:issue", "-impliedBy:MF", "-(#v1.0 #released)"),

      Example(
        "To combine multiple filters so that they ", <.em("all"),
        " must match, separate them by a space. Optionally, you can wrap them in parenthesis ", code("(…)"),
        " and treat it as a single filter (which allows you to do things like negate the whole thing)."
      )("#v1.0 #released", "(#v1.0 #released)"),

      Example(
        "You can also combine multiple filters so that if ", <.em("any"), " match, the whole filter matches.", <.br,
        "To do so, separate the filters by pipes ", code("|"), ".", <.br,
        "For example, ", code("#v1.0 | #v1.1"), " will match:",
        <.ul(
          <.li("requirements tagged with v1.0"),
          <.li("requirements tagged with v1.1"),
          <.li("requirements tagged with v1.0 and v1.1")),
          "Like above, you can also wrap them in parenthesis ", code("(…)"),
          " and treat it as a single filter (which allows you to do things like negate the whole thing).",
      )("MF | FR | UC", "has:issue | #bug", "(#v1.0 | #v1.1)"),

    ),

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Group("Examples")(

      Example(
        "This example selects all requirements that",
        <.ul(
          <.li("have a ", code("#released"), " tag, and…"),
          <.li("are either a functional requirement (FR) or a use case (UC), and…"),
          <.li("do not have both ", code("#business_ok"), " and ", code("#ops_ok"), " tags")),
        "In more natural language:", <.br,
        <.em("all functional requirements and use cases released without business and/or ops acceptance.")
      )("#released (FR | UC) -(#business_ok #ops_ok)"),

    )
  ))
}
