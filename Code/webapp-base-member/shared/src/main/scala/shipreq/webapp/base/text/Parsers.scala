package shipreq.webapp.base.text

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.parboiled2.{CharPredicate => CP, _}
import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.ArraySeq
import scalaz.{-\/, \/-}
import shapeless._
import shipreq.base.util.NonEmptyArraySeq
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.UseCaseStepLabelLookup
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.util._

@nowarn("msg=Auto-application.*deprecated")
object Parsers {

  def fixOptionalText[T <: Atom.Base](text: T#OptionalText): T#OptionalText =
    if (text.isEmpty)
      text
    else
      fixNonEmptyText(NonEmptyArraySeq.force(text)).whole

  def fixNonEmptyText[T <: Atom.Base](text: T#NonEmptyText): T#NonEmptyText = {
    val last: Atom.AnyAtom = text.last
    val newLast: Atom.AnyAtom =
      last match {
        case a: Atom.Literal         # Literal               =>
          val a2 = a.modText(TextMod.noWhitespaceRight.run)
          if (a2.value.isEmpty)
            null
          else
            a2
        case a: Atom.Headings        # Heading               => a.modTitle(fixNonEmptyText(_))
        case a: Atom.PlainTextMarkup # PlainTextMarkupStyled => a.unsafeWithInner(fixNonEmptyText(a.inner))
        case a: Atom.ListMarkup      # ListBase              => a.map(fixOptionalText(_))
        case _: Atom.CodeBlock       # CodeBlock
           | _: Atom.ContentRef      # CodeRef
           | _: Atom.ContentRef      # ReqRef
           | _: Atom.ContentRef      # UseCaseStepRef
           | _: Atom.Issue           # Issue
           | _: Atom.NewLine         # BlankLine
           | _: Atom.PlainTextMarkup # EmailAddress
           | _: Atom.PlainTextMarkup # Monospace
           | _: Atom.PlainTextMarkup # TeX
           | _: Atom.PlainTextMarkup # WebAddress
           | _: Atom.TagRef          # TagRef                => last
      }
    if (newLast eq null)
      NonEmptyArraySeq.maybe(text.whole.dropRight(1), text)(identity)
    else if (newLast eq last)
      text
    else
      text.updated(text.length - 1, newLast.asInstanceOf[T#Atom])
  }

  // Because there are special cases, not all whitespace is trimmed.
  // Not all whitespace need be trimmed because the parser already contains space handing - for example, literals are
  // trimmed as they're parsed, as confirmed by tests.
  //
  // Special cases:
  // 1) "* " is a valid multiline bullet with no content. "*" is not.
  // 2) "1. " is a valid multiline leader with no content. "1." is not.
  private val multiLineCanTrim: PreProcessor.CanTrim =
    (a, i) => a(i) match {
      case ' ' =>
        (i == 0) || {
          a(i - 1) match {
            case '*' => false
            case '.' => !(i >= 2 && a(i - 2).isDigit)
            case _   => true
          }
        }
      case c =>
        PreProcessor.CanTrim.whitespaceFn(c)
    }

  private val preProcessorMultiLine =
    PreProcessor(PreProcessor.FixChar.multiLine, multiLineCanTrim)

  val preProcessor: LineCardinality => String => PreProcessed =
    LineCardinality.memo {
      case MultiLine  => preProcessorMultiLine
      case SingleLine => PreProcessor.singleLine
    }

  // questionable: :;=?\/*
  val emailCharArray = """!$%+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~""".toCharArray
  val emailCharL = CP(emailCharArray)
  val emailCharR = emailCharL -- '.'

  val webAddressChar = CP.Visible -- ('{' :: '}' :: '[' :: ']' :: '<' :: '>' :: '`' :: Nil)

  private val useCaseStepTailChar = CP.AlphaNum ++ ' ' ++ '.'

  sealed abstract class StyleType(markdown: String) {
    final val prefix = markdown
    final val suffix = markdown
  }

  object StyleType {
    case object Bold          extends StyleType("**")
    case object Italic        extends StyleType("//")
    case object Strikethrough extends StyleType("~~")
    case object Underline     extends StyleType("__")

    def of(t: Atom.PlainTextMarkup # PlainTextMarkupStyled): StyleType =
      t match {
        case _: Atom.PlainTextMarkup # Bold          => Bold
        case _: Atom.PlainTextMarkup # Italic        => Italic
        case _: Atom.PlainTextMarkup # Strikethrough => Strikethrough
        case _: Atom.PlainTextMarkup # Underline     => Underline
      }

    lazy val values = AdtMacros.adtValues[StyleType]

    val isPossibleStart: String => Boolean =
      s => s.length >= 1 && (s.head match {
        case '*' | '/' | '~' | '_' => true // single char because it might be __ but the grammar limit stops after the first
        case _                     => false
      })
  }

  final case class StyleCtx(parentsOldestFirst: NonEmptyVector[StyleType]) {

    def latest: StyleType =
      parentsOldestFirst.last

    def begin(s: StyleType): StyleCtx =
      StyleCtx(parentsOldestFirst :+ s)

    def allow(s: StyleType): Boolean =
      !parentsOldestFirst.whole.contains(s)
  }

  object StyleCtx {
    def begin(s: StyleType): StyleCtx =
      apply(NonEmptyVector.one(s))
  }

  abstract class Base extends ParsingUtil {
    val t: Atom.Base
    val project: Project

    /** Optional whitespace */
    def OWS: Rule0 =
      rule(zeroOrMore(' '))

    /** Optional whitespace and/or newlines */
    def OWSNL: Rule0 =
      rule(anyOf(" \r\n").*)

    /** Wwhitespace and/or newlines */
    def WSNL: Rule0 =
      rule(anyOf(" \r\n").+)

    private val isWS: Char => Boolean =
      _ == ' '

    private val isNL: Char => Boolean = {
      case '\n' | '\r' => true
      case _           => false
    }

    @tailrec private def isStartOfLineAfterOWS(i: Int): Boolean =
      if (i < 0)
        true
      else {
        val c = input.charAt(i)
        if (isNL(c))
          true
        else if (isWS(c))
          isStartOfLineAfterOWS(i - 1)
        else
          false
      }

    def startOfLine: Rule0 =
      rule(BOI | test(isNL(lastChar)))

    def startOfLineAfterOWS: Rule0 =
      rule(BOI | test(isStartOfLineAfterOWS(cursor - 1)))

    val untilEOL = () => rule(OWS ~ EOL)

    val lookupReq: (ReqType.Mnemonic, ReqTypePos) => Option[ReqId] =
      (m, n) =>
        project.config.reqTypes.allByMnemonic.get(m)
          .map(t => PubidT(t.reqTypeId, n))
          .flatMap(project.content.reqs.pubids.apply)

    def hashRef: Rule1[HashRefTarget] =
      rule(hashRefStr(
        possibleStop = StyleType.isPossibleStart,
        parse        = project.config.hashRefLookup))
  }

  private val innerSpaceRegex = "([^ ]) {2,}([^ ])".r

  def fixLiteralWhiteSpace(input: String): String = {
    var s = input
    while({
      val t = innerSpaceRegex.replaceFirstIn(s, "$1 $2")
      val changed = s != t
      s = t
      changed
    }) ()
    if (s.startsWith("  "))
      s = " " + s.dropWhile(_ == ' ')
    while(s.endsWith("  ")) {
      s = s.dropRight(1)
    }
    s
  }

  // ===================================================================================================================
  // Modules

  trait Literal extends Base {
    override val t: Atom.Literal

    final type TokenRule = () => Rule1[t.Atom]

    protected val atomsToArraySeq: Seq[t.Atom] => ArraySeq[t.Atom] = input => {
      var v = ArraySeq.empty[t.Atom]
      var lastIsBlank = false

      // Here we ensure that we don't end up with blank lines next to things that don't allow them around themselves.
      // A lot of this is done by the parsing rules but in the case of blank lines around top-level code blocks,
      // it was too hard and would require too much fundamental change to all the parsers. It's done here instead now.
      for (a <- input) {
        if (a.isBlankLine) {
          if (v.isEmpty || v.last.allowBlankLineAfter) {
            v :+= a
            lastIsBlank = true
          }
        } else {
          if (!a.allowBlankLineBefore && lastIsBlank)
            v = v.dropRight(1) :+ a
          else
            v :+= a
          lastIsBlank = false
        }
      }
      v
    }

    def literalUntil[O <: HList](stop: () => Rule[HNil, O]): Rule1[t.Literal] =
      rule(capture(oneOrMore( !stop() ~ ANY )) ~> ((l: String) => t.Literal(fixLiteralWhiteSpace(l))))

    def textUntil(token: TokenRule, end: () => Rule0): Rule1[t.OptionalText] = {
      val endOrToken = () => rule(end() | token())
      rule(zeroOrMore(token() | literalUntil(endOrToken)) ~ end() ~> atomsToArraySeq)
    }

    def textUntilEOL(token: TokenRule): Rule1[t.OptionalText] =
      textUntil(token, untilEOL)
  }

  trait PlainTextMarkup extends Base {
    override val t: Atom.PlainTextMarkup

    // Hack due to https://github.com/sirthias/parboiled2/issues/120
    // runSubParser can only be used in a method directly in a class, not a trait like this
    protected def styledInner(s: StyleType): Rule1[t.styled.NonEmptyText]

    def webScheme = rule( (("http" | "ftp") ~ 's'.?) | "sftp" )

    // TODO ensure webAddress and emailAddress don't follow literal

    protected def stopPlainMarkupAt: Rule0 =
      rule(test(false))

    def webAddress =
      rule(capture(webScheme ~ "://" ~ (!stopPlainMarkupAt ~ webAddressChar).+) ~> t.WebAddress)

    def emailAddress =
      rule("mailto:".? ~ capture(
        (!stopPlainMarkupAt ~ emailCharL).+ ~
          '@' ~
          ((!stopPlainMarkupAt ~ emailCharR).+ ~ '.').+ ~
          (!stopPlainMarkupAt ~ emailCharR).+
      ) ~> t.EmailAddress)

    def tex =
      rule(surround(G.texSurround) ~> (_.trim |> t.TeX))

    def monospace =
      rule('`' ~ capture(oneOrMore(!('`' | NL | EOI) ~ ANY)) ~ '`' ~> t.Monospace)

    @nowarn("cat=unused")
    protected def styleCheck(s: StyleType): Rule0 =
      rule(test(true))

    def style(s: StyleType): Rule1[t.Atom] =
      rule(styleCheck(s) ~ s.prefix ~ !WSNL ~ styledInner(s) ~> ((i: t.styled.NonEmptyText) => makeStyle(s, i)))

    private def makeStyle(s: StyleType, i: t.styled.NonEmptyText): t.Atom =
      s match {
        case StyleType.Bold          => t.Bold         (i)
        case StyleType.Italic        => t.Italic       (i)
        case StyleType.Strikethrough => t.Strikethrough(i)
        case StyleType.Underline     => t.Underline    (i)
      }

    def styles: Rule1[t.Atom] =
      rule(
        style(StyleType.Bold) |
        style(StyleType.Italic) |
        style(StyleType.Strikethrough) |
        style(StyleType.Underline)
      )

    def plainTextMarkup =
      rule(styles | tex | webAddress | emailAddress | monospace)
  }

  trait StyledInner extends TopBase with SingleLine {
    val ctx: StyleCtx

    // unused
    override final protected val token: TokenRule = () => null

    protected def additionalTokens: Rule1[t.Atom]

    override protected def styleCheck(s: StyleType): Rule0 =
      rule(test(ctx.allow(s)))

    override protected def stopPlainMarkupAt: Rule0 =
      rule(ctx.latest.suffix)

    private def stopLiteral =
      rule(ctx.latest.suffix | NL | EOI | additionalTokens | plainTextMarkup)

    private def literal =
      rule(literalUntil(() => stopLiteral))

    private def innerToken: Rule1[t.Atom] =
      rule(additionalTokens | plainTextMarkup | literal)

    final def inline: Rule1[t.NonEmptyText] =
      rule(oneOrMore(innerToken) ~ ctx.latest.suffix ~ popSeqToNEA)
  }

  trait NewLine extends Base {
    override val t: Atom.NewLine
    def blankLine = rule(OWS ~ NL ~ OWSNL ~ push(t.blankLine))
  }

  def processCodeBlockCode(code: String): String =
    code
      .linesWithSeparators
      .map(_.replaceFirst("[ \r\n]+$", "")) // right-trim all lines
      .dropWhile(_.isEmpty)                 // remove leading blank lines
      .toArray
      .reverseIterator
      .dropWhile(_.isEmpty)                // remove trailing blank lines
      .toArray
      .reverseIterator
      .mkString("\n")

  trait CodeBlock extends Literal {
    override val t: Atom.CodeBlock with Atom.Literal

    private val codeBlockEnd = () => rule(
      NL ~ OWS ~ "```" ~ &(OWS ~ (NL | EOI))
    )

    def codeBlock: Rule1[t.CodeBlock] =
      rule(
        "```"
          ~ indentationLevelSoFar(3)
          ~ OWS
          ~ capture(CP.Visible.+).?
          ~ OWS
          ~ &(NL)
          ~ nonGreedyCapture0(codeBlockEnd)
          ~ indentationLevelSoFar(3)
          ~ OWS
          ~> buildCodeBlock
      )

    private val buildCodeBlock: (Int, Option[String], String, Int) => t.CodeBlock =
      (startIndent, lang, codeTxt, endIndent) => {
        val indent = startIndent min endIndent
        val code = processCodeBlockCode(codeTxt.unindent(indent))
        t.CodeBlock(lang, code)
      }
  }

  trait ListMarkup extends Literal with CodeBlock {
    override val t: Atom.ListMarkup with Atom.Literal with Atom.NewLine with Atom.CodeBlock

    private def firstLineCodeBlock =
      rule(codeBlock ~> ((x: t.CodeBlock) => ArraySeq(x)))

    def listItem(lead: () => Rule0, listToken: TokenRule): Rule1[t.ListItem] = {
      val tailLines: TokenRule = () => rule(codeBlock | listToken())
      rule(
        OWSNL
          ~ lead() ~ OWS
          ~ (firstLineCodeBlock | textUntil(listToken, untilEOL))
          ~ extraLine(lead, tailLines).*
          ~> combineListItemLines
      )
    }

    private val combineListItemLines: (ArraySeq[t.Atom], Seq[ArraySeq[t.Atom]]) => t.ListItem = (head, tail) => {
      tail.iterator.filter(_.nonEmpty).foldLeft(head) { (q, n) =>
        if (q.lastOption.exists(_.allowBlankLineAfter) && n.headOption.exists(_.allowBlankLineBefore))
          (q :+ t.blankLine) ++ n
        else
          q ++ n
      }
    }

    private def extraLine(lead: () => Rule0, listToken: TokenRule): Rule1[ArraySeq[t.Atom]] =
      rule(
        (NL ~ extraLine(lead, listToken)) |
        (' ' ~ OWS ~ !lead() ~ textUntil(listToken, untilEOL))
      )

    private def genericList(lead: () => Rule0, listToken: TokenRule): Rule1[NonEmptyArraySeq[t.ListItem]] =
      rule(startOfLineAfterOWS ~ listItem(lead, listToken).+ ~ OWSNL ~ popSeqToNEA[t.ListItem])

    private def orderedList(listToken: TokenRule): Rule1[t.OrderedList] = {
      def lead: Rule0 = rule(CP.Digit.+ ~ ". ")
      rule(genericList(() => lead, listToken) ~> t.OrderedList)
    }

    private def unorderedList(listToken: TokenRule): Rule1[t.UnorderedList] = {
      // See https://en.wikipedia.org/wiki/Bullet_(typography)
      def bullet: Rule0 = rule("* " | anyOf("•‣⁃⁌⁍∙○◘◦☙❥❧⦾⦿"))
      rule(genericList(() => bullet, listToken) ~> t.UnorderedList)
    }

    def listMarkup(listToken: TokenRule): Rule1[t.ListBase] =
      rule(OWSNL ~ (orderedList(listToken) | unorderedList(listToken)))
  }

  trait ContentRef extends Base with UseCaseStepLabel {
    override val t: Atom.ContentRef

    import G.reflinkSurround.parsing.{prefix, suffix}
    import ReqCode._

    def reqRef: Rule1[t.ReqRef] = rule(
      prefix ~ OWS ~ reqTypeMnemonicCI ~ OWS ~ ('-' ~ OWS).? ~ reqTypePos ~ OWS ~ suffix
        ~> lookupReq ~ popOptional[ReqId] ~> t.ReqRef)

    def reqCodeNode: Rule1[Node] = rule(
      capture(grammarStr(G.reqCode)(_.firstChar, _.tailChars, None, _.nodeLength)) ~> Node.applyFn)

    // Could be optimised to lookup each node as parsed and fail early
    def codeRef: Rule1[t.CodeRef] = rule(
      prefix ~ oneOrMore(OWS ~ reqCodeNode).separatedBy(OWS ~ G.reqCode.nodeSeparator) ~ OWS ~ suffix
        ~> lookupCode ~ popOptional[ReqCodeId] ~> t.CodeRef)

    val lookupCode: Seq[Node] => Option[ReqCodeId] = ss =>
      NonEmptyVector.maybe(ss.toVector, None: Option[ReqCodeId])(code =>
        project.content.reqCodes.get(code).flatMap(_.activeId))

    override def useCaseStepLabelLookup = project.content.reqs.useCaseStepLabelLookup

    def useCaseStepRef: Rule1[t.Atom] =
      rule(prefix ~ OWS ~ useCaseStepLabel ~ suffix ~> t.UseCaseStepRef)

    def contentRef: Rule1[t.Atom] =
      rule(useCaseStepRef | codeRef | reqRef)
  }

  trait TagRef extends Base {
    override val t: Atom.TagRef

    def tagRef = popPF[HashRefTarget, t.TagRef] { case -\/(tag) => t.TagRef(tag.id) }
  }

  trait UseCaseStepLabel extends ParsingUtil {

    /** Optional whitespace */
    def OWS: Rule0

    def useCaseStepLabelLookup: UseCaseStepLabelLookup

    /** If specified, allows parsing of [.1] instead of [n.1] where n is the value specified */
    def currentUseCase: Option[ReqTypePos]

    /** Expects no leading whitespace.
      * Gobbles any trailing whitespace.
      */
    def useCaseStepLabelAttempt: Rule1[UseCaseStepLabelLookup.Result] = {

      def stepLabelText: Rule1[String] =
        rule(capture(useCaseStepTailChar.+) ~ OWS) // trailing OWS to potentially gobble up multiline WS

      def ctxFree: Rule1[UseCaseStepLabelLookup.Result] = rule(
        ((ch('U')|'u') ~ (ch('C')|'c') ~ OWS ~ ('-' ~ OWS).?).? // (UC-)?
          ~ reqTypePos ~ OWS ~ '.' ~ OWS                        // 1.
          ~ stepLabelText                                       // 0.X.1.a.ii
          ~> ((pos: ReqTypePos, tail: String) => useCaseStepLabelLookup(pos, s"${pos.value}.$tail", allowAliases = false)))

      def withCtx: Rule1[UseCaseStepLabelLookup.Result] = rule(
        pushOptional(currentUseCase)
          ~ stepLabelText
          ~> ((pos: ReqTypePos, step: String) => useCaseStepLabelLookup(pos, step, allowAliases = true)))

      rule(ctxFree | withCtx)
    }

    /** Expects no leading whitespace.
      * Gobbles any trailing whitespace.
      */
    def useCaseStepLabel: Rule1[UseCaseStepId] =
      rule(useCaseStepLabelAttempt ~ pop_\/-[UseCaseStepId])
  }

  trait Issue extends Base {
    override val t: Atom.Issue
    import Text.{InlineIssueDesc => I}

    def issueRef: RuleAB[HashRefTarget, t.Issue] = {
      def id           = popPF[HashRefTarget, CustomIssueTypeId] { case \/-(i) => i.id }
      def optionalDesc = rule(OWS ~ issueInnerDesc ~> (_.whole) | push(ArraySeq.empty))
      rule(run(id) ~ optionalDesc ~> t.Issue)
    }

    // Hack due to https://github.com/sirthias/parboiled2/issues/120
    // runSubParser can only be used in a method directly in a class, not a trait like this
    protected def issueInnerDesc: Rule1[I.NonEmptyText] //= rule(runSubParser(I.parserI(project)(_).inline))
  }

  trait HeadingTitle extends Literal {
    val token: TokenRule
    final def inline: Rule1[t.NonEmptyText] = rule(textUntilEOL(token) ~ popNEA)
  }

  trait Headings extends Base { self: Literal with Headings =>
    override val t: Atom.Headings

    // Hack due to https://github.com/sirthias/parboiled2/issues/120
    // runSubParser can only be used in a method directly in a class, not a trait like this
    protected def headingTitle: Rule1[t.headingTitle.NonEmptyText]

    final val heading: TokenRule =
      () => rule(
        OWSNL ~ startOfLineAfterOWS
          ~ capture(
            '#' // 1
              ~ ('#' // 2
              ~ ('#' // 3
              ~ ('#' // 4
              ~ ('#' // 5
              ~ ('#' // 6
              ).? // 6
              ).? // 5
              ).? // 4
              ).? // 3
              ).? // 2
          ) ~ ' '
          ~ OWS ~ headingTitle
          ~ OWSNL
        ~> { (hstr: String, title: t.headingTitle.NonEmptyText) =>
          val n = hstr.length - 1
          t.unsafeHeadingByIdx(n, title)
        }
      )
  }

  // ===================================================================================================================

  trait SingleLine extends PlainTextMarkup with Literal {
    override val t: Atom.SingleLine
    def singleLine = plainTextMarkup
  }

  trait MultiLine extends SingleLine with NewLine with ListMarkup with CodeBlock with Headings {
    override val t: Atom.MultiLine
    protected val additionalTokens: TokenRule

    final val listToken: TokenRule =
      () => rule(additionalTokens() | singleLine)

    final val token: TokenRule =
      () => rule(listMarkup(listToken) | heading() | codeBlock | additionalTokens() | blankLine | singleLine)
  }

  // ===================================================================================================================

  trait TopBase extends Literal {
    protected val token: TokenRule
    final def optionalText: Rule1[t.OptionalText] = rule(OWS ~ textUntilEOL(token) ~ EOI)
    final def nonEmptyText: Rule1[t.NonEmptyText] = rule(optionalText ~ popNEA)
  }
}