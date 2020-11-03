package shipreq.webapp.base.text

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.parboiled2.{CharPredicate => CP, _}
import scala.collection.immutable
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import shapeless._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.UseCaseStepLabelLookup
import shipreq.webapp.base.text.Atom.{CodeBlockDetail, DisplayReqRef}
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.util._

object Parsers {

  def fixNonEmptyText[T <: Atom.Base](text: T#NonEmptyText): T#NonEmptyText = {
    val t1 = text.whole
    val t2 = fixOptionalText(t1)
    if ((t1 ne t2) && t2.nonEmpty)
      NonEmptyArraySeq.force(t2)
    else
      text
  }

  def fixOptionalText[T <: Atom.Base](rootText: T#OptionalText): T#OptionalText = {

    var prevIsSpace = false

    var _fixNonEmpty: (Atom.Base#NonEmptyText, Boolean) => Atom.Base#NonEmptyText = null

    def fixNonEmpty[T2 <: Atom.Base](text: T2#NonEmptyText, allowTrailingWS: Boolean): T2#NonEmptyText =
      _fixNonEmpty(text, allowTrailingWS).asInstanceOf[T2#NonEmptyText]

    def fixOptional[T2 <: Atom.Base](text: T2#OptionalText, allowTrailingWS: Boolean): T2#OptionalText = {
      var result: Array[T2#Atom] = null
      var dropLast = false
      var i = 0
      val last = text.length - 1
      while (i <= last) {
        val atom = text(i): Atom.Base#Atom
        var atom2 = atom

        def allowWsBeforeNext =
          if (i == last)
            allowTrailingWS
          else
            (text(i + 1): Atom.Base#Atom) match {

              case a: Atom.Literal         # Literal =>
                !a.value.startsWith(" ")

              // " " next
              case _: Atom.ContentRef      # CodeRef
                 | _: Atom.ContentRef      # ReqRef
                 | _: Atom.ContentRef      # UseCaseStepRef
                 | _: Atom.Issue           # Issue
                 | _: Atom.PlainTextMarkup # Bold
                 | _: Atom.PlainTextMarkup # EmailAddress
                 | _: Atom.PlainTextMarkup # Italic
                 | _: Atom.PlainTextMarkup # Monospace
                 | _: Atom.PlainTextMarkup # Strikethrough
                 | _: Atom.PlainTextMarkup # TeX
                 | _: Atom.PlainTextMarkup # Underline
                 | _: Atom.PlainTextMarkup # WebAddress
                 | _: Atom.TagRef          # TagRef =>
                true

              // next
              case _: Atom.CodeBlock       # CodeBlock
                 | _: Atom.Headings        # Heading1
                 | _: Atom.Headings        # Heading2
                 | _: Atom.Headings        # Heading3
                 | _: Atom.Headings        # Heading4
                 | _: Atom.Headings        # Heading5
                 | _: Atom.Headings        # Heading6
                 | _: Atom.ListMarkup      # OrderedList
                 | _: Atom.ListMarkup      # UnorderedList
                 | _: Atom.NewLine         # BlankLine =>
                false
            }

        atom match {

          case a: Atom.Literal # Literal =>
            val a2 =
              if (prevIsSpace && a.value.startsWith(" "))
                a.modText(_.drop(1))
              else
                a

            val a3 =
              if (i == last && !allowTrailingWS) {
                val x = a2.modText(TextMod.noWhitespaceRight.run)
                if (x.value.isEmpty)
                  dropLast = true
                x
              } else
                a2

            atom2 = a3
            prevIsSpace = a3.value.endsWith(" ")

          case a: Atom.Headings # Heading =>
            prevIsSpace = true
            atom2 = a.modTitle(fixNonEmpty(_, allowWsBeforeNext))

          case a: Atom.PlainTextMarkup # PlainTextMarkupStyled =>
            val a2 = a.unsafeWithInner(fixNonEmpty(a.inner, allowWsBeforeNext))
            atom2 = a2
            // No need to update prevIsSpace here.
            // It's a shared var so the last result of fixNonEmpty is exactly what's needed.

          case a: Atom.ListMarkup # ListBase =>
            atom2 = a.map { li =>
              prevIsSpace = true
              fixOptional(li, allowTrailingWS = false)
            }

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
             | _: Atom.TagRef          # TagRef =>
            prevIsSpace = false
        }

        if (!dropLast && (atom2 ne atom)) {
          if (result eq null) result = text.toArray
          result(i) = atom2.asInstanceOf[T2#Atom]
        }

        i += 1
      }

      if (dropLast) {
        if (result eq null) result = text.toArray
        result = result.dropRight(1)
      }

      if (result ne null)
        ArraySeq.unsafeWrapArray(result)
      else
        text
    }

    _fixNonEmpty = (text, allowTrailingWS) => {
      val t1 = text.whole
      val t2 = fixOptional(t1, allowTrailingWS)
      if ((t1 ne t2) && t2.nonEmpty)
        NonEmptyArraySeq.force(t2)
      else
        text
    }

    fixOptional(rootText, allowTrailingWS = false)
  }

  // Because there are special cases, not all whitespace is trimmed.
  // Not all whitespace need be trimmed because the parser already contains space handing - for example, literals are
  // trimmed as they're parsed, as confirmed by tests.
  //
  // Special cases:
  // 1) "* " is a valid multiline bullet with no content. "*" is not.
  // 2) "1. " is a valid multiline leader with no content. "1." is not.
  // 3) Whitespace before list item leads is important because it's additional context that affects how indentation of
  //    subsequent list items are interpreted
  private val multiLineCanTrim: PreProcessor.CanTrim =
    (a, i, leftTrimming) => a(i) match {
      case ' ' =>

        if (leftTrimming) {

          // Left trim
          val last = a.length - 1

          def postLI(i: Int): Boolean =
            if (i <= last)
              a(i) != ' ' // disallow trim if we've got an LI
            else
              true // not an LI, we can trim

          @tailrec
          def go(i: Int): Boolean =
            if (i == last)
              true // end of string
            else
              a(i) match {
                case ' '            => go(i + 1)
                case '*'            => postLI(i + 1)
                case c if c.isDigit =>
                  @tailrec
                  def canTrimPotentialOrderedListItem(i: Int): Boolean =
                    if (i == last)
                      true // end of string
                    else {
                      val c = a(i)
                      if (c.isDigit)
                        canTrimPotentialOrderedListItem(i + 1)
                      else if (c == '.')
                        postLI(i + 1) // we've confirmed the start of an OL LI, just need a space...
                      else
                        true
                    }
                  canTrimPotentialOrderedListItem(i + 1)
                case _ => true
              }

          go(i + 1)

        } else

          // Right trim
          (i > 0) && (a(i - 1) match {
            case '*' => false
            case '.' => !(i >= 2 && a(i - 2).isDigit)
            case _   => true
          })

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
    protected final def OWS: Rule0 =
      rule(zeroOrMore(' '))

    /** Optional whitespace and/or newlines */
    protected final def OWSNL: Rule0 =
      rule(anyOf(" \r\n").*)

    /** Wwhitespace and/or newlines */
    protected final def WSNL: Rule0 =
      rule(anyOf(" \r\n").+)

    protected final def indentationLevel: Rule1[Int] =
      rule(
        push {
          @tailrec
          def go(offset: Int): Int =
            charAtRC(offset) match {
              case ' ' => go(offset - 1)
              case _   => -offset - 1
            }
          go(-1)
        }
      )

    private val isWS: Char => Boolean =
      _ == ' '

    private val isNL: Char => Boolean = {
      case '\n' | '\r' => true
      case _           => false
    }

    @tailrec
    private def isStartOfLineAfterOWS(i: Int): Boolean =
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

    protected final def startOfLine: Rule0 =
      rule(BOI | test(isNL(lastChar)))

    protected final def startOfLineAfterOWS: Rule0 =
      rule(BOI | test(isStartOfLineAfterOWS(cursor - 1)))

    protected final val untilEOL = () => rule(OWS ~ EOL)

    protected final val lookupReq: (ReqType.Mnemonic, ReqTypePos) => Option[ReqId] =
      (m, n) =>
        project.config.reqTypes.allByMnemonic.get(m)
          .map(t => PubidT(t.reqTypeId, n))
          .flatMap(project.content.reqs.pubids.apply)

    protected final def hashRef: Rule1[HashRefTarget] =
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
    final type AtomSeqRule = () => Rule1[ArraySeq[t.Atom]]

    protected final val atomsToArraySeq: IterableOnce[t.Atom] => ArraySeq[t.Atom] = input => {
      var v = ArraySeq.empty[t.Atom]
      var lastIsBlank = false

      // Here we ensure that we don't end up with blank lines next to things that don't allow them around themselves.
      // A lot of this is done by the parsing rules but in the case of blank lines around top-level code blocks,
      // it was too hard and would require too much fundamental change to all the parsers. It's done here instead now.
      for (a <- input.iterator) {
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

    protected final val singleAtomSeq: t.Atom => ArraySeq[t.Atom] =
      ArraySeq1(_)

    protected final val atomSeqsToArraySeq: Seq[ArraySeq[t.Atom]] => ArraySeq[t.Atom] =
      i => atomsToArraySeq(i.iterator.flatten)

    def literalUntil[O <: HList](stop: () => Rule[HNil, O]): Rule1[t.Literal] =
      rule(capture(oneOrMore( !stop() ~ ANY )) ~> ((l: String) => t.Literal(fixLiteralWhiteSpace(l))))

    def tokensAndTextUntil(token: TokenRule, end: () => Rule0): Rule1[t.OptionalText] = {
      val endOrToken = () => rule(end() | token())
      rule(zeroOrMore(token() | literalUntil(endOrToken)) ~ end() ~> atomsToArraySeq)
    }

    def tokensAndTextUntilEOL(token: TokenRule): Rule1[t.OptionalText] =
      tokensAndTextUntil(token, untilEOL)

    def atomSeqsAndTextUntil(atomSeqs: AtomSeqRule, end: () => Rule0): Rule1[t.OptionalText] = {
      val endOrAtomSeq = () => rule(end() | atomSeqs())
      rule(zeroOrMore(atomSeqs() | (literalUntil(endOrAtomSeq) ~> singleAtomSeq)) ~ end() ~> atomSeqsToArraySeq)
    }

    def atomSeqsAndTextUntilEOL(atomSeqs: AtomSeqRule): Rule1[t.OptionalText] =
      atomSeqsAndTextUntil(atomSeqs, untilEOL)
  }

  // -------------------------------------------------------------------------------------------------------------------

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

  // -------------------------------------------------------------------------------------------------------------------

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

  // -------------------------------------------------------------------------------------------------------------------

  trait NewLine extends Base {
    override val t: Atom.NewLine
    def blankLine = rule(OWS ~ NL ~ OWSNL ~ push(t.blankLine))
  }

  // -------------------------------------------------------------------------------------------------------------------

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

    private val languageCP  = CP.Visible -- ':'
    private def attributeCP = languageCP
    private def separatorCP = CP.from(c => c == ':' || c == ' ')

    def codeBlockDetail: Rule1[CodeBlockDetail] =
      rule(
        separatorCP.*
        ~ capture(languageCP.+)
        ~ (separatorCP.+ ~ capture(attributeCP.+)).*
        ~ separatorCP.*
        ~> ((lang: String, attrs: Seq[String]) => {
          CodeBlockDetail(lang, TreeSet.empty[String] ++ attrs)
        })
      )

    def codeBlock: Rule1[t.CodeBlock] =
      rule(
        "```"
          ~ indentationLevelSoFar(3)
          ~ OWS
          ~ codeBlockDetail.?
          ~ OWS
          ~ &(NL)
          ~ nonGreedyCapture0(codeBlockEnd)
          ~ indentationLevelSoFar(3)
          ~ OWS
          ~> buildCodeBlock
      )

    private val buildCodeBlock: (Int, Option[CodeBlockDetail], String, Int) => t.CodeBlock =
      (startIndent, detail, codeTxt, endIndent) => {
        val indent = startIndent min endIndent
        val code = processCodeBlockCode(codeTxt.unindent(indent))
        t.CodeBlock(detail, code)
      }
  }

  // -------------------------------------------------------------------------------------------------------------------

  object ListMarkup {
    sealed trait ListItemType {
      final val some = Some(this)
    }
    object ListItemType {
      case object Unordered extends ListItemType
      case object Ordered extends ListItemType
    }

    final case class ListItem[+A](indent: Int, itemType: Option[ListItemType], body: ArraySeq[A])

    private final val Debug = false
  }

  trait ListMarkup extends Literal with CodeBlock {
    import ListMarkup._

    override val t: Atom.ListMarkup with Atom.Literal with Atom.NewLine with Atom.CodeBlock

    def listMarkup(listToken: TokenRule): Rule1[ArraySeq[t.ListBase]] =
      rule(OWSNL ~ indentationLevel ~ listItems(listToken) ~> mkLists)

    private def listItems(listToken: TokenRule): Rule1[NonEmptyArraySeq[ListItem[t.Atom]]] =
      rule(startOfLineAfterOWS ~ listItem(listToken).+ ~ OWSNL ~ popSeqSeqToNEA[ListItem[t.Atom]])

    private def listItem(listToken: TokenRule): Rule1[Seq[ListItem[t.Atom]]] = {
      val tailLines: TokenRule = () => rule(codeBlock | listToken())
      rule(
        optionalIndent
          ~ listItemLead ~ OWS
          ~ (firstLineCodeBlock | tokensAndTextUntil(listToken, untilEOL))
          ~ extraLine(tailLines).*
          ~> mkListItem
      )
    }

    private def optionalIndent: Rule1[String] =
      rule(capture(zeroOrMore(" ")) ~ (NL ~ pop[String] ~ optionalIndent).?)

    private def listItemLead: Rule1[ListItemType] =
      rule(
        (
          CP.Digit.+ ~ ". " ~ push(ListItemType.Ordered)
          ) | (
          // See https://en.wikipedia.org/wiki/Bullet_(typography)
          ("* " | anyOf("•‣⁃⁌⁍∙○◘◦☙❥❧⦾⦿")) ~ push(ListItemType.Unordered)
          )
      )

    private def firstLineCodeBlock =
      rule(codeBlock ~> ((x: t.CodeBlock) => ArraySeq1(x)))

    private def extraLine(listToken: TokenRule): Rule1[ListItem[t.Atom]] =
      rule(
        (NL ~ extraLine(listToken)) |
        (capture(' ' ~ OWS) ~ !listItemLead ~ tokensAndTextUntil(listToken, untilEOL) ~> mkExtraLine)
      )

    private val mkListItem: (String, ListItemType, ArraySeq[t.Atom], Seq[ListItem[t.Atom]]) => Seq[ListItem[t.Atom]] =
      (indent, itemType, headBody, tail) => {
        val lead = ListItem(indent.length, itemType.some, headBody)
        lead +: tail
      }

    private val mkExtraLine: (String, ArraySeq[t.Atom]) => ListItem[t.Atom] =
      (indent, body) => ListItem(indent.length, None, body)

    private final class BuildState(val itemType: ListItemType,
                                   headIndent  : Int,
                                   headBody    : t.ListItem) {

      @elidable(elidable.FINE)
      override def toString =
        s"BuildState(${indent()}, $itemType, ${items.toList})"

      private var _indent = headIndent
      def indent() = _indent

      private val items = ArrayBuffer.empty[t.ListItem]
      items += headBody

      def append(li: ListItem[t.Atom]): this.type = {
        items += li.body
        if (li.indent < _indent)
          _indent = li.indent
        this
      }

      def appendNested(list: t.ListBase): Unit = {
        val i = items.length - 1
        items.update(i, items(i) :+ list)
      }

      def modLatestListItem(f: t.ListItem => t.ListItem): Unit = {
        val i = items.length - 1
        val li1 = items(i)
        val li2 = f(li1)
        items.update(i, li2)
      }

      def result(): t.ListBase = {
        val lis = NonEmptyArraySeq.force[t.ListItem](ArraySeq.unsafeWrapArray(items.toArray))
        itemType match {
          case ListItemType.Unordered => t.UnorderedList(lis)
          case ListItemType.Ordered   => t.OrderedList(lis)
        }
      }
    }

    private val mkLists: (Int, NonEmptyArraySeq[ListItem[t.Atom]]) => ArraySeq[t.ListBase] =
      (initialIndentation, lisNonEmpty) => {
        val lis = lisNonEmpty.unsafeWholeArray

        if (initialIndentation > 0) {
          val li = lis(0)
          lis(0) = li.copy(indent = li.indent + initialIndentation)
        }

        if (Debug)
          println(
            s"""===============================================================================
               |inputs:${lisNonEmpty.iterator.map(l => "\n  - " + l.toString.quoteInner).mkString}
               |""".stripMargin)

        @tailrec
        def unfoldParents(parents: List[BuildState],
                          state: FreeOption[BuildState],
                          minIndent: Int): (List[BuildState], FreeOption[BuildState]) =
          parents match {
            case Nil =>
              (Nil, state)
            case immutable.::(p, ps) =>
              if (p.indent() < minIndent)
                (parents, state)
              else {
                if (state.nonEmpty) {
                  val li = state.getOrNull.result()
                  p.appendNested(li)
                }
                unfoldParents(ps, FreeOption(p), minIndent)
              }
          }

        @tailrec
        def go(pos      : Int,
               state    : FreeOption[BuildState],
               parents  : List[BuildState],
               completed: ArraySeq[t.ListBase]): ArraySeq[t.ListBase] =

          if (pos < lis.length) {
            // Add current item
            val li = lis(pos)

            li.itemType match {

              case Some(itemType) =>
                // Add new list item

                def newBuildState = new BuildState(itemType, li.indent, li.body)

                if (state.isEmpty) {
                  go(pos + 1, FreeOption(newBuildState), parents, completed)

                } else {
                  val s       = state.getOrNull
                  val indDiff = li.indent.compareTo(s.indent())

                  if (Debug)
                    println(
                      s"""Appending new list item.
                         |  li         = $li
                         |  parents    = $parents
                         |  state      = $state
                         |  indDiff    = ${indDiff}
                         |""".stripMargin
                    )

                  def completeCurrent(s: FreeOption[BuildState], parents: List[BuildState]) =
                    if (s.isEmpty)
                      completed
                    else {
                      val sibling = s.getOrNull.result()
                      parents match {
                        case Nil =>
                          completed :+ sibling
                        case immutable.::(p, _) =>
                          p.appendNested(sibling)
                          completed
                      }
                    }

                  if (indDiff > 0) {
                    // Greater indentation
                    go(pos + 1, FreeOption(newBuildState), s :: parents, completed)

                  } else if (indDiff < 0) {
                    // Lesser indentation
                    val (newParents, newState1) = unfoldParents(parents, state, li.indent)
                    if (newState1.exists(_.itemType == itemType)) {

                      // Same list type
                      val newState2 = newState1.fold(newBuildState, _.append(li))
                      go(pos + 1, FreeOption(newState2), newParents, completed)

                    } else {

                      // Different list type
                      val completed2 = completeCurrent(newState1, newParents)
                      go(pos + 1, FreeOption(newBuildState), newParents, completed2)
                    }

                  } else if (s.itemType == itemType) {
                    // Same level, same list type
                    s.append(li)
                    go(pos + 1, state, parents, completed)

                  } else {
                    // Same level, different list type
                    val completed2 = completeCurrent(state, parents)
                    go(pos + 1, FreeOption(newBuildState), parents, completed2)
                  }
                }

              case None =>
                // Append indented content to an existing list item

                // Note: this code looks unsafe because we don't have compile-time proof that all indented bodies
                // have parents, but this is trivially provable by looking at the parser. List items ALWAYS come before
                // indented bodies.
                val (_newParents, _newState) = unfoldParents(parents, state, li.indent)
                assert(_newState.nonEmpty,
                  s"""Indented bodies ALWAYS have parents.
                     |
                     |inputs:${lisNonEmpty.iterator.map(l => "\n  - " + l.toString.quoteInner).mkString}
                     |
                     |li         = $li
                     |parents    = $parents
                     |state      = $state
                     |newParents = ${_newParents}
                     |newState   = ${_newState}
                     |""".stripMargin)

                if (Debug)
                  println(
                    s"""Appending indented content.
                       |  li         = $li
                       |  parents    = $parents
                       |  state      = $state
                       |  newParents = ${_newParents}
                       |  newState   = ${_newState}
                       |""".stripMargin
                  )

                var newParents = _newParents
                var newState   = _newState.getOrNull

                if (newParents.nonEmpty && newState.indent() >= li.indent) {
                  val p = newParents.head
                  newParents = newParents.tail
                  p.appendNested(newState.result())
                  newState = p
                }

                newState.modLatestListItem { targetLI =>
                  var newBody = targetLI
                  val addBlank = newBody.lastOption.forall(_.allowBlankLineAfter) && li.body.headOption.exists(_.allowBlankLineBefore)
                  if (addBlank)
                    newBody :+= t.blankLine
                  newBody ++= li.body
                  newBody
                }

                go(pos + 1, FreeOption(newState), newParents, completed)
            }

          } else {
            // Done
            unfoldParents(parents, state, 0)._2.fold(completed, completed :+ _.result())
          }

        val result = go(0, FreeOption.empty, Nil, ArraySeq.empty)

        if (Debug)
          println(
            s"""
               |result:${result.iterator.map(l => "\n  - " + io.circe.Encoder.encodeString(l.toString).noSpaces.drop(1).dropRight(1)).mkString}
               |
               |===============================================================================
               |""".stripMargin)

        result
      }
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait ContentRef extends Base with UseCaseStepLabel {
    override val t: Atom.ContentRef

    import G.reflinkSurround.parsing.{prefix, suffix}
    import ReqCode._

    private def displayReqRef: Rule1[DisplayReqRef] =
      rule(
        (':' ~ OWS ~ push(DisplayReqRef.AsIdAndTitle))
        | push(DisplayReqRef.AsId)
      )

    private def refResult[A, I](make: (I, DisplayReqRef) => A): Rule[I :: DisplayReqRef :: HNil, A :: HNil] =
      rule(test(true) ~> make)

    def reqRef: Rule1[t.ReqRef] = rule(
      prefix ~ OWS ~ reqTypeMnemonicCI ~ OWS ~ ('-' ~ OWS).? ~ reqTypePos ~ OWS
        ~> lookupReq ~ popOptional[ReqId]
        ~ displayReqRef ~ suffix
        ~ refResult(t.ReqRef(_, _))
    )

    def reqCodeNode: Rule1[Node] = rule(
      capture(grammarStr(G.reqCode)(_.firstChar, _.tailChars, None, _.nodeLength)) ~> Node.applyFn)

    // Could be optimised to lookup each node as parsed and fail early
    def codeRef: Rule1[t.CodeRef] = rule(
      prefix ~ oneOrMore(OWS ~ reqCodeNode).separatedBy(OWS ~ G.reqCode.nodeSeparator) ~ OWS
        ~> lookupCode ~ popOptional[ReqCodeId]
        ~ displayReqRef ~ suffix
        ~ refResult(t.CodeRef(_, _))
    )

    val lookupCode: Seq[Node] => Option[ReqCodeId] = ss =>
      NonEmptyVector.maybe(ss.toVector, None: Option[ReqCodeId])(code =>
        project.content.reqCodes.get(code).flatMap(_.activeId))

    override def useCaseStepLabelLookup = project.content.reqs.useCaseStepLabelLookup

    def useCaseStepRef: Rule1[t.Atom] =
      rule(prefix ~ OWS ~ useCaseStepLabel ~ suffix ~> t.UseCaseStepRef)

    def contentRef: Rule1[t.Atom] =
      rule(useCaseStepRef | codeRef | reqRef)
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait TagRef extends Base {
    override val t: Atom.TagRef

    def tagRef = popPF[HashRefTarget, t.TagRef] { case -\/(tag) => t.TagRef(tag.id) }
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait UseCaseStepLabel extends ParsingUtil {

    /** Optional whitespace */
    protected def OWS: Rule0

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

  // -------------------------------------------------------------------------------------------------------------------

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

  // -------------------------------------------------------------------------------------------------------------------

  trait HeadingTitle extends Literal {
    val token: TokenRule
    final def inline: Rule1[t.NonEmptyText] = rule(tokensAndTextUntilEOL(token) ~ popNEA)
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

    protected val token: TokenRule

    def optionalText: Rule1[t.OptionalText] =
      rule(OWS ~ tokensAndTextUntilEOL(token) ~ EOI)
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait MultiLine extends SingleLine with NewLine with ListMarkup with CodeBlock with Headings {
    override val t: Atom.MultiLine
    protected val additionalTokens: TokenRule

    final val listToken: TokenRule =
      () => rule(additionalTokens() | singleLine)

    final val token: TokenRule =
      () => rule(heading() | codeBlock | additionalTokens() | blankLine | singleLine)

    val atomSeq: AtomSeqRule =
      () => rule(listMarkup(listToken) | (token() ~> singleAtomSeq))

    final override def optionalText: Rule1[t.OptionalText] =
      rule(atomSeqsAndTextUntilEOL(atomSeq) ~ EOI)
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait TopBase extends Literal {
    protected val token: TokenRule

    def optionalText: Rule1[t.OptionalText]

    final def nonEmptyText: Rule1[t.NonEmptyText] =
      rule(optionalText ~ popNEA)
  }
}