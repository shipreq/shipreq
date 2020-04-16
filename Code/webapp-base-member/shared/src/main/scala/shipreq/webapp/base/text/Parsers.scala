package shipreq.webapp.base.text

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.parboiled2.{CharPredicate => CP, _}
import scalaz.{-\/, \/-}
import shapeless._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.util.{ParsingUtil, PreProcessed, PreProcessor}

object Parsers {

  // Because there are special cases, not all whitespace is trimmed.
  // Not all whitespace need be trimmed because the parser already contains space handing - for example, literals are
  // trimmed as they're parsed, as confirmed by tests.
  //
  // Special cases:
  // 1) "* " is a valid multiline bullet with no content. "*" is not.
  private val multiLineCanTrim: PreProcessor.CanTrim =
    (a, i) => a(i) match {
      case ' ' =>
        (i == 0) || {
          val prevChar = a(i - 1)
          prevChar != '*' // Space need only be preserved after an asterisk
        }
      case c =>
        PreProcessor.canTrimWhitespaceFn(c)
    }

  val preProcessor: LineCardinality => String => PreProcessed =
    LineCardinality.memo {
      case MultiLine  => PreProcessor(PreProcessor.fixCharMultiLine, multiLineCanTrim)
      case SingleLine => PreProcessor.singleLine
    }

  // questionable: :;=?\/
  val emailCharArray = """!$%*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~""".toCharArray
  val emailCharL = CP(emailCharArray)
  val emailCharR = emailCharL -- '.'

  val webAddressChar = CP.Visible -- ('{' :: '}' :: '[' :: ']' :: '<' :: '>' :: '`' :: Nil)

  private val useCaseStepTailChar = CP.AlphaNum ++ ' ' ++ '.'

  abstract class Base extends ParsingUtil {
    type T <: Atom.Base
    val t: T
    val project: Project

    /** Optional whitespace */
    def OWS: Rule0 =
      rule(zeroOrMore(' '))

    /** Optional whitespace and/or newlines */
    def OWSNL: Rule0 =
      rule(anyOf(" \r\n").*)

    val untilEOL = () => rule(OWS ~ EOL)

    val lookupReq: (ReqType.Mnemonic, ReqTypePos) => Option[ReqId] =
      (m, n) =>
        project.config.reqTypes.allByMnemonic.get(m)
          .map(t => PubidT(t.reqTypeId, n))
          .flatMap(project.content.reqs.pubids.apply)

    def hashRef: Rule1[HashRefTarget] =
      rule(hashRefStr ~> (project.config.hashRefLookup _) ~ popOptional)
  }

  // ===================================================================================================================
  // Modules

  trait Literal extends Base {
    override type T <: Atom.Literal

    final type TokenRule = () => Rule1[t.Atom]

    protected val atomsToVector: Seq[t.Atom] => Vector[t.Atom] = input => {
      var v = Vector.empty[t.Atom]
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
      rule(capture(oneOrMore( !stop() ~ ANY )) ~> t.Literal)

    def textUntil(token: TokenRule, end: () => Rule0): Rule1[t.OptionalText] = {
      val endOrToken = () => rule(end() | token())
      rule(zeroOrMore(token() | literalUntil(endOrToken)) ~ end() ~> atomsToVector)
    }

    def text(token: TokenRule): Rule1[t.OptionalText] =
      textUntil(token, untilEOL)
  }

  trait PlainTextMarkup extends Base {
    override type T <: Atom.PlainTextMarkup

    def webScheme = rule( (("http" | "ftp") ~ 's'.?) | "sftp" )

    // TODO ensure webAddress and emailAddress don't follow literal

    def webAddress =
      rule(capture(webScheme ~ "://" ~ webAddressChar.+) ~> t.WebAddress)

    def emailAddress =
      rule("mailto:".? ~ capture(emailCharL.+ ~ '@' ~ (emailCharR.+ ~ '.').+ ~ emailCharR.+) ~> t.EmailAddress)

    def tex =
      rule(surround(G.texSurround) ~> (_.trim |> t.TeX))

    def monospace =
      rule('`' ~ capture(oneOrMore(!('`' | NL | EOI) ~ ANY)) ~ '`' ~> t.Monospace)

    def plainTextMarkup =
      rule( webAddress | emailAddress | tex | monospace )
  }

  trait NewLine extends Base {
    override type T <: Atom.NewLine
    def blankLine = rule(OWS ~ NL ~ OWSNL ~ push(t.blankLine))
  }

  trait CodeBlock extends Literal {
    override type T <: Atom.CodeBlock with Atom.Literal

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
        val code =
          codeTxt
            .unindent(indent)
            .linesWithSeparators
            .map(_.replaceFirst("[ \r\n]+$", "")) // right-trim all lines
            .dropWhile(_.isEmpty)                 // remove leading blank lines
            .toArray
            .reverseIterator
            .dropWhile(_.isEmpty)                // remove trailing blank lines
            .toArray
            .reverseIterator
            .mkString("\n")
        t.CodeBlock(lang, code)
      }
  }

  trait ListMarkup extends Literal with CodeBlock {
    override type T <: Atom.ListMarkup with Atom.Literal with Atom.NewLine with Atom.CodeBlock

    private def bullet: Rule0 =
      // See https://en.wikipedia.org/wiki/Bullet_(typography)
      rule("* " | anyOf("•‣⁃⁌⁍∙○◘◦☙❥❧⦾⦿"))

    private def firstLineCodeBlock =
      rule(codeBlock ~> ((x: t.CodeBlock) => Vector(x)))

    def listItem(listToken: TokenRule): Rule1[t.ListItem] = {
      val tailLines: TokenRule = () => rule(codeBlock | listToken())
      rule(
        OWSNL
          ~ bullet ~ OWS
          ~ (firstLineCodeBlock | textUntil(listToken, untilEOL))
          ~ extraLine(tailLines).*
          ~> combineListItemLines
      )
    }

    private val combineListItemLines: (Vector[t.Atom], Seq[Vector[t.Atom]]) => t.ListItem = (head, tail) => {
      tail.iterator.filter(_.nonEmpty).foldLeft(head) { (q, n) =>
        if (q.lastOption.exists(_.allowBlankLineAfter) && n.headOption.exists(_.allowBlankLineBefore))
          (q :+ t.blankLine) ++ n
        else
          q ++ n
      }
    }

    private def extraLine(listToken: TokenRule): Rule1[Vector[t.Atom]] =
      rule(
        (NL ~ extraLine(listToken)) |
        (' ' ~ OWS ~ !bullet ~ textUntil(listToken, untilEOL))
      )

     def unorderedList(listToken: TokenRule): Rule1[t.UnorderedList] =
       rule((BOI | (OWS ~ NL)) ~ listItem(listToken).+ ~ OWSNL ~ popSeqToNEV[t.ListItem] ~> t.UnorderedList)
  }

  trait ContentRef extends Base with UseCaseStepLabel {
    override type T <: Atom.ContentRef

    import G.reflinkSurround.parsing.{prefix, suffix}
    import ReqCode._

    def reqRef: Rule1[t.ReqRef] = rule(
      prefix ~ OWS ~ reqTypeMnemonicCI ~ OWS ~ ('-' ~ OWS).? ~ reqTypePos ~ OWS ~ suffix
        ~> lookupReq ~ popOptional[ReqId] ~> t.ReqRef)

    def reqCodeNode: Rule1[Node] = rule(
      capture(grammarStr(G.reqCode)(_.firstChar, _.tailChars, _.nodeLength)) ~> Node.applyFn)

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
    override type T <: Atom.TagRef

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
    override type T <: Atom.Issue
    import Text.{InlineIssueDesc => I}

    def issueRef: RuleAB[HashRefTarget, t.Issue] = {
      def id           = popPF[HashRefTarget, CustomIssueTypeId] { case \/-(i) => i.id }
      def optionalDesc = rule(OWS ~ issueInnerDesc ~> (_.whole) | push(Vector.empty))
      rule(run(id) ~ optionalDesc ~> t.Issue)
    }

    // Hack due to https://github.com/sirthias/parboiled2/issues/120
    // runSubParser can only be used in a method directly in a class, not a trait like this
    protected def issueInnerDesc: Rule1[I.NonEmptyText] //= rule(runSubParser(I.parserI(project)(_).inline))
  }

  // ===================================================================================================================

  trait SingleLine extends PlainTextMarkup with Literal {
    override type T <: Atom.SingleLine
    def singleLine = plainTextMarkup
  }

  trait MultiLine extends SingleLine with NewLine with ListMarkup with CodeBlock {
    override type T <: Atom.MultiLine
    protected val additionalTokens: TokenRule

    final val listToken: TokenRule =
      () => rule(additionalTokens() | singleLine)

    final val token: TokenRule =
      () => rule(unorderedList(listToken) | codeBlock | additionalTokens() | blankLine | singleLine)
  }

  // ===================================================================================================================

  abstract class TopBase[_T <: Atom.Literal](_t: _T) extends Literal {
    override final type T = _T
    override final val  t: T = _t
    protected val token: TokenRule
    final def optionalText: Rule1[T#OptionalText] = rule(OWS ~ text(token) ~ EOI)
    final def nonEmptyText: Rule1[T#NonEmptyText] = rule(optionalText ~ popNEV)
  }
}