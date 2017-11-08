package shipreq.webapp.base.text

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.parboiled2.{CharPredicate => CP, _}
import scala.annotation.{switch, tailrec}
import scalaz.{-\/, \/, \/-}
import shapeless._
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.base.util.VectorTree.PartialLocation
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.WebappConfig
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
        // Space need only be preserved after an asterisk
        !(i != 0 && a(i - 1) == '*')
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

  val webAddressChar = CP.Visible -- ('{' :: '}' :: '[' :: ']' :: '<' :: '>' :: Nil)

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

    protected def atomsToVector = (_: Seq[t.Atom]).toVector

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

    def mathtex =
      rule(surround(G.mathTexSurround) ~> (_.trim |> t.MathTeX))

    def plainTextMarkup =
      rule( webAddress | emailAddress | mathtex )
  }

  trait NewLine extends Base {
    override type T <: Atom.NewLine
    def blankLine = rule(OWS ~ NL ~ OWSNL ~ push(t.blankLine))
  }

  trait ListMarkup extends Literal {
    override type T <: Atom.ListMarkup with Atom.Literal

    def listItem(listToken: TokenRule): Rule1[t.ListItem] =
      rule(OWSNL ~ "* " ~ OWS ~ textUntil(listToken, untilEOL))

     def unorderedList(listToken: TokenRule): Rule1[t.UnorderedList] =
       rule((BOI | (OWS ~ NL)) ~ listItem(listToken).+ ~ OWSNL ~ popSeqToNEV[t.ListItem] ~> t.UnorderedList)
  }

  trait ReqRef extends Base {
    override type T <: Atom.ReqRef

    import G.reflinkSurround.parsing.{prefix, suffix}
    import ReqCode._

    def pubidRef: Rule1[t.ReqRef] = rule(
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

    def reqRef: Rule1[t.Atom] =
      rule(codeRef | pubidRef)
  }

  trait TagRef extends Base {
    override type T <: Atom.TagRef

    def tagRef = popPF[HashRefTarget, t.TagRef] { case -\/(tag) => t.TagRef(tag.id) }
  }

  trait UseCaseStepLabel extends ParsingUtil {

    /** Optional whitespace */
    def OWS: Rule0

    def reqs: Requirements

    /** If specified, allows parsing of [.1] instead of [n.1] where n is the value specified */
    def currentUseCase: Option[ReqTypePos]

    /** Expects no leading whitespace.
      * Gobbles any trailing whitespace.
      */
    def useCaseStepLabel: Rule1[UseCaseStepId] = {

      def dotStep: Rule1[String] =
        rule('.' ~ OWS ~ capture(CP.Alpha.+ | CP.Digit.+) ~ OWS)

      def ctxFree: Rule1[UseCaseStepId] = rule(
        ((ch('U')|'u') ~ (ch('C')|'c') ~ OWS ~ ('-' ~ OWS).?).? // UC-
          ~ reqTypePos ~ OWS                                    // 1
          ~ dotStep ~ dotStep.*                                 // .0.X.1.a.ii
          ~> lookupStep ~ popOptional[UseCaseStepId])

      def withCtx: Rule1[UseCaseStepId] = rule(
        pushOptional(currentUseCase)
          ~ (dotStep | (capture(CP.Alpha.+) ~ OWS)) // .0 | E
          ~ dotStep.*                               // .0.X.1.a.ii
          ~> lookupStep ~ popOptional[UseCaseStepId])

      rule(ctxFree | withCtx)
    }

    val lookupStep: (ReqTypePos, String, Seq[String]) => Option[UseCaseStepId] =
      (pos, nodeHead, nodeTail) => {

        val prefix = nodeHead.toUpperCase

        val (nodes, field) =
          StaticField.useCaseStepTrees.find(_.stepLabelPrefix.exists(_ ==* prefix)) match {
            case Some(sf) => (nodeTail, sf)
            case None     => (nodeHead +: nodeTail, StaticField.NormalAltStepTree)
          }

        def parseNodes(f: StaticField.UseCaseStepTree): Option[PartialLocation] = {
          val it = nodes.iterator
          @tailrec def go(q: Vector[Int], v: Validity, l: Int): Option[PartialLocation] =
            if (it.hasNext) {
              val node = it.next()

              // Only match uppercase. Lowercase x is used in step labels & ambiguous.
              if (node.length ==* 1 && node.charAt(0) ==* WebappConfig.useCaseStepsDeadNode)
                v match {
                  case Valid   => go(q :+ -1, Invalid, l)
                  case Invalid => None
                }
              else
                f.stepLabelsPerLevel.get(l).flatMap(_ parse node) match {
                  case Some(i) => go(q :+ i, v, l + 1)
                  case None    => None
                }
            } else
              NonEmptyVector.option(q)
                .filter(_.last >= 0) // Last node must be valid
                .map(PartialLocation(_, v))

          go(Vector.empty, Valid, 0)
        }

        for {
          uc ← reqs.getUseCaseByPos(pos)
          pl ← parseNodes(field)
          id ← field.useCaseSteps.get(uc).partialLocSteps.getOption(pl)
        } yield id
      }
  }

  trait UseCaseStepRef extends Base with UseCaseStepLabel {
    override type T <: Atom.UseCaseStepRef

    import G.reflinkSurround.parsing.{prefix, suffix}

    override def reqs = project.content.reqs

    def useCaseStepRef: Rule1[t.Atom] =
      rule(prefix ~ OWS ~ useCaseStepLabel ~ suffix ~> t.UseCaseStepRef)
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

  trait MultiLine extends SingleLine with NewLine with ListMarkup {
    override type T <: Atom.MultiLine
    protected val additionalTokens: TokenRule
    final val listToken: TokenRule =
      () => rule(additionalTokens() | singleLine)
    final val token: TokenRule =
      () => rule(unorderedList(listToken) | additionalTokens() | blankLine | singleLine)
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