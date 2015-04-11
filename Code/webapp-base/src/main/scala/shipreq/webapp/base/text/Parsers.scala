package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.base.util.NonEmptyVector
import scalaz.{\/, -\/, \/-}
import shapeless._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}

object Parsers {
  def preprocess: String => String =
    _.replace('\t', ' ')

  // questionable: :;=?\/
  val emailCharArray = """!$%*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~""".toCharArray
  val emailCharL = CharPredicate(emailCharArray)
  val emailCharR = emailCharL -- '.'

  val webAddressChar = CharPredicate.Visible -- ('{' :: '}' :: '[' :: ']' :: '<' :: '>' :: Nil)

  type RuleAB[-A, +B] = Rule[A :: HNil, B :: HNil]

  abstract class Base extends Parser {
    type T <: Text.Generic
    val t: T
    val project: Project

    /** Beginning Of Input */
    def BOI = rule(test(cursor == 0))

    /** Optional whitespace */
    def OWS = rule(zeroOrMore(' '))

    /** newline */
    def NL = rule('\n' | ('\r' ~ '\n'.?))

    /** Optional whitespace and/or newlines */
    def OWSNL = rule(anyOf(" \r\n").*)

    /** End Of Line (consumes NL) */
    def EOL = rule(NL | EOI)

    val untilEOL = () => rule(OWS ~ EOL)

    def trim = (_: String).trim

    /** int ≥ 1 */
    def int1n = rule( capture(CharPredicate.Digit19 ~ CharPredicate.Digit.*) ~> (_.toInt) )

    def popOptional[A]: RuleAB[Option[A], A] =
      rule(run((o: Option[A]) => test(o.isDefined) ~ push(o.get)))

    def popPF[A, B](pf: PartialFunction[A, B]): RuleAB[A, B] =
      rule(run((a: A) => test(pf isDefinedAt a) ~ push(pf(a)))) // TODO use pf.lift

    def popSeqToNEV[A]: RuleAB[Seq[A], NonEmptyVector[A]] =
      rule(run((v: Seq[A]) => test(v.nonEmpty) ~ push(NonEmptyVector(v.head, v.tail.toVector))))

    def popNEV[A]: RuleAB[Vector[A], NonEmptyVector[A]] =
      rule(run((v: Vector[A]) => test(v.nonEmpty) ~ push(NonEmptyVector(v.head, v.tail))))

    def grammarStr[G](g: G)(f: G => Grammar.FirstChar, w: G => Grammar.CharWhitelist, l: G => Grammar.Length): Rule0 =
      rule( f(g).charPredicate ~ (l(g).minus1 times w(g).charPredicate) )

    def nonGreedyCapture(stopAt: () => Rule0): Rule1[String] =
      rule(capture(oneOrMore(!stopAt() ~ ANY)) ~ stopAt())

    def surround(s: Grammar.Surrounds): Rule1[String] =
      surround(s.parsing)

    def surround(s: Grammar.Surround): Rule1[String] = {
      val end = () => rule(s.suffix)
      rule(s.prefix ~ nonGreedyCapture(end) ~> trim)
    }

    // TODO Not using Grammar because of case-sensitivity
    def reqTypeMnemonic = rule(
      capture(G.reqTypeMnemonic.length.total times CharPredicate.Alpha)
        ~> (_.toUpperCase |> ReqType.Mnemonic))

    def reqTypePos = rule( int1n ~> ReqTypePos )

    val lookupReq: (ReqType.Mnemonic, ReqTypePos) => Option[Req.Id] =
      (m, n) =>
        project.reqTypesByMnemonic.get(m)
          .map(t => Pubid(t.reqTypeId, n))
          .flatMap(project.reqs.data.reqIdByPubid)

    def hashRef = rule(
      G.hashRefKey.prefix ~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.allChars, _.length))
      ~> (HashRefKey(_) |> project.hashRefs.get) ~ popOptional
    )
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
    def newLine = rule(OWS ~ NL ~ OWSNL ~ push(t.newLine))
    // TODO Rename to BlankLine
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

    def reqRef: Rule1[t.ReqRef] = rule(
      G.reflinkPrefix ~ OWS ~ reqTypeMnemonic ~ OWS ~ ('-' ~ OWS).? ~ reqTypePos ~ OWS ~ G.reflinkSuffix
        ~> lookupReq ~ popOptional[Req.Id] ~> t.ReqRef)
  }

  trait TagRef extends Base {
    override type T <: Atom.TagRef

    def tagRef = popPF[HashRefTarget, t.TagRef] { case -\/(tag) => t.TagRef(tag.id) }
  }

  trait Issue extends Base {
    override type T <: Atom.Issue
    import Text.{InlineIssueDesc => I}

    def issueRef: RuleAB[HashRefTarget, t.Issue] = {
      def id           = popPF[HashRefTarget, CustomIssueType.Id] { case \/-(i) => i.id }
      def optionalDesc = rule(issueInnerDesc ~> (_.whole) | push(Vector.empty))
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
      () => rule(unorderedList(listToken) | additionalTokens() | newLine | singleLine)
  }

  // ===================================================================================================================

  abstract class TopBase[_T <: Atom.Literal](_t: _T) extends Literal {
    override final type T = _T
    override final val  t: T = _t
    protected val token: TokenRule
    final def optionalText: Rule1[T#OptionalText] = rule(OWS ~ text(token) ~ EOI)
    final def nonEmptyText: Rule1[T#NonEmptyText] = rule(optionalText ~ popNEV)
  }

  abstract class ReqTitle[_T <: Atom.ReqTitle](_t: _T, val project: Project, val input: ParserInput) extends TopBase(_t)
    with SingleLine
    with ReqRef
    with Issue {

    def hashToken = rule(hashRef ~ issueRef)
    val token = () => rule(hashToken | reqRef | singleLine)
  }
}