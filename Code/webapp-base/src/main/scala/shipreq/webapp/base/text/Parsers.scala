package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.base.util.NonEmptyVector
import scalaz.{\/, -\/, \/-}
import shapeless._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import Text.{Generic => TG}

object Parsers {
  def preprocess: String => String =
    _.replace('\t', ' ').replaceAll("\r\n?", "\n").trim

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

    /** Optional whitespace */
    def ows = rule( zeroOrMore(' ') )

    /** End Of Token */
    def EOT = rule( ows | EOI )

    /** End Of Line */
    def EOL = rule( "\n" | EOI )

    def int1n = rule( capture(CharPredicate.Digit19 ~ CharPredicate.Digit.*) ~> (_.toInt) )

    def pushOptional[A](o: Option[A]): Rule1[A] =
      rule( test(o.isDefined) ~ push(o.get) )

    def pushPF[A, B](a: A)(pf: PartialFunction[A, B]): Rule1[B] =
      a |> pf.lift |> pushOptional

    def runPF[A, B](pf: PartialFunction[A, B]): RuleAB[A, B] =
      rule(run((a: A) => pushPF(a)(pf)))

    def grammarStr[G](g: G)(f: G => Grammar.FirstChar, w: G => Grammar.CharWhitelist, l: G => Grammar.Length): Rule0 =
      rule( f(g).charPredicate ~ (l(g).minus1 times w(g).charPredicate) )

    def nonGreedyCapture(stopAt: () => Rule0): Rule1[String] = rule(
      capture((!(stopAt()) ~ ANY).+) ~ stopAt()
    )

    def surround(s: Grammar.Surrounds): Rule1[String] =
      surround(s.parsing)

    def surround(s: Grammar.Surround): Rule1[String] = {
      val end = () => rule(s.suffix)
      rule(s.prefix ~ nonGreedyCapture(end) ~> ((_: String).trim))
    }

    // TODO Not using Grammar because of case-sensitivity
    def reqTypeMnemonic = rule(
      capture(G.reqTypeMnemonic.length.total times CharPredicate.Alpha)
        ~> (_.toUpperCase |> ReqType.Mnemonic))

    def reqTypePos = rule( int1n ~> ReqTypePos )

    def lookupReq(m: ReqType.Mnemonic, n: ReqTypePos): Option[Req.Id] =
      project.reqTypesByMnemonic.get(m)
        .map(t => Pubid(t.reqTypeId, n))
        .flatMap(project.reqs.data.reqIdByPubid)

    def hashRef = rule(
      G.hashRefKey.prefix ~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.allChars, _.length))
      ~> (HashRefKey(_) |> project.hashRefs.get |> pushOptional)
    )
  }

  // ===================================================================================================================
  // Modules

  trait LiteralParser extends Base {
    override type T <: TG.Literal

    def literal =
      rule(ANY ~ push(-\/(lastChar)))

    def optionalText(token: () => Rule1[t.Atom]): Rule1[t.OptionalText] = rule(
      (token().~>(\/-(_)) | literal).* ~> (consolidate(_: Seq[Char \/ t.Atom]))
    )

    def nonEmptyText(token: () => Rule1[t.Atom]): Rule1[t.NonEmptyText] = rule(
      (token().~>(\/-(_)) | literal).+ ~> (consolidate(_: Seq[Char \/ t.Atom]) |> forceNEV)
    )

    def forceNEV[A](as: Vector[A]): NonEmptyVector[A] = // TODO No
      NonEmptyVector(as.head, as.tail)

    def consolidate(cs: Seq[Char \/ t.Atom]): t.OptionalText = {
      var lit = Vector.empty[Char]

      def prependLit(tail: t.OptionalText): t.OptionalText =
        if (lit.isEmpty) tail else {
          val l = t.Literal(lit.mkString)
          lit = Vector.empty
          l +: tail
        }

      val x =
      // TODO use foldLeft
        cs.foldRight[t.OptionalText](Vector.empty)((c, q) =>
          c match {
            case -\/(ch) => lit :+= ch; q
            case \/-(to) => to +: prependLit(q)
          }
        )

      prependLit(x)
    }
  }

  trait PlainTextMarkupParser extends Base {
    override type T <: TG.PlainTextMarkup

    def webScheme = rule( (("http" | "ftp") ~ 's'.?) | "sftp" )

    // TODO ensure webAddress and emailAddress don't follow literal

    def webAddress = rule(
      capture(webScheme ~ "://" ~ webAddressChar.+) ~> t.WebAddress
    ) //~ EOT)

    def emailAddress: Rule1[t.EmailAddress] = rule(
      "mailto:".?
        ~ capture(emailCharL.+ ~ '@' ~ (emailCharR.+ ~ '.').+ ~ emailCharR.+) ~> t.EmailAddress
    ) //~ EOT)

    def mathtex = rule(
      surround(G.mathTexSurround) ~> (_.trim |> t.MathTeX)
    ) //~ EOT)

    def plainTextMarkup =
      rule( webAddress | emailAddress | mathtex )
  }

  trait SingleLine extends PlainTextMarkupParser with LiteralParser {
    override type T <: TG.PlainTextMarkup with TG.Literal
    def singleLine = plainTextMarkup
  }

  trait NewLineParser extends Base {
    override type T <: TG.NewLine
    def newLine = rule( "\n" ~ push(t.NewLine()) )
  }

  trait ReqRefParser extends Base {
    override type T <: TG.ReqRef

    def reqRef: Rule1[t.ReqRef] = rule(
      G.reflinkPrefix ~ ows ~ reqTypeMnemonic ~ ows ~ ('-' ~ ows).? ~ reqTypePos ~ ows ~ G.reflinkSuffix
        //~ EOT
        ~> (lookupReq(_, _) |> pushOptional) ~> t.ReqRef
    )
  }

  trait TagRefParser extends Base {
    override type T <: TG.TagRef
    def tagRef = runPF[HashRefTarget, t.TagRef] {
      case -\/(tag) => t.TagRef(tag.id)
    }
  }

  trait IssueParser extends Base {
    override type T <: TG.Issue
    def issueRef: RuleAB[HashRefTarget, t.Issue] = {
      def id = runPF[HashRefTarget, CustomIssueType.Id] { case \/-(i) => i.id }
      def desc = rule(surround(G.issueDescSurround) ~> ((i: String) => new InlineIssueDescParser(project, i).main.run().get))
      def optionalDesc = rule(desc ~> (_.whole) | push(Vector.empty))
      rule(run(id) ~ optionalDesc ~> t.Issue)
    }
  }

  // ===================================================================================================================
  // Specialised

  final class InlineIssueDescParser(val project: Project, val input: ParserInput)
      extends SingleLine with ReqRefParser {

    override type T = Text.InlineIssueDesc.type
    override val  t = Text.InlineIssueDesc

    def main = rule(nonEmptyText(token) ~ EOI)
    val token: () => Rule1[t.Atom] = () => rule(reqRef | singleLine)
  }

  sealed class ReqTitleParser[TT <: TG.ReqTitle](tt: TT, val project: Project, val input: ParserInput)
      extends SingleLine with ReqRefParser with IssueParser {

    override type T = TT
    override val  t = tt

    def main = rule(optionalText(token) ~ EOI)
    def hashToken = rule(hashRef ~ issueRef)
    val token: () => Rule1[t.Atom] = () => rule(hashToken | reqRef | singleLine)
  }
  
  final class RecCodeGroupDescParser(p: Project, i: ParserInput) extends ReqTitleParser(Text.RecCodeGroupDesc, p, i)
  final class GenericReqDescParser  (p: Project, i: ParserInput) extends ReqTitleParser(Text.GenericReqDesc, p, i)
}