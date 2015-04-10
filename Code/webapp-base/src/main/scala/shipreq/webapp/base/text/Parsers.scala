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

    def runNEV[A]: RuleAB[Vector[A], NonEmptyVector[A]] =
      rule(run((v: Vector[A]) => test(v.nonEmpty) ~ push(NonEmptyVector(v.head, v.tail))))

    def runO[A]: RuleAB[Option[A], A] =
      rule(run((o: Option[A]) => test(o.isDefined) ~ push(o.get)))

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

  trait Literal extends Base {
    override type T <: Atom.Literal

    /*
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

      def addLit(tgt: t.OptionalText): t.OptionalText =
        if (lit.isEmpty) tgt else {
          val l = t.Literal(lit.mkString)
          lit = Vector.empty
          tgt :+ l
        }

      val x =
        cs.foldLeft[t.OptionalText](Vector.empty)((q, c) =>
          c match {
            case -\/(ch) => lit :+= ch; q
            case \/-(to) => addLit(q) :+ to
          }
        )

      addLit(x)
    }
    */

    protected def atomsToVector = (_: Seq[t.Atom]).toVector

    def literalUntil[O <: HList](stop: () => Rule[HNil, O]): Rule1[t.Literal] = rule(
      capture(oneOrMore( !(stop()) ~ ANY )) ~> t.Literal)

    def tokenOrLiteral(token: () => Rule1[t.Atom]): Rule1[t.Atom] = rule(
      token() | literalUntil(token))

    def optionalText(token: () => Rule1[t.Atom]): Rule1[t.OptionalText] =
      rule(tokenOrLiteral(token).* ~> atomsToVector)

    def nonEmptyText(token: () => Rule1[t.Atom]): Rule1[t.NonEmptyText] =
      rule(optionalText(token) ~ runNEV)
  }

  trait PlainTextMarkup extends Base {
    override type T <: Atom.PlainTextMarkup

    def webScheme = rule( (("http" | "ftp") ~ 's'.?) | "sftp" )

    // TODO ensure webAddress and emailAddress don't follow literal

    def webAddress = rule(
      capture(webScheme ~ "://" ~ webAddressChar.+) ~> t.WebAddress
    ) //~ EOT)

    def emailAddress = rule(
      "mailto:".?
        ~ capture(emailCharL.+ ~ '@' ~ (emailCharR.+ ~ '.').+ ~ emailCharR.+) ~> t.EmailAddress
    ) //~ EOT)

    def mathtex = rule(
      surround(G.mathTexSurround) ~> (_.trim |> t.MathTeX)
    ) //~ EOT)

    def plainTextMarkup =
      rule( webAddress | emailAddress | mathtex )
  }

  trait NewLine extends Base {
    override type T <: Atom.NewLine
    def newLine = rule( "\n" ~ push(t.NewLine()) )
  }

  trait ReqRef extends Base {
    override type T <: Atom.ReqRef

    def reqRef: Rule1[t.ReqRef] = rule(
      G.reflinkPrefix ~ ows ~ reqTypeMnemonic ~ ows ~ ('-' ~ ows).? ~ reqTypePos ~ ows ~ G.reflinkSuffix
        //~ EOT
        ~> (lookupReq(_, _) |> pushOptional) ~> t.ReqRef
    )
  }

  trait TagRef extends Base {
    override type T <: Atom.TagRef
    def tagRef = runPF[HashRefTarget, t.TagRef] {
      case -\/(tag) => t.TagRef(tag.id)
    }
  }

  trait Issue extends Base {
    override type T <: Atom.Issue
    import Text.{InlineIssueDesc => I}

    def issueRef: RuleAB[HashRefTarget, t.Issue] = {
      def id           = runPF[HashRefTarget, CustomIssueType.Id] { case \/-(i) => i.id }
      def optionalDesc = rule(issueInnerDesc ~> (_.whole) | push(Vector.empty))
      rule(run(id) ~ optionalDesc ~> t.Issue)
    }

    // Hack due to https://github.com/sirthias/parboiled2/issues/120
    // runSubParser can only be used in a method directly in a class, not a trait like this
    protected def issueInnerDesc: Rule1[I.NonEmptyText] //= rule(runSubParser(I.parserI(project)(_).inline))
  }

  // ===================================================================================================================

  trait SingleLine extends PlainTextMarkup with Literal {
    override type T <: Atom.PlainTextMarkup with Atom.Literal
    def singleLine = plainTextMarkup
  }

  abstract class TopBase[_T <: Atom.Literal](_t: _T) extends Literal {
    override final type T = _T
    override final val  t: T = _t
    final type TokenRule = () => Rule1[t.Atom]
    protected val token: TokenRule
  }

  abstract class DefaultOptional[_T <: Atom.Literal](_t: _T) extends TopBase(_t) {
    final def main = rule(optionalText(token) ~ EOI)
  }

  abstract class DefaultNonEmpty[_T <: Atom.Literal](_t: _T) extends TopBase(_t) {
    final def main = rule(nonEmptyText(token) ~ EOI)
  }

  abstract class ReqTitle[_T <: Atom.ReqTitle](_t: _T, val project: Project, val input: ParserInput) extends DefaultOptional(_t)
    with SingleLine
    with ReqRef
    with Issue {

    def hashToken = rule(hashRef ~ issueRef)
    val token = () => rule(hashToken | reqRef | singleLine)
  }
}