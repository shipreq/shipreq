package shipreq.webapp.base.text

import scala.annotation.tailrec
import shipreq.base.util.{NonEmptyVector, Must}
import shipreq.base.util.SafeStringOps._
import shipreq.webapp.base.UiText.Unmust
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.util.ReqCodeTreeItem
import Atom.AnyAtom

/**
 * Turns elements of data into user-facing plain text.
 */
object PlainText {

  // ScalaJS StringBuilder just uses String concatenation so fuck it.

  private implicit def surroundDisplay(s: Grammar.Surrounds) = s.display

  private implicit class OptionalTextOps[T <: Text.Generic](val _t: T#OptionalText) extends AnyVal {
    @inline def asOption: Option[_t.type] =
      if (_t.isEmpty) None else Some[_t.type](_t)

    @inline def net: Option[T#NonEmptyText] =
      NonEmptyVector.option(_t)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def reqCodeIndentation(is: NonEmptyVector[ReqCodeTreeItem.Indent]): String = {
    import ReqCodeTreeItem._
    val I = "│"
    is.reduceMapLeft1({
      case IndentChild    => I
      case IndentSpace(l) => I ~ (" " * (l - 1))
    })(_ ~ ' ' ~ _)
  }

  def reqCodeTreeItem(ti: ReqCodeTreeItem): String =
    NonEmptyVector.option(ti.indent) match {
      case None     => reqCode(ti.suffix)
      case Some(is) => reqCodeIndentation(is) ~ G.reqCode.nodeSeparator ~ reqCode(ti.suffix)
    }

  def reqCode(c: ReqCode.Value) = c.reduceMapLeft1(_.value)(_ ~ G.reqCode.nodeSeparator ~ _)

  def hashtag(key: HashRefKey) = G.hashRefKey.prefix ~ key.value

  def pubid(p: Project, id: ReqId): Must[String] =
    p.reqs.data.reqM(id)
      .flatMap(r => pubid(p, r.pubid))

  def pubid(p: Project, pid: Pubid): Must[String] =
    p.reqType(pid.reqTypeId).map(pubid(_, pid.pos))

  def pubid(reqType: ReqType, pos: ReqTypePos): String =
    reqType.mnemonic.value ~ "-" ~ pos.value

  // -------------------------------------------------------------------------------------------------------------------

  type ForProject = ProjectText[String]

  def apply(p: Project): ForProject = {

    def codeRef(id: ReqCodeId): Must[String] = {
      import Must.Auto._
      import ProjectText.ReqCodeResolution._
      ProjectText.resolveReqCode(id, p.reqCodes.data).flatMap {
        case ActiveCode(c, _)     => G reflinkSurround reqCode(c)
        case DeadGroup(c)         => G reflinkSurround reqCode(c)
        case ReqWithAltCode(c, _) => G reflinkSurround reqCode(c)
        case ReqWithoutCodes(r)   => reqRef(r)
      }
    }

    def reqRef(req: ReqId): Must[String] =
      for {
        r   ← p.reqs.data.reqM(req)
        pid = r.pubid
        rt  ← p.reqType(pid.reqTypeId)
      } yield
      G.reflinkSurround(pubid(rt, pid.pos))

    def tagRef(id: ApplicableTagId): Must[String] =
      p.atag(id).map(t => hashtag(t.key))

    def issue(id: CustomIssueTypeId, desc: Option[String]): Must[String] =
      p.customIssueType(id).map(it =>
        (hashtag(it.key) /: desc)(_ ~ G.issueDescSurround(_)))

    val outOfListNewline = "\n\n"

    val format: Text.AnyOptional => String = {
      def nest(acc: String, newline: String, atoms: Vector[AnyAtom]): String = {
        @tailrec def go(acc: String, atoms: Vector[AnyAtom]): String =
          if (atoms.isEmpty)
            acc
          else {
            import Atom._
            val cur = atoms.head match {
              case a: Literal         # Literal       => a.value
              case a: NewLine         # BlankLine     => newline
              case a: ReqRef          # ReqRef        => reqRef(a.value).unmust
              case a: ReqRef          # CodeRef       => codeRef(a.value).unmust
              case a: Issue           # Issue         => issue(a.typ, a.desc.asOption map run).unmust
              case a: PlainTextMarkup # WebAddress    => a.value
              case a: PlainTextMarkup # EmailAddress  => a.value
              case a: PlainTextMarkup # MathTeX       => G.mathTexSurround(a.value)
              case a: TagRef          # TagRef        => tagRef(a.value).unmust
              case a: ListMarkup      # UnorderedList =>
                val newline2 = if (newline eq outOfListNewline) "\n  " else newline ~ "  "
                a.items.foldLeft("")((q, li) => nest(s"$q${newline}* ", newline2, li)) ~ newline
            }
            go(acc ~ cur, atoms.tail)
          }

        go(acc, atoms)
      }
      @inline def run: Text.AnyOptional => String = nest("", outOfListNewline, _)
      run
    }

    ProjectText(p, format)
  }
}
