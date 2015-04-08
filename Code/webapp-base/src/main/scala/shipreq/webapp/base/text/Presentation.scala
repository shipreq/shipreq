package shipreq.webapp.base.text

import scala.annotation.tailrec
import shipreq.base.util.{NonEmptyVector, Must}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import Text._

/**
 * That which affects users' presentation of their requirements, belongs here.
 *
 * Examples:
 * - Token formatting. `[FR-34]`, `[fr-34]` in red when deleted.
 */
object Presentation {

  // ScalaJS StringBuilder just uses String concatenation so fuck it.

  private implicit class StringOps(val _s: String) extends AnyVal {
    @inline def ~(b: String): String = _s + b
  }

  private implicit class MustOps(val _m: Must[String]) extends AnyVal {
    @inline def unmust: String = UiText.mustA(_m)
  }

  private implicit def surroundDisplay(s: Grammar.Surrounds) = s.display

  private implicit class OptionalTextOps[T <: Generic](val _t: T#OptionalText) extends AnyVal {
    @inline def asOption: Option[_t.type] =
      if (_t.isEmpty) None else Some[_t.type](_t)

    @inline def net: Option[T#NonEmptyText] =
      NonEmptyVector.option(_t)
  }

  // -------------------------------------------------------------------------------------------------------------------

  /**
   * A "reflink" is a token that refers to another (sub-)requirement.
   * It can usually be clicked on to navigate to the referenced content.
   */
  def reflink(in: String) = G.reflinkPrefix ~ in ~ G.reflinkSuffix

  def hashtag(key: HashRefKey) = G.hashRefKey.prefix ~ key.value

  // -------------------------------------------------------------------------------------------------------------------

  def pubid(pid: Pubid)(implicit project: Project): Must[String] =
    project.reqType(pid.reqTypeId).map(pubid(_, pid.pos))

  def pubid(reqType: ReqType, pos: ReqTypePos): String =
    reqType.mnemonic.value ~ "-" ~ pos.value.toString

  def reqRef(req: Req.Id)(implicit p: Project): Must[String] =
    for {
      r   ← p.reqs.data.reqM(req)
      pid = r.pubid
      rt  ← p.reqType(pid.reqTypeId)
    } yield
      reflink(pubid(rt, pid.pos))

  def tagRef(id: ApplicableTag.Id)(implicit p: Project): Must[String] =
    p.atag(id).map(t => hashtag(t.key))

  def issue(id: CustomIssueType.Id, desc: Option[String])(implicit p: Project): Must[String] =
    p.customIssueType(id).map(it =>
      (hashtag(it.key) /: desc)(_ ~ G.issueDescSurround(_)))

  // -------------------------------------------------------------------------------------------------------------------

  def textToString(implicit p: Project): Generic#OptionalText => String = {
    import Generic._

    def nest(acc: String, newline: String, atoms: Vector[Generic#Atom]): String = {
      @tailrec def go(acc: String, atoms: Vector[Generic#Atom]): String =
        if (atoms.isEmpty)
          acc
        else {
          val cur = atoms.head match {
            case a: Literal         # Literal       => a.value
            case a: NewLine         # NewLine       => newline
            case a: ReqRef          # ReqRef        => reqRef(a.value).unmust
            case a: Issue           # Issue         => issue(a.typ, a.desc.asOption map run).unmust
            case a: PlainTextMarkup # WebAddress    => a.value
            case a: PlainTextMarkup # EmailAddress  => a.value
            case a: PlainTextMarkup # MathTeX       => G.mathTexSurround(a.value)
            case a: TagRef          # TagRef        => tagRef(a.value).unmust
            case a: ListMarkup      # UnorderedList =>
              val newline2 = newline ~ "  "
              a.items.foldLeft("")((q, li) => nest(s"$q${newline}* ", newline2, li)) ~ newline
          }
          go(acc ~ cur, atoms.tail)
        }

      go(acc, atoms)
    }

    @inline def run: Generic#OptionalText => String = nest("", "\n", _)
    run
  }
}
