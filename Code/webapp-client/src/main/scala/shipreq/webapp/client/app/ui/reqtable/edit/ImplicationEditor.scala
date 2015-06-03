package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.effect.IO
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{\/-, -\/}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.base.util.{Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.util.Plain

// TODO Hide dead reqs & maintain across edits (unless show deleted is on)
// TODO ImplicationEditor needs validation

object ImplicationEditor {
  import AutoComplete.ReqItem
  import DataImplicits._

  type A = ReqId

  val editor = textSetEditor[A]("ImplicationEditor", Grammar.pubidSeqFormat.apply)

  case class Lookup(legal: Stream[ReqItem], illegal: Map[String, ParseRejection]) {
    lazy val legalm = legal.map(_.mapStrengthL(_.pubidStrNorm)).toMap

    def outlaw(rej: ParseRejection, f: Req => Boolean): Lookup = {
      val (ko, ok) = legal.partition(i => f(i.req))
      val illegal2 = ko.foldLeft(illegal)((m, i) => m.updated(i.pubidStrNorm, rej))
      Lookup(ok, illegal2)
    }
  }

  def lookupAll(p: Project, pt: PlainText.ForProject): Lookup =
    Lookup(AutoComplete.reqItems(p, pt), UnivEq.emptyMap)

  def lookupForCol(p: Project, l: Lookup, fid: CustomField.Implication.Id): Must[Lookup] =
    p.customField(fid).map(f =>
      l.outlaw(None, _.reqTypeId ≠ f.reqTypeId))

  def declFwd(c: Column.ImplicationSrc.type): Boolean = false
  def declFwd(c: Column.ImplicationTgt.type): Boolean = true
  def declFwd(c: CustomField.Implication.Id): Boolean = false

  /**
   * @param declFwd If true, the user edits what this subject implies (ie. subject → edit-specified).
   *                If false, then it's what implies this subject     (ie. subject ← edit-specified).
   */
  def lookupForSubject(p: Project, l: Lookup, subject: ReqId, declFwd: Boolean): Lookup = {
    val (a, b) = if (declFwd) (p.implicationTgtToSrcTC, p.implicationSrcToTgtTC)
                 else         (p.implicationSrcToTgtTC, p.implicationTgtToSrcTC)
    l.outlaw(
      Some("That would cause a cycle in your implication graph."),
      r => a(subject).contains(r.id) || b(r.id).contains(subject))
  }

  def apply(initial   : Set[Pubid],
            project   : Px[Project],
            textSearch: Px[TextSearch],
            lookupM   : Px[Must[Lookup]],
            setState  : Option[Cell.State] => IO[Unit]): Cell.State = {

    def init: String = {
      val p = project.value()
      initial.toVector.map(pid =>
        UiText mustA PlainText.pubid(p, pid)
      ).sorted mkString " "
    }

    val lookup = lookupM.map(mustResolve(_)(Lookup(Stream.empty, UnivEq.emptyMap)))

    val autoComplete: Px[AutoComplete] =
      for {
        l <- lookup
        s <- textSearch
      } yield ReusableVal.byRef(
        AutoComplete.req(s, l.legal, Plain))

    val parser: Parser[A] = () => {
      val l = lookup.value()
      s =>
        l.legalm.get(s).map(_.req.id.right) orElse
          l.illegal.get(s).map(-\/.apply) getOrElse
            leftNone
    }

    val abort: IO[Unit] =
      setState(None)

    val commit: Set[A] => IO[Unit] =
    // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

    Cell.selfManage(setState, init)(
      editor.Props(_, _, abort, parser, toSetWithoutValidation, commit, autoComplete.value()).apply)
  }
}
