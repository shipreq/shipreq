package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.effect.IO
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{\/-, -\/}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.base.util.{SetDiff, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.TextSeqEditor._
import shipreq.webapp.client.lib.Plain

object ImplicationEditor {
  import AutoComplete.ReqItem
  import DataImplicits._

  val editor = textSetEditor[ReqId, SetDiff[ReqId]]("ImplicationEditor", Grammar.pubidSeqFormat.apply)

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

  def lookupForCustomImpCol(p: Project, l: Lookup, fid: CustomField.Implication.Id): Must[Lookup] =
    p.customField(fid).map(f =>
      l.outlaw(None, _.reqTypeId ≠ f.reqTypeId))

  def initialValueForCustomColumn(p: Project, fid: CustomField.Implication.Id, id: ReqId): Stream[Pubid] = {
    val impIds = p.reqFieldData.data.implications.tgtToSrc(id).toStream
    val impReqs = unmust(p.reqs.data.reqsM(impIds))
    impReqs.map(_.pubid)
  }

  def apply(initial0  : Seq[Pubid],
            subjectId : ReqId,
            column    : Column,
            project   : Px[Project],
            textSearch: Px[TextSearch],
            lookupM   : Px[Must[Lookup]],
            setState  : Option[Cell.State] => IO[Unit]): Cell.State = {

    /**
     * If true, the user edits what this subject implies (ie. subject → edit-specified).
     * If false, then it's what implies this subject     (ie. subject ← edit-specified).
     */
    val declFwd = column match {
      case Column.ImplicationTgt => true
      case _                     => false
    }

    val (initialValues, initialTextValue) = {
      val p = project.value()

      val reqs = unmust(
        for {
          lu ← lookupM.value()
          ls = lu.legal.map(_.req.id).toSet - subjectId
          rs ← p.reqs.data.reqsByPubidM(initial0.toVector)
        } yield rs.filter(ls contains _.id))

      val text = reqs
        .map(r => UiText mustA PlainText.pubid(p, r.pubid))
        .sorted
        .mkString(" ")

      (reqs.map(_.id).toSet, text)
    }

    val lookup = lookupM.map(mustResolve(_)(Lookup(Stream.empty, UnivEq.emptyMap)))

    val autoComplete: Px[AutoComplete] =
      for {
        l <- lookup
        s <- textSearch
      } yield ReusableVal.byRef(
        AutoComplete.req(s, l.legal, Plain))

    val parser: Parser[ReqId] = () => {
      val l = lookup.value()
      s =>
        l.legalm.get(s).map(_.req.id.right) orElse
          l.illegal.get(s).map(-\/.apply) getOrElse
          leftNone
    }

    val abort: IO[Unit] =
      setState(None)

    val validate: Vector[ReqId] => ParseResult[SetDiff[ReqId]] = in => {
      val newValues = in.toSet - subjectId // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = project.value().reqFieldData.data.implications
      var is = if (declFwd) pi.srcToTgt else pi.tgtToSrc
      is = is.mod(subjectId, diff.apply)
      if (ReqFieldData.implicationCycleDetector.hasCycle(is.m))
        -\/(Some("That would cause a cycle in your implication graph."))
      else
        \/-(diff)
    }

    val commit: SetDiff[ReqId] => IO[Unit] =
    // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ if (s.nonEmpty) println("Sent to ze server: " + s) }

    Cell.selfManage(setState, initialTextValue)(
      editor.Props(_, _, abort, parser, validate, commit, autoComplete.value()).apply)
  }
}
