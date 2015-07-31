package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{\/-, -\/}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{SetDiff, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.RemoteDataEditor
import shipreq.webapp.client.app.ui.TextSeqEditor
import shipreq.webapp.client.lib.ui.TextEditor
import shipreq.webapp.client.lib.Plain
import TextSeqEditor._

object ImplicationEditor {
  import AutoComplete.ReqItem
  import DataImplicits._

  type ImpDiff = SetDiff[ReqId]

  val editor = new TextSeqEditor[ReqId, ImpDiff](
    "ImplicationEditor", Grammar.pubidSeqFormat.apply, TextEditor.Input)

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
    p.config.customField(fid).map(f =>
      l.outlaw(None, _.reqTypeId ≠ f.reqTypeId))

  def initialValueForCustomColumn(p: Project, fid: CustomField.Implication.Id, id: ReqId): Stream[Pubid] = {
    val impIds = p.implications.tgtToSrc(id).toStream
    val impReqs = unmust(p.reqs.reqsM(impIds))
    impReqs.map(_.pubid)
  }

  /**
   * If true, the user edits what this subject implies (ie. subject → edit-specified).
   * If false, then it's what implies this subject     (ie. subject ← edit-specified).
   */
  def isDeclFwd(column: Column): Boolean =
    column match {
      case Column.ImplicationTgt => true
      case _                     => false
    }

  def apply(initial   : Option[(ReqId, Seq[Pubid])],
            column    : Column,
            project   : Px[Project],
            textSearch: Px[TextSearch],
            lookupM   : Px[Must[Lookup]],
            setSelf   : RemoteDataEditor.SetOpStateFor[String],
            commitFn  : ImpDiff => RemoteDataEditor.OnCommit): RemoteDataEditor.StateFor[String] = {

    val declFwd = isDeclFwd(column)

    val (initialValues, initialTextValue) = {
      val p = project.value()

      val reqs = unmust(
        for {
          lu ← lookupM.value()
          ls = initial.foldLeft(lu.legal.map(_.req.id).toSet)(_ - _._1)
          rs ← p.reqs.reqsByPubidM(initial.fold(Vector.empty[Pubid])(_._2.toVector))
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

    val onCommit = RemoteDataEditor.CommitFilter(commitFn).ignore(_.isEmpty)

    val validate: Vector[ReqId] => ParseResult[ImpDiff] = in => {
      val newValues = initial.foldLeft(in.toSet)(_ - _._1) // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = project.value().implications
      var is = if (declFwd) pi.srcToTgt else pi.tgtToSrc
      for (i <- initial)
        is = is.mod(i._1, diff.apply)
      if (Implications.cycleDetector.hasCycle(is.m))
        -\/(Some("That would cause a cycle in your implication graph."))
      else
        \/-(diff)
    }

    RemoteDataEditor.default[String, String](
      initialTextValue, identity, setSelf,
      (s, u, abort, commit) =>
        editor.Props(s, u, abort, parser, validate, v => commit(onCommit(v)), autoComplete.value(), cellStyle, cellErrorMsgStyle).apply)
  }

  def edit(subjectId : ReqId,
           initial   : Seq[Pubid],
           column    : Column,
           project   : Px[Project],
           textSearch: Px[TextSearch],
           lookupM   : Px[Must[Lookup]],
           setSelf   : RemoteDataEditor.SetOpStateFor[String],
           commitFn  : UpdateContentOnCommit): RemoteDataEditor.StateFor[String] = {

    val declFwd = isDeclFwd(column)

    val onCommit = {
      import UpdateContentCmd._
      val f: ImpDiff => UpdateContentCmd =
        if (declFwd)
          PatchImplicationTgt(subjectId, _)
        else
          PatchImplicationSrc(subjectId, _)
      commitFn cmap f
    }

    apply(Some((subjectId, initial)), column, project, textSearch, lookupM, setSelf,  onCommit)
  }
}
