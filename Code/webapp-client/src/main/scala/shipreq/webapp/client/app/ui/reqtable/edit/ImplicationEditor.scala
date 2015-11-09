package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{\/-, -\/}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{SetDiff, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.{RemoteDataEditor, TextSeqEditor, VUCA}
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

  def lookupForCustomImpCol(p: Project, l: Lookup, fid: CustomField.Implication.Id): Lookup = {
    val f = p.config.customField(fid)
    l.outlaw(None, _.reqTypeId ≠ f.reqTypeId)
  }

  def initialValueForCustomColumn(p: Project, fid: CustomField.Implication.Id, id: ReqId): Stream[Pubid] =
    p.implications.backwards(id)
      .toStream
      .map(p.reqs.req(_).pubid)

  /**
   * If true, the user edits what this subject implies (ie. subject → edit-specified).
   * If false, then it's what implies this subject     (ie. subject ← edit-specified).
   */
  def isDeclFwd(column: Column): Boolean =
    column match {
      case Column.ImplicationTgt => true
      case _                     => false
    }

  def prepare(initial   : Option[(ReqId, Seq[Pubid])],
              column    : Column,
              project   : Px[Project],
              textSearch: Px[TextSearch],
              lookup    : Px[Lookup]): (VUCA[String, ImpDiff] => editor.Props, String) = {

    val declFwd = isDeclFwd(column)

    val (initialValues, initialTextValue) = {
      val p = project.value()

      val reqs = {
        val legal = initial.foldLeft(lookup.value().legal.map(_.req.id).toSet)(_ - _._1)
        initial.fold(Stream.empty[Pubid])(_._2.toStream)
          .map(p.reqs.reqByPubid)
          .filter(legal contains _.id)
      }


      val text = reqs
        .map(r => PlainText.pubid(p, r.pubid))
        .sorted
        .mkString(" ")

      (reqs.map(_.id).toSet, text)
    }

    val autoComplete: Px[AutoComplete] =
      for {
        l <- lookup
        s <- textSearch
      } yield ReusableVal.byRef(
        AutoComplete.req(s, l.legal, Plain))

    val parser: Parser[ReqId] = s => {
      val l = lookup.value()
      l.legalm.get(s).map(_.req.id.right) orElse
        l.illegal.get(s).map(-\/.apply) getOrElse
        leftNone
    }

    val validate: Vector[ReqId] => ParseResult[ImpDiff] = in => {
      val newValues = initial.foldLeft(in.toSet)(_ - _._1) // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = project.value().implications
      var is = pi.dir(declFwd)
      for (i <- initial)
        is = is.mod(i._1, diff.apply)
      if (Implications.cycleDetector.hasCycle(is.m))
        -\/(Some("That would cause a cycle in your implication graph."))
      else
        \/-(diff)
    }

    (editor.Props(_, parser, validate, autoComplete.value(), cellStyle, cellErrorMsgStyle), initialTextValue)
  }

  def selfManaged(initial   : Option[(ReqId, Seq[Pubid])],
                  column    : Column,
                  project   : Px[Project],
                  textSearch: Px[TextSearch],
                  lookup    : Px[Lookup],
                  commitFn  : ImpDiff => RemoteDataEditor.OnCommit): InitSelfManagedA[String] = {

    val (props, initialTextValue) = prepare(initial, column, project, textSearch, lookup)

    val onCommit = RemoteDataEditor.CommitFilter(commitFn).ignore(_.isEmpty)

    (initialTextValue, (s, u, a, commit) => props(VUCA(s, u, v => commit(onCommit(v)), a)).render)
  }

  def edit(subjectId : ReqId,
           initial   : Seq[Pubid],
           column    : Column,
           project   : Px[Project],
           textSearch: Px[TextSearch],
           lookup    : Px[Lookup],
           commitFn  : UpdateContentOnCommit): InitSelfManagedA[String] = {

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

    selfManaged(Some((subjectId, initial)), column, project, textSearch, lookup, onCommit)
  }
}
