package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Px
import monocle.Optional
import scalaz.syntax.bind.ToBindOps
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.{ProjectWidgets, RemoteDataEditor}
import shipreq.webapp.client.lib.TCB

object ColumnEditors {
  type CellEditor = RemoteDataEditor.SetOpState => Option[Cell.State]

  def noEditor: CellEditor =
    _ => None
}

final class ColumnEditors(project       : Px[Project],
                          plainText     : Px[PlainText.ForProject],
                          projectWidgets: Px[ProjectWidgets],
                          textSearch    : Px[TextSearch],
                          modTable      : Cell.ModTable,
                          saveIO        : (UpdateContentCmd, TCB.Success, TCB.Failure) => Callback) {

  import ColumnEditors._

  private val applicability = project.map(Applicability.apply)

  def startCellEditing(row: Row, col: Column, fin: TCB.Finally): Option[Callback] =
    row.live match {
      case Live => startCellEditing2(row, col, fin)
      case Dead => None
    }

  private def startCellEditing2(row: Row, col: Column, fin: TCB.Finally): Option[Callback] = {
    val editor: CellEditor =
      applicability.value().apply(col).choose(row, na = noEditor)(
        row match {

          case r: GenericReqRow =>
            col match {
              case Column.Code           => codesForReq(r, fin)
              case Column.Title          => genericReqTitle(r, fin)
              case Column.Tags           => tags(r, fin)
              case Column.ReqType        => reqType(r, fin)
              case Column.Pubid          => noEditor
              case Column.ImplicationSrc => imps(Row.implicationSrc, col)(r, fin)
              case Column.ImplicationTgt => imps(Row.implicationTgt, col)(r, fin)
              case Column.CustomField(f, _) =>
                f match {
                  case id: CustomField.Text       .Id => cfText(id)(r, fin)
                  case id: CustomField.Tag        .Id => cfTag(id)(r, fin)
                  case id: CustomField.Implication.Id => cfImp(id, col)(r, fin)
                }
            }

          case r: ReqCodeGroupRow =>
            col match {
              case Column.Code           => codeForGroup(r, fin)
              case Column.Title          => reqCodeGroupTitle(r, fin)
              case Column.Pubid
                 | Column.ReqType
                 | Column.Tags
                 | Column.ImplicationSrc
                 | Column.ImplicationTgt
                 | _: Column.CustomField => noEditor
            }
        }
      )

    val loc = Cell.Loc(row.sourceId, col)

    val modCell = modTable(loc)

    editor(modCell) match {
      case Some(cmd) => Some(modCell(cmd, Callback.empty))
      case None      => None // init returns None = editing not allowed = nothing to do
    }
  }

  // ===================================================================================================================

  private val updateContentOnCommit: UpdateContentOnCommit =
    RemoteDataEditor.CommitFilter(
      cmd => cb =>
        cb.lock >> saveIO(cmd, cb.succeeded, cb.failed))

  private def mkEditor[R <: Row, A](f: R => UpdateContentOnCommit => InitSelfManagedA[A]) =
    mkEditorO[R, A](r => Some(f(r)))

  private def mkEditorO[R <: Row, A](f: R => Option[UpdateContentOnCommit => InitSelfManagedA[A]]): (R, TCB.Finally) => CellEditor =
    (r, fin) => {
      import RemoteDataEditor._
      val pa: PostAbort = {
        case AbortFromEditor   => TCB.Abort(fin.cb)
        case AbortAfterSuccess => TCB.Abort.nop // Life and KB focus have moved on by this point
      }
      f(r) match {
        case Some(g) =>
          val i = g(updateContentOnCommit)
          setOpState => Some(default[A, A](i._1, a => a, setOpState, pa, fin.cb, i._2))
        case None => noEditor
      }
    }

  // ===================================================================================================================

  val reqType = mkEditor[GenericReqRow, CustomReqType] { r =>
    val iv = project.value().config.reqTypeC(r.req.reqTypeId)
    val id = r.req.id
    val fields = project.map(_.config.customReqTypes.values.toSet)
    ReqTypeSelector(iv, id, fields, _)
  }

  val genericReqTitle = mkEditor[GenericReqRow, String] { r =>
    val id = r.req.id
    val iv = r.req.title
    RichTextEditor.GenericReqTitle.edit(id, iv, project, plainText, projectWidgets, textSearch, _)
  }

  val reqCodeGroupTitle = mkEditor[ReqCodeGroupRow, String] { r =>
    val id = r.reqCodeId
    val iv = r.group.title
    RichTextEditor.ReqCodeGroupTitle.edit(id, iv, project, plainText, projectWidgets, textSearch, _)
  }

  def cfText(fid: CustomField.Text.Id) = mkEditor[GenericReqRow, String] { r =>
    val td = project.value().reqText.getOrElse(fid, Map.empty)
    val id = r.req.id
    val iv = td.get(id).map(_.whole) getOrElse Vector.empty
    val fe = new RichTextEditor.CustomTextField(fid)
    fe.edit(id, iv, project, plainText, projectWidgets, textSearch, _)
  }

  private val reqCodeTrie = project.map(_.reqCodes.trie)

  val codesForReq = mkEditor[GenericReqRow, String] { r =>
    val id = r.req.id
    val iv = project.value().reqCodes.activeReqCodesByTarget(r.req.id)
    ReqCodeEditor.ForReqs.edit(id, iv, reqCodeTrie, _)
  }

  val codeForGroup = mkEditor[ReqCodeGroupRow, String] { r =>
    val id   = r.reqCodeId
    val iv   = r.reqCode
    ReqCodeEditor.ForGroup.edit(id, iv, reqCodeTrie, _)
  }

  val tags = mkEditor[GenericReqRow, String] { r =>
    val id = r.req.id
    val l  = project map TagEditor.lookupForNoCol
    val p  = project.value()
    val iv = p.reqTags(id)
    TagEditor.edit(id, iv, p, l, _)
  }

  def cfTag(fid: CustomField.Tag.Id) = mkEditor[GenericReqRow, String] { r =>
    val id = r.req.id
    val l  = project map (TagEditor.lookupForCol(_, fid))
    val p  = project.value()
    val iv = p.reqTags(id) & r.exp.tagsForCF(fid).toSet
    TagEditor.edit(id, iv, p, l, _)
  }

  lazy val impLookup =
    Px.apply2(project, plainText)(ImplicationEditor.lookupAll)

  def imps(l: Optional[Row, Vector[Pubid]], col: Column) = mkEditorO[GenericReqRow, String](r =>
    l.getOption(r).map(iv =>
      ImplicationEditor.edit(r.req.id, iv, col, project, textSearch, impLookup, _)))

  def cfImp(fid: CustomField.Implication.Id, col: Column) = mkEditorO[GenericReqRow, String] { r =>
    val lookup = for {p <- project; l <- impLookup} yield ImplicationEditor.lookupForCustomImpCol(p, l, fid)
    Row.cfImp(fid).getOption(r).map { _ =>
      val id = r.req.id
      val iv = ImplicationEditor.initialValueForCustomColumn(project.value(), fid, id)
      ImplicationEditor.edit(id, iv, col, project, textSearch, lookup, _)
    }
  }
}
