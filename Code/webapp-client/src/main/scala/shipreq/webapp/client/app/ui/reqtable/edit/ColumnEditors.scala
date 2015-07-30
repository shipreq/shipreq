package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.Px
import monocle.Optional
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import shipreq.base.util.Must
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.{ProjectWidgets, RemoteDataEditor}
import shipreq.webapp.client.lib.TIO

object ColumnEditors {
  case class CellEditor(init: RemoteDataEditor.SetOpState => Option[Cell.State]) extends AnyVal

  def noEditor = CellEditor(_ => None)
}

final class ColumnEditors(project       : Px[Project],
                          plainText     : Px[PlainText.ForProject],
                          projectWidgets: Px[ProjectWidgets],
                          textSearch    : Px[TextSearch],
                          modTable      : Cell.ModTable,
                          saveIO        : (UpdateContentCmd, TIO.Success, TIO.Failure) => IO[Unit]) {

  import ColumnEditors._

  private val applicability = project.map(Applicability.apply)

  def startCellEditing(row: Row, col: Column): Option[IO[Unit]] =
    row.live match {
      case Live => startCellEditing2(row, col)
      case Dead => None
    }

  private def startCellEditing2(row: Row, col: Column): Option[IO[Unit]] = {
    val editor: CellEditor =
      applicability.value().apply(col).choose(row, na = noEditor)(
        row match {

          case r: GenericReqRow =>
            col match {
              case Column.Code           => codesForReq(r)
              case Column.Title          => genericReqTitle(r)
              case Column.Tags           => tags(r)
              case Column.ReqType        => reqType(r)
              case Column.Pubid          => noEditor
              case Column.ImplicationSrc => imps(Row.implicationSrc, col)(r)
              case Column.ImplicationTgt => imps(Row.implicationTgt, col)(r)
              case Column.CustomField(f, _) =>
                f match {
                  case id: CustomField.Text       .Id => cfText(id)(r)
                  case id: CustomField.Tag        .Id => cfTag(id)(r)
                  case id: CustomField.Implication.Id => cfImp(id, col)(r)
                }
            }

          case r: ReqCodeGroupRow =>
            col match {
              case Column.Code           => codeForGroup(r)
              case Column.Title          => reqCodeGroupTitle(r)
              case Column.Pubid
                 | Column.ReqType
                 | Column.Tags
                 | Column.ImplicationSrc
                 | Column.ImplicationTgt
                 | _: Column.CustomField => noEditor
            }
        }
      )

    val loc = Cell.Loc(row.id, col)

    val modCell = modTable(loc)

    editor.init(modCell) match {
      case Some(cmd) => Some(modCell(cmd))
      case None      => None // init returns None = editing not allowed = nothing to do
    }
  }

  // ===================================================================================================================

  private val updateContentOnCommit: UpdateContentOnCommit =
    RemoteDataEditor.CommitFilter(
      cmd => cb =>
        cb.lock >> saveIO(cmd, cb.succeeded, cb.failed))

  private def mkEditor[R <: Row](f: R => (RemoteDataEditor.SetOpState, UpdateContentOnCommit) => Cell.State) =
    mkEditorO[R](r => Some(f(r)))

  private def mkEditorO[R <: Row](f: R => Option[(RemoteDataEditor.SetOpState, UpdateContentOnCommit) => Cell.State]): R => CellEditor =
    r => f(r) match {
      case Some(g) => CellEditor(m => Some(g(m, updateContentOnCommit)))
      case None    => noEditor
    }

  // ===================================================================================================================

  val reqType = mkEditorO[GenericReqRow] { r =>
    val initialM = project.value().config.reqTypeC(r.req.reqTypeId)
    mustResolveO(initialM).map { iv =>
      val id = r.req.id
      val fields = project.map(_.config.customReqTypes.values.toSet)
      ReqTypeSelector(iv, id, fields)
    }
  }

  val genericReqTitle = mkEditor[GenericReqRow] { r =>
    val id = r.req.id
    val iv = r.req.title
    RichTextEditor.GenericReqTitle(iv, id, project, plainText, projectWidgets, textSearch)
  }

  val reqCodeGroupTitle = mkEditor[ReqCodeGroupRow] { r =>
    val id = r.reqCodeId
    val iv = r.group.title
    RichTextEditor.ReqCodeGroupTitle(iv, id, project, plainText, projectWidgets, textSearch)
  }

  def cfText(fid: CustomField.Text.Id) = mkEditor[GenericReqRow] { r =>
    val td = project.value().reqText.getOrElse(fid, Map.empty)
    val id = r.req.id
    val iv = td.get(id).map(_.whole) getOrElse Vector.empty
    val fe = new RichTextEditor.CustomTextField(fid)
    fe(iv, id, project, plainText, projectWidgets, textSearch)
  }

  val codesForReq = mkEditor[GenericReqRow] { r =>
    val id = r.req.id
    val iv = project.value().reqCodes.activeReqCodesByTarget(r.req.id)
    val vs = project.map(p => Validators.reqCode.VS(p.reqCodes.trie, iv))
    ReqCodeEditor.ForReqs(iv, id, vs)
  }

  val codeForGroup = mkEditor[ReqCodeGroupRow] { r =>
    val id = r.reqCodeId
    val iv = r.reqCode
    val vs = project.map(p => Validators.reqCode.VS(p.reqCodes.trie, Set(iv)))
    ReqCodeEditor.ForGroup(iv, id, vs)
  }

  val tags = mkEditor[GenericReqRow] { r =>
    val id = r.req.id
    val l  = project map TagEditor.lookupForNoCol
    val p  = project.value()
    val iv = p.reqTags(id)
    TagEditor(iv, id, p, l)
  }

  def cfTag(fid: CustomField.Tag.Id) = mkEditor[GenericReqRow] { r =>
    val id = r.req.id
    val l  = project map (TagEditor.lookupForCol(_, fid))
    val p  = project.value()
    val iv = p.reqTags(id) & r.exp.tagsForCF(fid).toSet
    TagEditor(iv, id, p, l)
  }

  lazy val impLookup =
    Px.apply2(project, plainText)(ImplicationEditor.lookupAll)

  def imps(l: Optional[Row, Vector[Pubid]], col: Column) = mkEditorO[GenericReqRow](r =>
    l.getOption(r).map(iv =>
      ImplicationEditor(iv, r.req.id, col, project, textSearch, impLookup map Must.apply)))

  def cfImp(fid: CustomField.Implication.Id, col: Column) = mkEditorO[GenericReqRow] { r =>
    val lookup = for {p <- project; l <- impLookup} yield ImplicationEditor.lookupForCustomImpCol(p, l, fid)
    Row.cfImp(fid).getOption(r).map { _ =>
      val id = r.req.id
      val iv = ImplicationEditor.initialValueForCustomColumn(project.value(), fid, id)
      ImplicationEditor(iv, id, col, project, textSearch, lookup)
    }
  }
}
