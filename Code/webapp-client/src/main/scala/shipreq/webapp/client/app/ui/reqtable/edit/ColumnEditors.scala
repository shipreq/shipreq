package shipreq.webapp.client.app.ui.reqtable
package edit

import monocle.Optional
import scalaz.effect.IO
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Must, Px}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.ProjectWidgets

final class ColumnEditors(project       : Px[Project],
                          plainText     : Px[PlainText.ForProject],
                          projectWidgets: Px[ProjectWidgets],
                          textSearch    : Px[TextSearch],
                          cellset       : Cell.SetIO) {

  type State                = Option[Cell.State]
  type SetState             = State => IO[Unit]
  type InitState            = SetState => State
  type InitEditor[R <: Row] = R => InitState

  private def noEditor: InitState =
    _ => None

  @inline private implicit def autoSome(c: Cell.State): Option[Cell.State] =
    Some(c)

  @inline private def initEditor[R <: Row](f: R => SetState => Option[Cell.State]): InitEditor[R] =
    f

  private def initEditorO[R <: Row](f: R => Option[SetState => Cell.State]): InitEditor[R] =
    r => f(r).fold[SetState => Option[Cell.State]](_ => None)(g => g(_).some)

  def startCellEditing(row: Row, col: Column): Option[IO[Unit]] = {
    val init: InitState =
      row match {

        case r: GenericReqRow =>
          col match {
            //case Column.Code           => 
            case Column.Title          => genericReqTitle(r)
            case Column.Tags           => tags(r)
            case Column.Pubid          => noEditor
            case Column.ImplicationSrc => imps(Row.implicationSrc, ImplicationEditor declFwd Column.ImplicationSrc)(r)
            case Column.ImplicationTgt => imps(Row.implicationTgt, ImplicationEditor declFwd Column.ImplicationTgt)(r)
            case Column.CustomField(f) =>
              f match {
                case id: CustomField.Text       .Id => cfText(id)(r)
                case id: CustomField.Tag        .Id => cfTag(id)(r)
                case id: CustomField.Implication.Id => cfImp(id)(r)
              }
          }

        case r: ReqCodeGroupRow =>
          col match {
            //case Column.Code           => 
            case Column.Title          => reqCodeGroupTitle(r)
            case Column.Pubid
               | Column.ReqType
               | Column.Tags
               | Column.ImplicationSrc
               | Column.ImplicationTgt
               | Column.CustomField(_) => noEditor
          }
      }

    val setState: SetState =
      s => cellset(Cell.SetCmd(row.id, col, s))

    val initialState: State =
      init(setState)

    initialState.map(_ => setState(initialState))
  }

  val reqCodeGroupTitle = initEditor[ReqCodeGroupRow](r =>
    RichTextEditor.ReqCodeGroupTitle(r.group.title, project, plainText, projectWidgets, textSearch, _))

  val genericReqTitle = initEditor[GenericReqRow](r =>
    RichTextEditor.GenericReqTitle(r.req.title, project, plainText, projectWidgets, textSearch, _))

  val tags = initEditor[GenericReqRow] { r =>
    val lookup = project map TagEditor.lookupForNoCol
    TagEditor(r.mv.tags, project.value(), lookup, _)
  }

  def cfTag(id: CustomField.Tag.Id) = initEditor[GenericReqRow] { r =>
    val lookup = project map (TagEditor.lookupForCol(_, id))
    TagEditor(r.exp.tagsForCF(id), project.value(), lookup, _)
  }

  lazy val impsLookup =
    Px.apply2(project, plainText)(ImplicationEditor.lookupAll)

  def imps(l: Optional[Row, Vector[Pubid]], declFwd : Boolean) = initEditorO[GenericReqRow] { r =>
    l.getOption(r).map { initialValue =>
      val lookup2 = for {p <- project; l <- impsLookup}
        yield Must(ImplicationEditor.lookupForSubject(p, l, r.req.id, declFwd))
      ImplicationEditor(initialValue, project, textSearch, lookup2, _)
    }
  }

  def cfImp(id: CustomField.Implication.Id) = initEditorO[GenericReqRow] { r =>
    val lookup2 = for {p <- project; l <- impsLookup} yield ImplicationEditor.lookupForCol(p, l, id)
    Row.cfImp(id).getOption(r).map { initialValue =>
      val declFwd = ImplicationEditor declFwd id
      val lookup3 = for {p <- project; lm <- lookup2}
        yield lm.map(ImplicationEditor.lookupForSubject(p, _, r.req.id, declFwd))
      ImplicationEditor(initialValue, project, textSearch, lookup3, _)
    }
  }

  def cfText(id: CustomField.Text.Id) = initEditor[GenericReqRow] { r =>
    val textData = project.value().reqFieldData.data.text.getOrElse(id, Map.empty)
    val initialValue = textData.get(r.req.id).map(_.whole) getOrElse Vector.empty
    RichTextEditor.CustomTextField(initialValue, project, plainText, projectWidgets, textSearch, _)
  }
}
