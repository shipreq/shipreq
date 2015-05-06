package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import scalacss.ScalaCssReact._
import scalacss.StyleA
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}

final class ColumnRenderer(
  val column     : Column,
  val header     : ReactElement,
  val render     : Row => ReactElement,
  val columnStyle: Option[StyleA])

object ColumnRenderer {
  val `N/A`: ReactElement = <.span(*.`N/A`, "–")
  val empty: ReactElement = <.span
}

class ColumnRenderers(project: Project, columnName: Column.NameResolver, widgets: ProjectWidgets) {
  import ColumnRenderer._

  def apply(c: Column): ColumnRenderer = {
    val cr: Column => ColumnRenderer = 
      c match {
        case Column.Pubid          => pubid
        case Column.ReqType        => reqType
        case Column.Code           => code
        case Column.Title          => title
        case Column.Tags           => tags(Row.tags)
        case Column.ImplicationSrc => imps(Row.implicationSrc) //("… ⇒")
        case Column.ImplicationTgt => imps(Row.implicationTgt) //("⇒ …")
        case Column.CustomField(f) =>
          f match {
            case id: CustomField.Text       .Id => cfText(id)
            case id: CustomField.Tag        .Id => tags(Row cfTag id)
            case id: CustomField.Implication.Id => imps(Row cfImp id)
          }
      }
    cr(c)
  }

  private def make(render: Row => ReactElement, columnStyle: Option[StyleA] = None): Column => ColumnRenderer =
    c => makeS(columnName(c), render, columnStyle)(c)

  private def makeS(headerName: String, render: Row => ReactElement, columnStyle: Option[StyleA] = None): Column => ColumnRenderer =
    c => new ColumnRenderer(c, <.span(headerName), render, columnStyle)

  private def maybeEmpty[A](lens: Optional[Row, Vector[A]], r: Row)(f: Vector[A] => ReactElement): ReactElement =
    lens.getOption(r).filter(_.nonEmpty).fold(empty)(f)

  @deprecated("placeholder is for dev purposes only.", "")
  private def placeholder =
    makeS("∅", Function const <.span("∅"))

  private def pubid = make {
    case GenericReqRow(req, _, _) => widgets.pubidText(req.pubid)()
    case _: ReqCodeGroupRow       => `N/A`
  }

  private def reqType = make {
    case GenericReqRow(req, _, _) => widgets.reqType(req.reqTypeId)()
    case _: ReqCodeGroupRow       => `N/A`
  }

  private def code = make {
    case GenericReqRow(_, exp, _)           => widgets.reqCodes(exp.reqCodeTree, exp.reqCodes)
    case ReqCodeGroupRow(id, _, _, Some(t)) => widgets.reqCodeTreeItem(t)
    case ReqCodeGroupRow(id, _, c, None)    => widgets.flatReqCode(c)
  }

  private def title = make {
    case GenericReqRow(req, _, _)     => widgets.reqTitle(req)
    case ReqCodeGroupRow(id, g, _, _) => widgets.reqCodeGroupTitle(id, g)
  }

  private def imps(lens: Optional[Row, Vector[Pubid]]) = make {
    case r: GenericReqRow   => maybeEmpty(lens, r)(widgets.pubidRefs)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def tags(lens: Optional[Row, Vector[ApplicableTagId]]) = make {
    case r: GenericReqRow   => maybeEmpty(lens, r)(widgets.tags)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def cfText(id: CustomField.Text.Id) = {
    val f = widgets.customTextField(id)
    make {
      case GenericReqRow(req, _, _) => f(req.id).fold(empty)(w => w)
      case _: ReqCodeGroupRow       => `N/A`
    }
  }
}
