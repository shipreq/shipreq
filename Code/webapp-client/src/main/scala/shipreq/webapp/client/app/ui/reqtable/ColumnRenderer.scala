package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import monocle.function.index
import monocle.std.mapIndex
import japgolly.scalacss.ScalaCssReact._
import japgolly.scalacss.StyleA
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import DataImplicits._


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
        case Column.Desc           => desc
        case Column.Tags           => tags
        case Column.ImplicationSrc => imps(Row._implicationSrc) //("… ⇒")
        case Column.ImplicationTgt => imps(Row._implicationTgt) //("⇒ …")
        case Column.CustomField(f) =>
          f match {
            case id: CustomField.Text       .Id => cfText(id)
            case id: CustomField.Tag        .Id => cfTags(id)
            case id: CustomField.Implication.Id => imps(Row._cfImps ^|-? index(id))
          }
      }
    cr(c)
  }

  private def make(render: Row => ReactElement, columnStyle: Option[StyleA] = None): Column => ColumnRenderer =
    c => makeS(columnName(c), render, columnStyle)(c)

  private def makeS(headerName: String, render: Row => ReactElement, columnStyle: Option[StyleA] = None): Column => ColumnRenderer =
    c => new ColumnRenderer(c, <.span(headerName), render, columnStyle)

  @deprecated("placeholder is for dev purposes only.", "")
  private def placeholder =
    makeS("∅", Function const <.span("∅"))

  private def pubid = make {
    case GenericReqRow(req, _, _) => widgets.pubidText(req.pubid)()
  }

  private def reqType = make {
    case GenericReqRow(req, _, _) => widgets.reqType(req.reqTypeId)()
  }

  private def code = {
    def render(codes: Vector[ReqCode]): ReactElement =
          <.ul(codes.map(c => <.li(c.txt)))
    make {
      case GenericReqRow(_, exp, _) => render(exp.reqCodes)
    }
  }

  private def tags = make {
    case GenericReqRow(_, _, mv) => widgets.tags(mv.tags)
  }

  private def cfTags(id: CustomField.Tag.Id) = make {
    case GenericReqRow(_, exp, _) => widgets.tags(exp.cfTags.getOrElse(id, Vector.empty))
  }

  private def desc = make {
    case GenericReqRow(req, _, _) => widgets.text(req.desc)
  }

  private def cfText(id: CustomField.Text.Id) = {
    val textData = project.reqFieldData.data.text.getOrElse(id, Map.empty)
    make {
      case GenericReqRow(req, _, _) => textData.get(req.id) map (widgets.text1(_)) getOrElse empty
    }
  }

  private def imps(l: Optional[Row, Vector[Pubid]]) = make(
    l.getMaybe(_).cata(widgets.pubidRefs, empty))
}
