package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalacss.ScalaCssReact._
import japgolly.scalacss.StyleA
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import DataImplicits._


final class ColumnRenderer(
  val header     : ReactElement,
  val render     : Row => ReactElement,
  val columnStyle: Option[StyleA])

object ColumnRenderer {
  val `N/A`: ReactElement =
    <.span(*.`N/A`, "–")
}

class ColumnRenderers(project: Project, columnName: Column.NameResolver, widgets: ProjectWidgets) {

  def apply(c: Column): ColumnRenderer = c match {
    case Column.PubId          => pubId(c)
    case Column.ReqType        => reqType(c)
    case Column.Code           => code(c)
    case Column.Desc           => desc(c)
    case Column.Tags           => tags(c)
    case Column.ImplicationSrc => placeholder
    case Column.ImplicationTgt => placeholder
    case Column.CustomField(f) =>
      f match {
        case id: CustomField.Text       .Id => cfText(id)(c)
        case id: CustomField.Tag        .Id => cfTags(id)(c)
        case id: CustomField.Implication.Id => placeholder
      }
  }

  protected def make(render: Row => ReactElement): Column => ColumnRenderer =
    make(render, None)

  protected def make(render: Row => ReactElement, columnStyle: Option[StyleA]): Column => ColumnRenderer =
    c => new ColumnRenderer(<.span(columnName(c)), render, columnStyle)
  
  // @deprecated("placeholder is for dev purposes only.", "")
  def placeholder =
    new ColumnRenderer(<.span("∅"), Function const <.span("∅"), None)

  def pubId = make {
    case GenericReqRow(req, _, _) => widgets.pubIdText(req.pubId)()
  }

  def reqType = make {
    case GenericReqRow(req, _, _) => widgets.reqType(req.reqTypeId)()
  }

  def code = {
    def render(codes: List[ReqCode]): ReactElement =
          <.ul(codes.map(c => <.li(c.txt)))
    make {
      case GenericReqRow(_, exp, _) => render(exp.reqCodes)
      // case ReqCodeGroupRow(_, c) => xxx(c :: Nil)
    }
  }

  def tags = make {
    case GenericReqRow(_, _, mv) => widgets.tagList(mv.tags)
  }

  def cfTags(id: CustomField.Tag.Id) = make {
    case GenericReqRow(_, exp, _) => widgets.tagList(exp.cfTags.getOrElse(id, Nil))
  }

  def desc = make {
    case GenericReqRow(req, _, _) => widgets.text(req.desc)
  }

  val textData = project.reqFieldData.data.text
  val empty: ReactElement = <.span

  def cfText(id: CustomField.Text.Id) = make {
    case GenericReqRow(req, _, _) => textData.get(id).flatMap(_ get req.id) map (widgets.text1(_)) getOrElse empty
  }

  //  // ===================================================================================================================
//  class Desc extends ColumnRenderer {
//
//    override def columnStyle = None
//
//    override def header: ReactElement =
//      <.span("Desc") // Use Column.NameResolver
//
//    override val render: Row => ReactElement = {
//      case GenericReqRow(req, _, _) => ??? //xxx(req.desc)
////      case ReqCodeGroupRow(g, _) => xxx(g.desc)
//    }
//
//    def xxx(desc: String): ReactElement = <.span(desc)
//  }
//
//  // ===================================================================================================================
//  class CFText(project: Project, id: CustomField.Text.Id) extends ColumnRenderer {
//
//    override def columnStyle = None
//
//
//    val reqs = project.reqFieldData.data.text(id)
//
//    override def header: ReactElement =
//      <.span("TODO") // Use Column.NameResolver
//
//    override val render: Row => ReactElement = {
//      case GenericReqRow(req, _, _) =>
//        val valueO = reqs.get(req.id)
//        ???
////      case ReqCodeGroupRow(_, _) => `N/A`
//    }
//  }
//
//  // ===================================================================================================================
//  class CFTag(project: Project, scope: Option[Must[Tag.Id]]) extends ColumnRenderer {
//
//    override def columnStyle = None
//
//
//    def this(project: Project, fieldId: CustomField.Tag.Id) {
//      this(project, project.customField(fieldId).map(_.tagId).some)
//    }
//
//    // TODO if scope is None, we need to know everyone elses (tagWhitelists : Set[Tag.Id])
//    val tagWhitelist: Option[Set[Tag.Id]] = scope.map(???)
//
//    override def header: ReactElement =
//      <.span(scope.fold("Tags")(???)) // Use Column.NameResolver
//
//    override val render: Row => ReactElement = {
//      case GenericReqRow(req, _, _) =>
//        var reqtags = project.reqFieldData.data.tags(req.id)
//        tagWhitelist.foreach{w => reqtags = reqtags.filter(w.contains)}
//        ???
////      case ReqCodeGroupRow(_, _) => `N/A`
//    }
//  }
}
