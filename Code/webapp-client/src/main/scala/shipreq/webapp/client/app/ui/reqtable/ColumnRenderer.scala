package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.widget._
import DataImplicits._


trait ColumnRenderer {
  //  def column: Column
  def header: ReactElement
  val render: Row => ReactElement
}

// =====================================================================================================================
object ColumnRenderer {

  val `N/A`: ReactElement =
    <.span(^.marginLeft.auto, ^.marginRight.auto, "–")

  def thingy(project: Project, columnName: Column.NameResolver): Column => ColumnRenderer = {
    case Column.PubId          => new PubId(project)
    case Column.ReqType        => new ReqType(project)
    case Column.Code           => new Code
    case Column.Desc           => new Desc
    case Column.CustomField(f) =>
      f match {
        case id: CustomField.Text       .Id => new CFText(project, id)
        case id: CustomField.Tag        .Id => new CFTag(project, id)
        case id: CustomField.Implication.Id => Null
      }
  }

  // TODO don't create classes -- use new ColumnRenderer {}
  // (renderFn: Row => ReactElement)(project: Project, columnName: Column.NameResolver)(c: Column)

  // ===================================================================================================================
  @deprecated("ColumnRenderer.Null is for dev purposes only.", "")
  object Null extends ColumnRenderer {
    override def header: ReactElement = <.span("NULL")
    override val render: Row => ReactElement = Function const <.span("∅")
  }

  // ===================================================================================================================
  class PubId(project: Project) extends ColumnRenderer {
    override def header: ReactElement =
      <.span("ID") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => PubidW(req.pubId, project).render
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

  // ===================================================================================================================
  class ReqType(project: Project) extends ColumnRenderer {
    override def header: ReactElement =
      <.span("ReqType") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => ReqTypeW(req.reqTypeId, project).render
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

  // ===================================================================================================================
  class Code extends ColumnRenderer {
    override def header: ReactElement =
      <.span("Code") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(_, e)   => xxx(e.reqCodes)
      case ReqCodeGroupRow(_, c) => xxx(c :: Nil)
    }

    def xxx(codes: List[ReqCode]): ReactElement =
      <.ul(codes.map(c => <.li(c.txt)))
  }

  // ===================================================================================================================
  class Desc extends ColumnRenderer {
    override def header: ReactElement =
      <.span("Desc") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => xxx(req.desc)
      case ReqCodeGroupRow(g, _) => xxx(g.desc)
    }

    def xxx(desc: String): ReactElement = <.span(desc)
  }

  // ===================================================================================================================
  class CFText(project: Project, id: CustomField.Text.Id) extends ColumnRenderer {

    val reqs = project.reqFieldData.data.text(id)

    override def header: ReactElement =
      <.span("TODO") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) =>
        val valueO = reqs.get(req.id)
        ???
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

  // ===================================================================================================================
  class CFTag(project: Project, scope: Option[Must[Tag.Id]]) extends ColumnRenderer {

    def this(project: Project, fieldId: CustomField.Tag.Id) {
      this(project, project.customField(fieldId).map(_.tagId).some)
    }

    // TODO if scope is None, we need to know everyone elses (tagWhitelists : Set[Tag.Id])
    val tagWhitelist: Option[Set[Tag.Id]] = scope.map(???)

    override def header: ReactElement =
      <.span(scope.fold("Tags")(???)) // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) =>
        var reqtags = project.reqFieldData.data.tags(req.id)
        tagWhitelist.foreach{w => reqtags = reqtags.filter(w.contains)}
        ???
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }
}
