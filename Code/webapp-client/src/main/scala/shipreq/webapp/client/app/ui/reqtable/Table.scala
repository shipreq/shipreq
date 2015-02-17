package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.webapp.base.data._
import SCRATCH._
import shipreq.webapp.client.app.ui.{ShowPublicReqId, ShowReqType}

object Table {

  case class Props(viewSettings: ViewSettings,
                   project     : Project,
                   columnName  : Column.NameResolver)

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  val noReqCodesToExpand: List[List[ReqCode]] = List(Nil)

  final class Backend($: BackendScope[Props, Unit]) {
    def render: ReactElement = {
      val p = $.props

      // Init columns
      val xxx = ColumnRenderer.thingy(p.project, p.columnName)
      val crs: Vector[ColumnRenderer] =
        p.viewSettings.columns.map(xxx)

      val expandCodes = p.viewSettings.order.includesCode

      // Collect rows
      var rows: Vector[Row] =
        p.project.reqs.values.foldLeft(Vector.empty[Row])((q, i) => i match {
          case r: GenericReq =>

          // Filter deleted

          // Expansion
          val expandedReqCodes: List[List[ReqCode]] = {
            val codes = p.project.reqCodesPerTarget(r.id)
            if (codes.isEmpty)
              noReqCodesToExpand
            else if (expandCodes)
              codes.foldLeft[List[List[ReqCode]]](Nil)((q2, c) => (c :: Nil) :: q2)
            else
              List(codes.toList) // TODO sort
          }

          (q /: expandedReqCodes)((q2, codes) => q2 :+ GenericReqRow(r, Expansion(None, None, codes)))
        })

      // Add SHRs

      // Sort
      
      // Render
      // TODO handle zero rows nicely. "33 reqs (SHRs?), 11 deleted, 3 excluded by filter."
      <.table(
        <.thead(
          <.tr(
            crs.map(cr =>
              <.th(
                cr.header)))),
        <.tbody(
          rows.map(r =>
            <.tr(
              crs.map(cr =>
                <.td(
                  cr render r))))))
    }
  }
}

// =====================================================================================================================

case class Expansion(implicationSrc: Option[Req.Id],
                     implicationTgt: Option[Req.Id],
                     reqCodes      : List[ReqCode])
object Expansion {
  val none = Expansion(None, None, Nil)
}

sealed trait Row
case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row
case class GenericReqRow(req: GenericReq, exp: Expansion) extends Row

// =====================================================================================================================
sealed trait ColumnRenderer {
//  def column: Column
  def header: ReactElement
  val render: Row => ReactElement
}
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

  @deprecated("ColumnRenderer.Null is for dev purposes only.", "")
  object Null extends ColumnRenderer {
    override def header: ReactElement = <.span("NULL")
    override val render: Row => ReactElement = Function const <.span("∅")
  }

  // -------------------------------------------------------------------------------------------------------------------
  // TODO don't create classes -- use new ColumnRenderer {}
  // (renderFn: Row => ReactElement)(project: Project, columnName: Column.NameResolver)(c: Column)

  class PubId(project: Project) extends ColumnRenderer {
    override def header: ReactElement =
      <.span("ID") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => ShowPublicReqId(req.pubId, project).render
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

  class ReqType(project: Project) extends ColumnRenderer {
    override def header: ReactElement =
      <.span("ReqType") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => ShowReqType(req.reqTypeId, project).render
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

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

  class Desc extends ColumnRenderer {
    override def header: ReactElement =
      <.span("Desc") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) => xxx(req.desc)
      case ReqCodeGroupRow(g, _) => xxx(g.desc)
    }

    def xxx(desc: String): ReactElement = <.span(desc)
  }

  class CFText(project: Project, id: CustomField.Text.Id) extends ColumnRenderer {

    val reqs = project.reqFieldData.text(id)

    override def header: ReactElement =
      <.span("TODO") // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) =>
        val valueO = reqs.get(req.id)
        ???
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }

  class CFTag(project: Project, scope: Option[Tag.Id]) extends ColumnRenderer {

    def this(project: Project, fieldId: CustomField.Tag.Id) {
      this(project, None) //project.fields.data.customFields.get(fieldId).asInstanceOf[CustomField.Tag].tagId.some)
      // TODO add getField[I,D](I): Err \/ D
    }

    // TODO if scope is None, we need to know everyone elses (tagWhitelists : Set[Tag.Id])
    val tagWhitelist: Option[Set[Tag.Id]] = scope.map(???)

    override def header: ReactElement =
      <.span(scope.fold("Tags")(???)) // Use Column.NameResolver

    override val render: Row => ReactElement = {
      case GenericReqRow(req, _) =>
        var reqtags = project.reqFieldData.tags(req.id)
        tagWhitelist.foreach{w => reqtags = reqtags.filter(w.contains)}
        ???
      case ReqCodeGroupRow(_, _) => `N/A`
    }
  }
}
