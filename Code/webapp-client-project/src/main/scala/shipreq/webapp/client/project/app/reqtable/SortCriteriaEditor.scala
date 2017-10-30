package shipreq.webapp.client.project.app.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.Memo
import shipreq.webapp.base.ClientResources
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.client.project.app.Style.reqtable.{sortEditor => *}
import shipreq.webapp.client.project.widgets.DragToReorder

/**
 * Looks like this:
 *
 *    Sort: [↓ Code]  [↑ Title]  [↓ ID]
 *
 * Features:
 * - Click to change sort method (direction).
 * - Drag to reorder.
 * - Drag outside to remove criterion.
 */
object SortCriteriaEditor {

  case class Props(value     : SortCriteria,
                   update    : SortCriteria ~=> Callback,
                   allColumns: ColumnPlus.All) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusability: Reusability[Props] =
    Reusability.byRef || Reusability.caseClass

  private val renderSortMethod = Memo[SortMethod, VdomElement] { m =>
    import ClientResources._
    def pair(a: VdomTag, b: VdomTag) = <.div(a(*.sortMethodHalfTop), b(*.sortMethodHalfBottom))
    val tag = m match {
      case SortMethod.Asc            => sortAscImg(*.sortMethodFull)
      case SortMethod.Desc           => sortDescImg(*.sortMethodFull)
      case SortMethod.AscThenBlanks  => pair(sortAscImg, sortBlankImg)
      case SortMethod.DescThenBlanks => pair(sortDescImg, sortBlankImg)
      case SortMethod.BlanksThenAsc  => pair(sortBlankImg, sortAscImg)
      case SortMethod.BlanksThenDesc => pair(sortBlankImg, sortDescImg)
    }
    tag(^.title :=  m.description)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def rotateSortMethod(c: Column): Callback =
      $.props >>= { p =>
        val sc = p.value
        val nv = sc.rotateSortMethod(c) getOrElse sc
        p update nv
      }

    private def updateItems(newOrder: Vector[SortCriterion]): Callback =
      $.props >>= { p =>
        val newValue = SortCriteria.attempt(newOrder) getOrElse p.value.copy(last = SortCriteria.defaultConclusive)
        p update newValue
      }

    private def dndRenderFn(content: DragToReorder.Content[SortCriterion]): CallbackTo[VdomElement] =
      $.props.map { p =>
        var conclusiveSeen = false

        def renderItem(i: DragToReorder.Item[SortCriterion]): VdomNode = {
          val col = i.data.column
          val conclusive = i.data.isConclusive
          val status =
            if (conclusiveSeen && !conclusive)
              DragToReorder.Tombstone
            else
              i.status
          conclusiveSeen |= conclusive

          <.table(*.draggableCriterion(status),
            i.mod,
            ^.onClick --> rotateSortMethod(col),
            <.tbody(
              <.tr(*.criterionBorder,
                <.td(*.name(conclusive), p.allColumns(col).fold("")(_.name)),
                <.td(*.sortMethod, renderSortMethod(i.data.method)))))
        }

        <.section(
          <.div(*.header, "Sort:"),
          <.div(*.dragArea,
            content.rootMod,
            content.items toVdomArray renderItem)): VdomElement
      }

    private val dnd = new DragToReorder[SortCriterion](updateItems, dndRenderFn)

    def render(p: Props): VdomElement =
      dnd.Component(p.value.all.whole)
  }

  val Component = ScalaComponent.builder[Props]("SortCriteriaEditor")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
