package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ClientResources
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.client.project.app.Style.reqtable.{sortEditor => *}

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
    Reusability.byRef || Reusability.derive

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

    private val dnd =
      DragToReorderFeature[SortCriterion](
        getData             = $.props.map(_.value.all.whole),
        updateData          = u => updateItems(u.newOrder),
        updateUI            = $.forceUpdate,
        dragOutsideToRemove = true,
      )

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

    def render(p: Props): VdomElement = {
      var conclusiveSeen = false

      def renderItem(i: DragToReorderFeature.Item[SortCriterion]): VdomNode = {
        val col = i.data.column
        val conclusive = i.data.isConclusive
        val status =
          if (conclusiveSeen && !conclusive)
            DragToReorderFeature.Status.Tombstone
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
          dnd.container,
          dnd.items().toVdomArray(renderItem)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
