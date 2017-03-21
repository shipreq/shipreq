package shipreq.webapp.client.project.app.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.Memo
import shipreq.webapp.client.project.app.Assets
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
object SortEditor {

  case class Props(value       : SortCriteria,
                   update      : SortCriteria ~=> Callback,
                   nameResolver: Column.NameResolver)

  implicit val reusability = Reusability.caseClass[Props]

  val renderSortMethod = Memo[SortMethod, VdomElement] { m =>
    import Assets._
    def pair(a: VdomTag, b: VdomTag) = <.div(a(*.sortMethodHalfTop), b(*.sortMethodHalfBottom))
    val tag = m match {
      case SortMethod.Asc            => sortSvgAsc(*.sortMethodFull)
      case SortMethod.Desc           => sortSvgDesc(*.sortMethodFull)
      case SortMethod.AscThenBlanks  => pair(sortSvgAsc, sortSvgBlank)
      case SortMethod.DescThenBlanks => pair(sortSvgDesc, sortSvgBlank)
      case SortMethod.BlanksThenAsc  => pair(sortSvgBlank, sortSvgAsc)
      case SortMethod.BlanksThenDesc => pair(sortSvgBlank, sortSvgDesc)
    }
    tag(^.title :=  m.description)
  }

  class Backend($: BackendScope[Props, Unit]) {

    def rotateSortMethod(c: Column): Callback =
      $.props >>= { p =>
        val sc = p.value
        val nv = sc.rotateSortMethod(c) getOrElse sc
        p update nv
      }

    val dnd = new DragToReorder[SortCriterion](
      newOrder =>
        $.props >>= { p =>
          val newValue = SortCriteria.attempt(newOrder) getOrElse p.value.copy(last = SortCriteria.defaultConclusive)
          p update newValue
        },

      content =>
        $.props map[VdomElement] { p =>
          val nameResolver = p.nameResolver
          var conclusiveSeen = false

          def renderItem(i:  DragToReorder.Item[SortCriterion]): VdomNode = {
            val col = i.data.column
            val conclusive = i.data.isConclusive
            val status =
              if (conclusiveSeen && !conclusive)
                DragToReorder.Tombstone
              else
                i.status
            conclusiveSeen |= conclusive

            <.table(
              *.itemOuter(status),
              i.mod,
              ^.onClick --> rotateSortMethod(col),
              <.tbody(
                <.tr(
                  <.td(
                    *.itemSortMethod,
                    renderSortMethod(i.data.method)),
                  <.td(
                    *.itemName(conclusive),
                    nameResolver(col)))))
          }

          <.div(*.outer,
            "Sort: ",
            <.div(
              *.dragArea,
              content.rootMod,
              content.items toVdomArray renderItem)
          )
      })

    def render(p: Props) =
      dnd.Component(p.value.all.whole)
  }

  val Component = ScalaComponent.builder[Props]("SortEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
