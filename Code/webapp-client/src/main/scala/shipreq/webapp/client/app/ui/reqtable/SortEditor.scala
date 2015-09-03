package shipreq.webapp.client.app.ui.reqtable

import shipreq.base.util.UnivEq

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.webapp.client.app.ui.DragToReorder
import shipreq.webapp.client.app.ui.Style.reqtable.{sortEditor => *}
import shipreq.webapp.client.lib.ui.Assets

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
  val D = new DragToReorder[SortCriterion]

  case class Props(value       : SortCriteria,
                   update      : SortCriteria ~=> Callback,
                   nameResolver: Column.NameResolver)

  implicit val reusability = Reusability.caseClass[Props]

  val renderSortMethod = UnivEq.mutableHashMapMemo[SortMethod, ReactElement] { m =>
    import Assets._
    def pair(a: ReactTag, b: ReactTag) = <.div(a(*.sortMethodHalfTop), b(*.sortMethodHalfBottom))
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

    val updateFn = ReusableFn { (newOrder: Vector[SortCriterion]) =>
      $.propsCB >>= { p =>
        val newValue = SortCriteria.attempt(newOrder) getOrElse p.value.copy(last = SortCriteria.defaultConclusive)
        p update newValue
      }
    }

    def rotateSortMethod(c: Column): Callback =
      $.propsCB >>= { p =>
        val sc = p.value
        val nv = sc.rotateSortMethod(c) getOrElse sc
        p update nv
      }

    val renderFn = ReusableFn { (c: D.Content) =>
      val nameResolver = $.props.nameResolver
      var conclusiveSeen = false

      def renderItem(i:  D.Item) = {
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
          c.rootMod,
          c.items map renderItem)
      ): ReactElement
    }

    def render(p: Props) =
      D.Component(D.Props(p.value.all.whole, updateFn, renderFn))
  }

  val Component = ReactComponentB[Props]("SortEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
