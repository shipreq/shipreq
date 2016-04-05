package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.univeq._
import org.scalajs.dom
import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.webapp.client.data.{Off, On}
import shipreq.webapp.client.lib.DND

/**
 * Something like this:
 *   1. Ξ [x] Item 3
 *   2. Ξ [x] Item 1
 *   3. Ξ [x] Item 5
 *   4. Ξ [ ] Item 2
 *   5. Ξ [x] Item 5
 *   6. Ξ [ ] Item 6
 */
object OrderedSubsetEditor {

  case class Styles(row     : TagMod = EmptyTag,
                    dragHnd : TagMod = EmptyTag,
                    checkbox: TagMod = EmptyTag,
                    label   : TagMod = EmptyTag)

  val noStyle = new Styles()
  val _noStyle = (_: Any, _: Any) => noStyle
}

final class OrderedSubsetEditor[A: Equal] {
  import OrderedSubsetEditor._

  case class State(all: Vector[(A, On)]) {
    val on: Vector[A] =
      all.filter(_._2 :: On).map(_._1)

    def toggle(a: A): State =
      State(all.map {
        case (a2, o) if a ≟ a2 => (a2, !o)
        case t                 => t
      })

    def filter(f: A => Boolean): State =
      State(all.filter(t => f(t._1)))
  }

  object State {
    def init(all: Vector[A])(isOn: A => On): State = {
      State(all.map(a => (a, isOn(a))))
    }

    implicit def univEqState(implicit ev: UnivEq[A]): UnivEq[State] =
      UnivEq.derive
  }

  case class Props(state    : State,
                   update   : State => Callback,
                   label    : A => String,
                   mandatory: A => Boolean,
                   filter   : A => Boolean,
                   styles   : (A, On) => Styles = _noStyle)

  val Component =
    ReactComponentB[Props]("OrderedSubsetEditor")
      .initialState(DND.Parent.initialState[A])
      .renderBackend[Backend]
      .domType[dom.html.OList]
      .build

  final class Backend($: BackendScope[Props, DND.Parent.PState[A]]) {

    val Row = DND.Child.dndItemComponentB[A, (Props, On)]({
      case (outerAttr, draghnd, a, (p, on)) =>

        def toggleIO: Callback =
          p.update(p.state toggle a)

        def checkboxAttr: TagMod =
          if (p.mandatory(a))
            ^.disabled := true
          else
            ^.onChange --> toggleIO

        val style = p.styles(a, on)

        <.li(outerAttr, style.row,
          draghnd(style.dragHnd),
          <.label(
            Widgets.checkbox(on)(checkboxAttr)(style.checkbox),
            <.span(style.label, p.label(a))))
    })

    def li(p: Props)(a: A, on: On): ReactElement =
      Row((a, DND.Parent.cProps($, a, moveIO(p)), (p, on)))

    def moveIO(p: Props)(from: A, to: A): Callback =
      p.update(move(p.state)(from, to))

    def render(p: Props) = {
      val rows: Stream[ReactElement] =
        p.state.all.toStream
          .filter(p filter _._1)
          .map(t => li(p)(t._1, t._2))

      <.ol(^.cls := "ordsubset", rows)
    }
  }

  def move(state: State)(from: A, to: A): State =
    State(DND.move(from, to, state.all)((a, b) => a ≟ b._1))
}
