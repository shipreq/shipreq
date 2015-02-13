package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.DND
import scala.collection.GenTraversable
import scala.collection.immutable.TreeMap
import scalaz.Equal
import scalaz.effect.IO

/**
 * Something like this:
 *   1. Ξ [x] Item 3
 *   2. Ξ [x] Item 1
 *   3. Ξ [x] Item 5
 *   4. Ξ [x] Item 2
 *   5. Ξ [ ] Item 5
 *   6. Ξ [ ] Item 6
 */
object OrderedSubsetEditor {

  case class Props[A](value    : Vector[A],
                      all      : GenTraversable[A],
                      label    : A => String,
                      mandatory: A => Boolean,
                      change   : Vector[A] => IO[Unit])

  def Component[A: Equal] =
    ReactComponentB[Props[A]]("OrderedSubsetEditor")
      .initialState(DND.Parent.initialState[A])
      .backend(new Backend(_))
      .render(_.backend.render)
      .domType[dom.html.OList]
      .build

  final class Backend[A]($: BackendScope[Props[A], DND.Parent.PState[A]])(implicit E: Equal[A]) {

    val Row = DND.Child.dndItemComponentB[A, (Props[A], Boolean)]({
      case (outerAttr, draghnd, a, (p, on)) =>

        def toggleIO: IO[Unit] =
          IO(
            if (on)
              p.value.filterNot(E.equal(a, _))
            else
              p.value :+ a
          ).flatMap(p.change)

        @inline def checkboxAttr: TagMod =
          if (p.mandatory(a))
            ^.disabled := true
          else
            ^.onChange ~~> toggleIO

        <.li(outerAttr, (!on) ?= (^.cls := "off"),
          draghnd,
          <.label(
            UI.checkbox(on)(checkboxAttr),
            <.span(^.cls := "label", p.label(a))))
    })

    def li(p: Props[A], inactive: Iterable[A])(a: A, on: Boolean): ReactElement =
      Row((a, DND.Parent.cProps($, a, moveIO(p, inactive)), (p, on)))

    def moveIO(p: Props[A], inactive: Iterable[A])(from: A, to: A): IO[Unit] =
      p.change(move(p.value, inactive, p.mandatory, E)(from, to))

    def render = {
      val p = $.props

      val orderedInactiveValues =
        p.all.foldLeft(TreeMap.empty[String, A])((q, a) =>
          if (p.value.exists(E.equal(a, _)))
            q
          else
            q.updated(p.label(a), a)
        ).values

      val li2 = li(p, orderedInactiveValues) _

      val activeRows =
        p.value.foldLeft(Vector.empty[ReactElement])(_ :+ li2(_, true))

      val inactiveRows =
        orderedInactiveValues.map(li2(_, false))

      val rows: Vector[ReactElement] =
        activeRows ++ inactiveRows

      <.ol(^.cls := "ordsubset", rows)
    }
  }

  def move[A](prev: Vector[A], inactive: Iterable[A], mandatory: A => Boolean, e: Equal[A])(from: A, to: A): Vector[A] = {
    var r = DND.move(from, to)(prev)(e)

    // Handle moving inactive up into active
    if (inactive.headOption.filter(e.equal(to, _)).isDefined && !prev.exists(e.equal(from, _)))
      r :+= from

    // Prevent dragging off mandatory items
    if (mandatory(from) && !r.exists(e.equal(from, _)))
      r = prev

    r
  }
}
