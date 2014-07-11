package golly

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.{document, console, window}

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._

import scala.collection.immutable.SortedSet
import scalaz.{LensFamily, Lens}

object ReactExamples {

  implicit final class ComponentScope_SS_Ext2[State](val u: ComponentScope_SS[State]) extends AnyVal {
    @inline def setStateL[V](l: LensFamily[State, State, _, V])(v: V) = u.modState(l.set(_, v))
    @inline def modState2(f: State => State, callback: => Unit) = u.setState(f(u.state), callback)
  }

  def textChangeRecv(f: String => Unit): SyntheticEvent[dom.HTMLInputElement] => Unit = e => f(e.target.value)
  def textChangeRecvL[State](t: ComponentScope_SS[State], l: Lens[State, String]) = textChangeRecv(t setStateL l)

  // ===================================================================================================================

  object Sample4 {

    case class State(people: SortedSet[String], text: String, focusPerson: Option[String])
    val stateTextL = Lens.lensg[State, String](a => b => a.copy(text = b, focusPerson = None), _.text)

    class PeopleListBackend(t: BackendScope[Unit, State]) {
      def delete(name: String): Unit = {
        val p = t.state.people
        if (p.contains(name))
          t.setState(State(p - name, name, None))
      }

      val onChange = textChangeRecvL(t, stateTextL)

      val onKP: SyntheticEvent[dom.HTMLInputElement] => Unit =
        e => if (e.keyboardEvent.keyCode == 13) {
            e.preventDefault()
            add()
          }

      def add(): Unit = t.setState(State(t.state.people + t.state.text, "", Some(t.state.text)))
    }

    case class PeopleListProps(people: SortedSet[String], latest: Option[String], deleteFn: String => Unit)

    val PeopleList = {
      val focusNext = Ref[dom.HTMLInputElement]("latest")

      ReactComponentB[PeopleListProps]("PeopleList")
        .render(P =>
          if (P.people.isEmpty)
            div(color := "#800")("No people in your list!!").render
          else
            ol(P.people.toList.map(p =>
              li(
                input(value := p, (P.latest contains p) && (ref := focusNext))(),
                button(marginLeft := 1.em, onclick runs P.deleteFn(p))("Delete"))
            )).render
          )
          .componentDidUpdate((t,_,_) => focusNext(t).tryFocus())
          .componentDidMount(t => focusNext(t).tryFocus())
          .create
    }

    val PeopleEditor = ReactComponentB[Unit]("PeopleEditor")
      .getInitialState(_ => State(SortedSet("First","Second", "x"), "Middle", Some("Second")))
      .backend(new PeopleListBackend(_))
      .render((_,S,B) =>
          div(
            h3("People List")
            ,div(PeopleList(PeopleListProps(S.people, S.focusPerson, B.delete)))
            ,h3("Add")
            ,input(onchange ==> B.onChange, onkeypress ==> B.onKP, value := S.text)()
            ,button(onclick runs B.add())("+")
          ).render
      )
      .create

    def apply(): Unit =
      React.renderComponent(PeopleEditor(()), document getElementById "target2")
  }

  // ===================================================================================================================

  // TODO className
  val cls = "className".attr

  /**
   * In order to support multiple instances we need global state. Only one cell per browser can have focus so if
   * Component A's cell has focus and user clicks cell in Component B, Component A needs to lose focus.
   */
  object ExcelLike {

    case class RowProps(cell: String, focused: Boolean, setFocus: SyntheticEvent[_] => Unit)

    val Row = ReactComponentB[RowProps]("Row")
      .render(P =>
        tr(td(P.focused && (cls := "focus"), onmousedown ==> P.setFocus)(P.cell))
      ).create

    case class State(focus: Option[Int], cells: Vector[String])

    // TODO BackendScope rename or typealias to BackendScope
    class Backend(T: BackendScope[Unit, State]) {

      val UP = 38
      val DOWN = 40
      val F2 = 113

      def onGlobalKeyDown(e: dom.KeyboardEvent): Unit =
        T.state.focus.foreach(curFocus =>
          e.keyCode match {
            case UP =>
//              console.log("UP")
              if (focus(curFocus - 1)) e.preventDefault()
            case DOWN =>
//              console.log("DOWN")
              if (focus(curFocus + 1)) e.preventDefault()
            case F2 =>
              dom.alert(s"Edit cell #$curFocus, ${T.state.cells(curFocus)}")
            case _ =>
//              console.log(e)
          }
        )

      def onGlobalMouseDown(f: js.UndefOr[js.Function1[dom.MouseEvent, _]])(e: dom.MouseEvent): Unit = {
//        console.log("Global", f, e)
        if (f != null) f.foreach(_(e))
        T.modState(_.copy(focus = None))
      }

      def onRowClick(e: SyntheticEvent[_], row: Int): Unit = {
//        console.log("Row", row, e)
        if (focus(row)) e.stopPropagation()
      }

      def focus(i: Int) =
        if (i >= 0 && i < T.state.cells.length) {
          T.modState(_.copy(focus = Some(i)))
          true
        } else
          false
    }

    val Table = ReactComponentB[Unit]("Table")
      .initialState(State(None, Vector("A", "B", "C", "D")))
      .backend(new Backend(_))
      .render((_,S,B) =>
        table(cls := "excellike")(
          tr(th("CELL2")),
          S.cells.zipWithIndex.map(x =>
            Row(RowProps(x._1, S.focus contains x._2, B.onRowClick(_, x._2))))
        )
      )
      .componentWillMount(T => {
        dom.onkeydown = T.backend.onGlobalKeyDown _
        dom.onmousedown = T.backend.onGlobalMouseDown(dom.onmousedown) _
      })
      .create

    def apply(): Unit = {
      Table(()) render document.getElementById("target1")
//      Table(()) render document.getElementById("target2")
    }
  }
}
