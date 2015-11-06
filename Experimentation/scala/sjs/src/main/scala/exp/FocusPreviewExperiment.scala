package exp

import monocle._
import monocle.macros.Lenses
import org.scalajs.dom, dom.ext.KeyCode
import shipreq.base.util.Util
import scalajs.js
import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._

/**
  * Preview available:
  * - when editing and focused and (dirty or has been edited since receiving focus)
  *
  * Editor opens:
  * - when clicked
  * - when navigated to by KB
  *
  * Editor closes:
  * - on commit (enter)
  * - on abort (escape)
  * - when loses focus and there is no change
  */
object FocusPreviewExperiment {

  import CompState._

  @inline implicit def MonocleReactCompStateOpsDD2[$, S]($: $)(implicit ops: $ => ReadDirectWriteDirectOps[S]) =
    new MonocleReactCompStateOps2[ReadDirectWriteDirectOps[S], S, Unit](ops($))

  @inline implicit def MonocleReactCompStateOpsDC2[$, S]($: $)(implicit ops: $ => ReadDirectWriteCallbackOps[S]) =
    new MonocleReactCompStateOps2[ReadDirectWriteCallbackOps[S], S, Callback](ops($))

  @inline implicit def MonocleReactCompStateOpsCC2[$, S]($: $)(implicit ops: $ => ReadCallbackWriteCallbackOps[S]) =
    new MonocleReactCompStateOps2[ReadCallbackWriteCallbackOps[S], S, Callback](ops($))

  final class MonocleReactCompStateOps2[Ops <: WriteOpAux[S, W], S, W](private val $: Ops) extends AnyVal {
    def setStateL[L[_, _, _, _], B](l: L[S, S, _, B])(b: B, cb: Callback = Callback.empty)(implicit L: SetterMonocle[L]): W =
      $.modState(L.set(l)(b), cb)
  }


  def main(): Unit = {
    val tgt = dom.document.getElementById("target")
    ReactDOM.render(Table.Comp(), tgt)
  }


  object Table {
    val sampleData = Vector[String](
      "blah [blah] #1",
      "blah",
      "blah blah",
      "[blah] blah [blah]")

    val Id = "FocusPreviewExperiment"

    class Backend($: BackendScope[Unit, Unit]) {

      def ref(i: Int) = Ref.to(Row.Comp, "row_" + i)

      lazy val focusIndex: Int => Callback = i => Callback byName {
        val i2 = Util.fitCollectionIndex(i, sampleData.length)
        val c = ref(i2)($).get
        val b = c.backend
        c.state.edit match {
          case Some(_) => b.ref(c).tryFocus
          case None => b.startEdit
        }
      }

      def render =
        <.table(
          ^.id := Id,
          //      <.thead(
          //        <.tr(<.th("demo"))),
          <.tbody(
            sampleData.zipWithIndex.map(d =>
              <.tr(
                ^.key := d._2,
                Row.Comp.withRef(ref(d._2))(Row.Props(d._1, d._2, focusIndex))
              ))))
    }

    val Comp = ReactComponentB[Unit]("Outer")
      .renderBackend[Backend]
      .buildU
  }

  object Row {

    case class Props(value: String, index: Int, focusIndex: Int => Callback)

    @Lenses
    case class State(value: String, index: Int, edit: Option[String], focus: Boolean, changedSinceFocus: Boolean)

    object SimpleParser {
      val token = """^(.*?)\[([^\[]+?)\](.*)$""".r

      def append(q: Vector[ReactTag], s: String) =
        if (s.isEmpty) q else q :+ <.span(s)

      @scala.annotation.tailrec
      def go(s: String, acc: Vector[ReactTag]): Vector[ReactTag] = {
        val m = token.pattern.matcher(s)
        if (m.matches) {
          var q = append(acc, m group 1)
          q :+= <.span(^.color := "red", ^.backgroundColor := "#ddd", ^.padding := "0 6px", m group 2)
          go(m group 3, q)
        }
        else
          append(acc, s)
      }

      def apply(s: String): ReactTag =
        <.span(go(s, Vector.empty): _*)
    }

    // Values here correspond to values in CSS in index.html
    val tg = Addons.ReactCssTransitionGroup("fadeanim", enterTimeout = 110, leaveTimeout = 110, component = "div")

    class Backend($: BackendScope[Props, State]) {

      val ref = Ref[dom.html.Input]("i")

      val startEdit: Callback =
        $.modState(s => s.copy(edit = Some(s.value)), Callback byName ref($).tryFocus)

      def render(p: Props, s: State): ReactElement = {

        val inner = s.edit match {
          case None =>
            SimpleParser(s.value)

          case Some(t) =>
            def onChange(e: ReactEventI): Callback =
              $.modState(s => s.copy(edit = Some(e.target.value), changedSinceFocus = true))

            def onKey(e: ReactKeyboardEventI): Callback =
              CallbackOption.keyCodeSwitch(e) {
                case KeyCode.Escape => $.setState(State(s.value, s.index, None, false, false))
                case KeyCode.Enter => $.setState(State(t, s.index, None, false, false))
                case KeyCode.Down => p.focusIndex(s.index + 1)
                case KeyCode.Up => p.focusIndex(s.index - 1)
              }

            def onFocus: Callback =
              $.modState(s => s.copy(focus = true, changedSinceFocus = false))

            def onBlur: Callback =
              $.modState { s =>
                val newEdit = s.edit.filter(_ != s.value)
                s.copy(edit = newEdit, focus = false, changedSinceFocus = false)
              }

            val input =
              <.input(
                ^.backgroundColor := (if (s.focus) "#ffc" else "#f2f2d6"),
                ^.ref := ref,
                ^.`type` := "text",
                ^.onChange ==> onChange,
                ^.onKeyDown ==> onKey,
                ^.onFocus --> onFocus,
                ^.onBlur --> onBlur,
                ^.value := t)

            val showPreview = s.focus && (s.changedSinceFocus || s.value != t)

            def preview =
              if (showPreview)
                tg(
                  <.div(^.key := 9,
                    <.div("Preview:"),
                    <.div(^.backgroundColor := "#efe", SimpleParser(t))))
              else
                tg()

            <.div(input, preview)
        }

        val outer = s.edit match {
          case None =>
            TagMod(
              ^.onClick --> startEdit)

          case Some(_) =>
            TagMod()

        }

        <.td(
          ^.border := "solid 1px #444",
          ^.padding := "0.5ex 1ex",
          ^.width := "30ex",
          outer,
          inner)
      }
    }

    val Comp = ReactComponentB[Props]("Row")
      .initialState_P[State](p => State(p.value, p.index, None, false, false))
      .renderBackend[Backend]
      .build
  }
}
