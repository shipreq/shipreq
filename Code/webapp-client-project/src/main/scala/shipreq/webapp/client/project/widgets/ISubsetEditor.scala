package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.base.util.{IMap, ISubset}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.client.base.lib.ClientUtil

object ISubsetEditor {

  def Component[A: UnivEq](staticProps: StaticProps[A]) =
    ReactComponentB[Mode[A]]("ISubsetEditor")
      .backend(new Backend(_, staticProps))
      .renderBackend
      .build

  // -------------------------------------------------------------------------------------------------------------------
  sealed trait Mode[A]

  case class ViewMode[A](value: ISubset[A], startEdit: Option[Callback]) extends Mode[A]

  case class EditMode[A](state     : EditState[A],
                         update    : EditState[A] => Callback,
                         finishEdit: Option[ISubset[A]] => Callback) extends Mode[A]

  // -------------------------------------------------------------------------------------------------------------------
  sealed abstract class Method(val code: String, val label: String)
  object Method {
    case object All  extends Method("a", "All")
    case object Only extends Method("o", "Only…")
    case object Not  extends Method("n", "Not…")

    implicit def equality: UnivEq[Method] = UnivEq.derive

    val all   = List[Method](All, Only, Not)
    val index = IMap.empty((_: Method).code) ++ all
  }

  // -------------------------------------------------------------------------------------------------------------------
  case class EditState[A: UnivEq](method: Method, values: Set[A]) {
    def result: Option[ISubset[A]] = method match {
      case Method.All  => Some(ISubset.All())
      case Method.Only => NonEmptySet.option(values).map(ISubset.Only.apply)
      case Method.Not  => NonEmptySet.option(values).map(ISubset.Not .apply)
    }
  }

  object EditState {
    def init[A: UnivEq](i: ISubset[A], defaultValues: Set[A]): EditState[A] = i match {
      case ISubset.All()    => EditState(Method.All,  defaultValues)
      case ISubset.Only(vs) => EditState(Method.Only, vs.tail + vs.head)
      case ISubset.Not(vs)  => EditState(Method.Not,  vs.tail + vs.head)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  final case class StaticProps[A](preprocess : Stream[A] => Stream[A],
                                  renderValue: A => ReactNode,
                                  allValues  : TraversableOnce[A]) {
    val allValueStatic =
      preprocess(allValues.toStream).foldLeft(Vector.empty[ValueStatic[A]])((q, a) =>
        q :+ ValueStatic(a,
          <.span(renderValue(a)),
          <.input(^.`type` := "checkbox")))
  }

  final case class ValueStatic[A](value: A, rendered: ReactTag, checkbox: ReactTag)

  // -------------------------------------------------------------------------------------------------------------------
  final class Backend[A: UnivEq]($: BackendScope[Mode[A], Unit], staticProps: StaticProps[A]) {
    import staticProps._

    val radioGroupName =
      ClientUtil.uniqueStr.runNow()

    def render(props: Mode[A]): ReactElement =
      props match {
        case m: ViewMode[A] => renderViewMode(m)
        case m: EditMode[A] => renderEditMode(m)
      }

    def renderViewMode(m: ViewMode[A]): ReactElement = {
      def selection(prefix: String, i: NonEmptySet[A]): TagMod = {
        val as = preprocess(i.head #:: i.tail.toStream)
        val ns = as.map(renderValue)
        val vs = ns.head #:: ns.tail.flatMap(v => Stream[ReactNode](", ", v))
        (prefix + ": ") #:: vs #::: Stream[ReactNode](".")
      }

      val values: TagMod = m.value match {
        case ISubset.All()    => "All."
        case ISubset.Only(vs) => selection("Only", vs)
        case ISubset.Not(vs)  => selection("Not", vs)
      }

      val editButton =
        m.startEdit.map(cb =>
          <.button(^.onClick --> cb, "Edit"))

      val all = editButton.fold[TagMod](values)(btn => Seq(values, btn))
      <.div(all)
    }

    val inputRadio =
      <.input(^.`type` := "radio", ^.name := radioGroupName)

    def renderEditMode(mode: EditMode[A]): ReactElement = {
      import mode.state

      val methodSelection =
        Method.all.map { v =>
          val selected = v ==* state.method
          <.label(
            ^.classSet1("isubsetM", "checked" -> selected),
            inputRadio(
              ^.value     := v.code,
              ^.checked   := selected,
              ^.onChange --> mode.update(state.copy(method = v))),
            <.span(v.label))
        }

      val allowValueSelection = state.method match {
        case Method.All => false
        case Method.Only
           | Method.Not => true
      }

      def valueSelection: TagMod = {
        def attr(a: A, selected: Boolean): TagMod = {
          def change =
            CallbackTo {
              val u = state.values.ifelse(_ => selected, _ - a, _ + a)
              state.copy(values = u)
            } >>= mode.update
          (^.checked := selected) + (^.onChange --> change)
        }

        allValueStatic.map { p =>
          val selected = state.values contains p.value
          <.label(
            ^.classSet1("isubsetV", "checked" -> selected),
            p.checkbox(attr(p.value, selected)),
            p.rendered)
        }
      }

      val saveButton = {
        val cb = state.result.map(v => mode.finishEdit(Some(v)))
        <.button("Save",
          ^.onClick -->? cb,
          ^.disabled  := cb.isEmpty)
      }

      val cancelButton =
        <.button("Cancel",
          ^.onClick --> mode.finishEdit(None))

      <.div(
        <.div(methodSelection),
        allowValueSelection ?= <.div(valueSelection),
        <.div(saveButton, cancelButton))
    }
  }
}
