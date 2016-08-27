package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js
import shipreq.base.util.univeq._

object Select {

  /*
  TODO Delete
  type Value = String

  case class Option(value: Value, title: String)

  implicit def optionUnivEq: UnivEq[Option] = UnivEq.derive

  implicit val optionOrdering: Ordering[Option] = Ordering.by(_.title)

  final case class Props(options: Seq[Option], selected: js.UndefOr[Value] = js.undefined) {
    @inline def render = Component(this)
  }

 implicit val reusabilityProps: Reusability[Props] =
   Reusability.fn((x, y) =>
     (x.selected == y.selected) && x.options.corresponds(x.options)(_ ==* _))

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement = {
      val optionArray = new js.Array[ReactTag]
      for (o <- p.options)
        optionArray push <.option(
          ^.key := o.value,
          ^.value := o.value,
          ^.selected := p.selected.exists(_ == o.value),
          o.title)
      <.select(^.cls := "ui dropdown", optionArray)
    }
  }

  val Component = ReactComponentB[Props]("Select")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
  */

  type OptionKey = String

  case class Option[+A](key: OptionKey, title: String, value: A)

  implicit def optionOrdering[A]: Ordering[Option[A]] = Ordering.by(_.title)

  def apply[A](options: Traversable[Option[A]],
               selected: js.UndefOr[OptionKey] = js.undefined)(
               onChange: Option[A] => Callback) = {

    val optionArray = new js.Array[ReactNode]
    for (o <- options) {
      optionArray push <.option(
        ^.key := o.key,
        ^.value := o.key,
        o.title)
    }

    def onChange2: SyntheticEvent[HTMLSelectElement] => Callback =
      _.extract(_.target.value)(v =>
        options.find(_.key ==* v) match {
          case Some(o) => onChange(o)
          case None => Callback.warn(s"'$v' missing from $options")
        }
      )

    <.select(
      ^.cls := "ui dropdown",
      ^.value := selected.getOrElse(null),
      ^.onChange ==> onChange2,
      optionArray)
  }
}
