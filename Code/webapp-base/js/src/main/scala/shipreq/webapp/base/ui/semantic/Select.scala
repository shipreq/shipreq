package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js
import shipreq.base.util.univeq._

object Select {

  type OptionKey = String

  final case class Option[+A](key: OptionKey, title: String, value: A)

  implicit def optionOrdering[A]: Ordering[Option[A]] = Ordering.by(_.title)

  sealed trait Props {
    type A
    val options : Traversable[Option[A]]
    val selected: js.UndefOr[OptionKey]
    val tagMod  : TagMod
    val onChange: Option[A] => Callback

    private[Select] lazy val rendered = {
      val optionArray =
        options.toVdomArray(o => <.option(^.key := o.key, ^.value := o.key, o.title))

      def onChange2: ReactEventFrom[HTMLSelectElement] => Callback =
        _.extract(_.target.value)(v =>
          options.find(_.key ==* v) match {
            case Some(o) => onChange(o)
            case None => Callback.warn(s"'$v' missing from $options")
          }
        )

      <.select(
        tagMod,
        ^.cls := "ui dropdown",
        ^.value := selected.getOrElse(null),
        ^.onChange ==> onChange2,
        optionArray)
    }
  }

  def apply[A](options : Traversable[Option[A]],
               selected: js.UndefOr[OptionKey] = js.undefined,
               tagMod  : TagMod = EmptyVdom)
              (onChange: Option[A] => Callback): VdomElement = {
    type AA       = A
    val _options  = options
    val _selected = selected
    val _tagMod   = tagMod
    val _onChange = onChange
    Component(new Props {
      override type A = AA
      override val options  = _options
      override val selected = _selected
      override val tagMod   = _tagMod
      override val onChange = _onChange
    })
  }

  private class Backend($: BackendScope[Props, Unit]) {
    val enableDropdown: Callback =
      Dropdown.enable($.getDOMNode)
  }

  private val Component = ScalaComponent.builder[Props]("Select")
    .backend(new Backend(_))
    .render_P(_.rendered)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
