package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{Disabled, Enabled}

object Select {

  type OptionKey = String

  final case class Option[+A](key: OptionKey, title: String, value: A)

  implicit def optionOrdering[A]: Ordering[Option[A]] = Ordering.by(_.title)

  sealed trait Props {
    type A
    val options : Traversable[Option[A]]
    val selected: scala.Option[OptionKey]
    val enabled : Enabled
    val tagMod  : TagMod
    val onChange: Option[A] => Callback

    private[Select] lazy val rendered = {
      val optionArray = VdomArray.empty()

      if (selected.isEmpty)
        optionArray += <.option(^.key := "∅")

      optionArray ++=
        options.toIterator.map(o => <.option(^.key := o.key, ^.value := o.key, o.title))

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
        (^.cls := "disabled").when(enabled is Disabled),
        selected.whenDefined(^.value := _),
        ^.onChange ==> onChange2,
        optionArray)
    }
  }

  def apply[A](options : Traversable[Select.Option[A]],
               selected: scala.Option[OptionKey] = None,
               enabled : Enabled = Enabled,
               tagMod  : TagMod = EmptyVdom)
              (onChange: Select.Option[A] => Callback): VdomElement = {
    type AA       = A
    val _options  = options
    val _selected = selected
    val _enabled  = enabled
    val _tagMod   = tagMod
    val _onChange = onChange
    Component(new Props {
      override type A = AA
      override val options  = _options
      override val selected = _selected
      override val enabled  = _enabled
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
