package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js
import shipreq.base.util.univeq._

object Select {

  type OptionKey = String

  case class Option[+A](key: OptionKey, title: String, value: A)

  implicit def optionOrdering[A]: Ordering[Option[A]] = Ordering.by(_.title)

  def apply[A](options: Traversable[Option[A]],
               selected: js.UndefOr[OptionKey] = js.undefined)(
               onChange: Option[A] => Callback) = {

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
      ^.cls := "ui dropdown",
      ^.value := selected.getOrElse(null),
      ^.onChange ==> onChange2,
      optionArray)
  }
}
