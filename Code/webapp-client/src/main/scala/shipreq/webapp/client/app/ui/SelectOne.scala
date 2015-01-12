package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.HTMLSelectElement
import scala.scalajs.js
import scalaz.Equal
import scalaz.effect.IO
import shipreq.base.util.ParseInt

object SelectOne {

  case class Choice[A](value   : A,
                       label   : String,
                       disabled: Boolean) {
    def map[B](f: A => B): Choice[B] = copy(value = f(value))
  }

  case class Props[A](selected: A,
                      choices : Seq[Choice[A]],
                      onSelect: Option[A => IO[Unit]])

  def optional[A](choices  : Seq[Choice[A]],
                  nopLabel : String = ""): Seq[Choice[Option[A]]] = {
    val nop = Choice[Option[A]](None, nopLabel, false)
    choices.foldLeft(Vector(nop))(_ :+ _.map[Option[A]](Some.apply))
  }

  def component[A: Equal] =
    ReactComponentB[Props[A]]("SelectOne")
      .render(render(_))
      .domType[HTMLSelectElement]
      .build

  def render[A](props: Props[A])(implicit E: Equal[A]): ReactTag = {

    val (options, selectedValue) = {
      var sel = -1
      var i = 0
      var j = js.Array[ReactNode]()
      props.choices.foreach { v =>
        j.push(
          <.option(
            ^.value    := i,
            ^.key      := i,
            ^.disabled := v.disabled,
            v.label))
        if (sel == -1 && E.equal(v.value, props.selected))
          sel = i
        i += 1
      }
      (j, sel)
    }

    def onChange: SyntheticEvent[HTMLSelectElement] => Option[IO[Unit]] =
      e => for {
        i  ← ParseInt unapply e.target.value
        v  = props.choices(i).value
        io ← props.onSelect
      } yield io(v)

    <.select(
      ^.value      := selectedValue,
      ^.disabled   := props.onSelect.isEmpty,
      ^.onChange ~~>? onChange,
      options)
  }
}
