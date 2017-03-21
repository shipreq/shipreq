package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.scalajs.react._, vdom.html_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js
import scalaz.Equal
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.base.data.{Disabled, Enabled}

object SelectOne {

  type Choices[A] = NonEmptyVector[Choice[A]]

  case class Choice[A](value  : A,
                       label  : String,
                       enabled: Enabled) {
    def map[B](f: A => B): Choice[B] = copy(value = f(value))
  }

  case class Props[A](selected: A,
                      choices : Choices[A],
                      select  : Option[A => Callback],
                      style   : TagMod = EmptyVdom)

  def Component[A: Equal] =
    ScalaComponent.builder[Props[A]]("SelectOne")
      .render_P(render(_))
      .build

  def render[A](props: Props[A])(implicit E: Equal[A]): VdomTag = {

    val (options, selectedValue) = {
      var sel = -1
      var i = 0
      val j = VdomArray.empty()
      props.choices.foreach { c =>
        j += <.option(
          ^.value := i,
          ^.key := i,
          ^.disabled := (c.enabled :: Disabled),
          c.label)
        if (sel == -1 && E.equal(c.value, props.selected))
          sel = i
        i += 1
      }
      (j, sel)
    }

    def onChange: ReactEventFrom[HTMLSelectElement] => Option[Callback] =
      e => for {
        i  ← ParseInt unapply e.target.value
        c  ← props.choices(i)
        io ← props.select
      } yield io(c.value)

    <.select(
      props.style,
      ^.value      := selectedValue,
      ^.disabled   := props.select.isEmpty,
      ^.onChange ==>? onChange,
      options)
  }

  // ===================================================================================================================

  def optional[A](choices: Vector[Choice[A]], nopLabel: String = ""): Choices[Option[A]] = {
    val nop = Choice[Option[A]](None, nopLabel, Enabled)
    val tail = choices.map(_.map(_.some))
    NonEmptyVector(nop, tail)
  }
}
