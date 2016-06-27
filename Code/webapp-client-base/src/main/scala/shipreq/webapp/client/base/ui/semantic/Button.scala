package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq.UnivEq
import Button._

/** http://semantic-ui.com/elements/button.html */
object Button {

  sealed abstract class Attr(cls: ClassName) extends HasClass(cls)
  object Attr {
    case object Compact  extends Attr("compact")
    case object Circular extends Attr("circular")
    case object Inverted extends Attr("inverted")
    implicit def univEq: UnivEq[Attr] = UnivEq.derive
  }

  sealed abstract class Type(cls: ClassName) extends HasClass(cls)
  object Type {
    case object Default              extends Type(NoClass)
    case object Basic                extends Type("basic")
    case object Primary              extends Type("primary")
    case object Secondary            extends Type("secondary")
    case object Positive             extends Type("positive")
    case object Negative             extends Type("negative")
    case class  IconOnly(icon: Icon) extends Type("icon")
    implicit def univEq: UnivEq[Type] = UnivEq.derive
  }

  sealed abstract class State(c: ClassName, val disable: Boolean) extends HasClass(c)
  object State {
    case object Default  extends State(NoClass, false)
    case object Active   extends State("active", false)
    case object Disabled extends State(NoClass, true)
    case object Loading  extends State("loading", true)
    implicit def univEq: UnivEq[State] = UnivEq.derive
  }
}

case class Button(attr  : Multiple[Attr] = Multiple.empty,
                  tipe  : Type           = Type.Default,
                  state : State          = State.Default,
                  colour: Colour         = Colour.Default,
                  size  : Size           = Size.Default) {

  val tag = {
    var t = <.button(^.cls := "ui button" <+ attr <+ tipe <+ state <+ colour <+ size)

    if (state.disable)
      t = t(^.disabled := "disabled")

    tipe match {
      case Type.IconOnly(i) => t = t(i.tag)
      case _ => ()
    }

    t
  }
}
