package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.html
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
    case object Default                                    extends Type(NoClass)
    case object Basic                                      extends Type("basic")
    case class  BasicIconAndText(icon: Icon, text: TagMod) extends Type("basic")
    case class  BasicIconOnly   (icon: Icon)               extends Type("basic")
    case class  IconAndText     (icon: Icon, text: TagMod) extends Type(NoClass)
    case class  IconOnly        (icon: Icon)               extends Type("icon")
    // implicit def univEq: UnivEq[Type] = UnivEq.derive
  }

  sealed abstract class State(c: ClassName, val disable: Boolean) extends HasClass(c)
  object State {
    case object Default  extends State(NoClass, false)
    case object Active   extends State("active", false)
    case object Disabled extends State(NoClass, true)
    case object Loading  extends State("loading", true)
    implicit def univEq: UnivEq[State] = UnivEq.derive
    def enabledWhen(e: Boolean): State = if (e) Default else Disabled
    @inline def disabledWhen(d: Boolean): State = enabledWhen(!d)
  }

  def group(bs: VdomTagOf[html.Button]*) =
    divCls("ui buttons")(bs: _*)
}

final case class Button(attr  : Multiple[Attr] = Multiple.empty,
                        tipe  : Type           = Type.Default,
                        state : State          = State.Default,
                        colour: ColourPlus     = ColourPlus.Default,
                        size  : Size           = Size.Default) {

  val tag: VdomTagOf[html.Button] = {
    var t = <.button(^.cls := "ui button" <+ attr <+ tipe <+ state <+ colour <+ size)

    if (state.disable)
      t = t(^.disabled := true)

    t = tipe match {
      case Type.BasicIconAndText(i, x) => t(i.tag, x, ^.whiteSpace.pre) // whiteSpace.pre keeps icon & text on same line
      case Type.IconAndText     (i, x) => t(i.tag, x)
      case Type.IconOnly        (i)    => t(i.tagNoMargin)
      case Type.BasicIconOnly   (i)    => t(i.tagNoMargin)
      case _                           => t
    }

    t
  }
}
