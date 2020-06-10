package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq

/** http://semantic-ui.com/collections/message.html */
object Message {

  def jquerySel = ".ui.message"

  sealed abstract class Attr(cls: ClassName) extends HasClass(cls)
  object Attr {
    case object Floating extends Attr("floating")
    case object Compact  extends Attr("compact")
    implicit def univEq: UnivEq[Attr] = UnivEq.derive
  }

  sealed abstract class Type(cls: ClassName) extends HasClass(cls)
  object Type {
    case object Default  extends Type(NoClass)
    case object Warning  extends Type("warning")
    case object Info     extends Type("info")
    case object Positive extends Type("positive")
    case object Success  extends Type("success")
    case object Negative extends Type("negative")
    case object Error    extends Type("error")
    implicit def univEq: UnivEq[Type] = UnivEq.derive
  }

  /*
  sealed abstract class State(c: ClassName) extends HasClass(c)
  object State {
    case object Default  extends State(NoClass)
    case object Hidden   extends State("hidden")
    case object Visible  extends State("visible")
    implicit def univEq: UnivEq[State] = UnivEq.derive
  }
  */

  case class Style(tipe  : Type           = Type.Default,
                   attr  : Multiple[Attr] = Multiple.empty,
                   colour: ColourPlus     = Colour.Default,
                   size  : Size           = Size.Default) {
    val tag = divCls("ui message" <+ tipe <+ attr <+ colour <+ size)
  }

  def apply(style: Style, icon: Icon, header: TagMod, content: TagMod): VdomTag =
    withBody(style, icon, header, <.p(content))

  def withBody(style: Style, icon: Icon, header: TagMod, body: VdomNode): VdomTag =
    style.tag(^.cls := "icon",
      icon.tag,
      <.div(^.cls := "content",
        <.div(^.cls := "header", header),
        body))
}
