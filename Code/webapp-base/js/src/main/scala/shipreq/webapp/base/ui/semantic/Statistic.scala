package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
/** http://semantic-ui.com/views/statistic.html */
object Statistic {

  sealed abstract class Attr(cls: ClassName) extends HasClass(cls)
  object Attr {
    case object Horizontal extends Attr("horizontal")
    case object Inverted   extends Attr("inverted")
    implicit def univEq: UnivEq[Attr] = UnivEq.derive
  }

  case class Style(attr: Multiple[Attr] = Multiple.empty,
                   size: Size           = Size.Default) {

    val groupCont = divCls("statistic" <+ attr <+ size)
    def soloCont = groupCont.addClass("ui")
    def cont(solo: Boolean) = if (solo) soloCont else groupCont
  }

  val NoStyle = Style()

  def simple(value: TagMod, label: TagMod): Props =
    Props(NoStyle, Value(value), label)

  sealed abstract class Value {
    val cont: VdomTag
  }

  object Value {
    private val value    = "value"
    private val divValue = divCls(value)
    @inline def apply(content: TagMod) = Normal(content)
    case class Normal(content: TagMod) extends Value {
      override val cont = divValue(content)
    }
  }

  type Label = TagMod
  private val divLabel = divCls("label")

  final case class Props(style: Style,
                         value: Value,
                         label: Label) {

    @inline def render = Component(this)

    private[semantic] def render2(solo: Boolean) =
      style.cont(solo)(
        value.cont,
        divLabel(label))
  }

  val Component = ScalaFnComponent[Props](_.render2(true))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/** http://semantic-ui.com/views/statistic.html#statistic-group */
object StatisticGroup {

  case class Style(size: Size = Size.Default) {
    val cont = divCls("ui statistics" <+ size)
  }

  type Statistics = Seq[Statistic.Props]

  final case class Props(style: Style, statistics: Statistics) {
    @inline def render = Component(this)
  }

  private def render(p: Props) =
    p.style.cont(
      p.statistics.map(_.render2(false)): _*)

  val Component = ScalaFnComponent(render)
}

