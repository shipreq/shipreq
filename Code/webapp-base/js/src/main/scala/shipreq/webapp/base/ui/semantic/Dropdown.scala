package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.{Node, html}
import scala.scalajs.js
import shipreq.webapp.base.lib.DomUtil._

/** http://semantic-ui.com/modules/dropdown.html
  */
object Dropdown {

  sealed abstract class ItemState(c: ClassName) extends HasClass(c)
  object ItemState {
    case object Default  extends ItemState(NoClass)
    case object Loading  extends ItemState("loading")
    case object Error    extends ItemState("error")
    case object Active   extends ItemState("active")
    case object Disabled extends ItemState("disabled")
    implicit def univEq: UnivEq[ItemState] = UnivEq.derive

    def activeIf(b: Boolean): ItemState =
      if (b) Active else Default
  }

  sealed abstract class Item {
    val tag: VdomTag
  }

  object Item {
    private val item = "item"
    private val divItem = divCls(item)
    private val divHeader = divCls("header")

    case class Header(content: TagMod, state: ItemState = ItemState.Default) extends Item {
      override val tag = divHeader(content) <+ state
    }

    case class Div(content: TagMod, state: ItemState = ItemState.Default) extends Item {
      override val tag = divItem(content) <+ state

      /** Registers an onClick listener in such a way that avoids annoying Semantic UI settings a selected flag on the
        * item when clicked.
        */
      def withOnClick(getDOMNode: CallbackOption[Node], onClick: Callback) = {
        // Semantic UI doesn't respect CallbackOption.asEventDefault
        def onClick2: Callback =
          onClick >> getDOMNode.map(Dropdown.jquery(_).dropdown("clear"))
        copy(content = TagMod(content, ^.onClick --> onClick2))
      }
    }

    case class Link(a: VdomTagOf[html.Anchor], state: ItemState = ItemState.Default) extends Item {
      override val tag = a.addClass(item) <+ state
    }
  }

  type Items = Seq[Item]

  val itemValue = VdomAttr[String]("data-value")

  def enable(getDom       : CallbackTo[ComponentDom],
             clearActive  : Boolean = false,
             clearSelected: Boolean = false): Callback =
    for {
      n <- getDom.toCBO
      e <- CallbackOption.liftOption(n.toElement)
    } yield {
      val sel = ".ui.dropdown"
      var j = JQuery(e)
      if (!j.is(sel))
        j = j.find(sel)
      j.dropdown()
      if (clearActive) j.find(".active").toggleClass("active")
      if (clearSelected) j.find(".selected").toggleClass("selected")
      ()
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait JsOptions extends js.Object {
    import JsOptions._

    /** Dropdowns have multiple built-in actions that can occur on ITEM SELECTION. You can specify a built-in action by
      * passing its name to settings.action or pass a custom function that performs an action. */
    val action: js.UndefOr[Action] = js.undefined

    /** Event used to trigger dropdown. Default = click */
    var on: js.UndefOr[On] = js.undefined

    var `match` : js.UndefOr[Match   ] = js.undefined
    var delay   : js.UndefOr[Delay   ] = js.undefined
    var onChange: js.UndefOr[OnChange] = js.undefined
  //var debug   : js.UndefOr[Boolean ] = js.undefined
  //var verbose : js.UndefOr[Boolean ] = js.undefined
  }

  object JsOptions {

    sealed trait On extends js.Any
    object On {
      /** Dropdown appears when user clicks the dropdown */
      @inline def Click = "click".asInstanceOf[On]
      /** Dropdown appears when user hovers over the dropdown */
      @inline def Hover = "hover".asInstanceOf[On]
    }

    sealed trait Action extends js.Any
    object Action {
      /** Hides the dropdown menu and stores value, but does not change text */
      @inline def Hide = "hide".asInstanceOf[Action]
      @inline def nothing = "nothing".asInstanceOf[Action]

      def custom(f: Args => Unit): Action = {
        val j: js.Function3[String, String, html.Element, Unit] = (a, b, c) => f(Args(a, b, c))
        j.asInstanceOf[Action]
      }

      final case class Args(text: String, value: String, element: html.Element) {
        val downdown = element.findParent(_.classList.contains("dropdown")).get

        private val $ = shipreq.webapp.base.ui.semantic.JQuery(element).asInstanceOf[js.Dynamic]
        private val $$ = shipreq.webapp.base.ui.semantic.JQuery(downdown).asInstanceOf[js.Dynamic]

        def hide        (): Unit = $$.dropdown("hide")
        def clear       (): Unit = $$.dropdown("clear")
        def hideAndClear(): Unit = {hide(); clear()}
        def select      (): Unit = $.dropdown("set value", value)
      }
    }

    sealed trait Match extends js.Any
    object Match {
      @inline def Both  = "both".asInstanceOf[Match]
      @inline def Text  = "text".asInstanceOf[Match]
      @inline def Value = "value".asInstanceOf[Match]
    }

    trait Delay extends js.Object {
      val show: js.UndefOr[Int] = js.undefined
      val hide: js.UndefOr[Int] = js.undefined
    }

    // Arg = itemValue
    type OnChange = js.Function1[String, Unit]

    /** Allows dropdown item selection. */
    def default: JsOptions =
      new JsOptions {
        // This prevents Semantic UI's JS modifying the DOM when the user clicks an item
        override val action = Dropdown.JsOptions.Action.Hide
      }

    def readOnly: JsOptions =
      new JsOptions {
        // This prevents Semantic UI's JS:
        //   1. modifying the DOM when the user clicks an item
        //   2. marking items as selected/active when clicked
        override val action = Dropdown.JsOptions.Action.nothing
      }
  }

  implicit class JsOptionsOps(private val o: JsOptions) extends AnyVal {
    import JsOptions._

    def withOnChange(f: String => Unit): JsOptions = {
      o.onChange = (f: OnChange)
      o
    }

    def withOn(on: On): JsOptions = {
      o.on = on
      o
    }

    def onHover: JsOptions =
      withOn(On.Hover)

    def withDelay(show: js.UndefOr[Int] = js.undefined,
                  hide: js.UndefOr[Int] = js.undefined): JsOptions = {
      val s = show
      val h = show
      o.delay = new Delay {
        override val show = s
        override val hide = h
      }
      o
    }
  }

  def jquery(n: Node): JQuery =
    JQuery(n).find(".ui.dropdown")
}
