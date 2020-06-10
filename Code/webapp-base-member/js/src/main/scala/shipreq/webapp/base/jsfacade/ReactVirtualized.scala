package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.PackageBase._
import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._

object ReactVirtualized {

  // https://github.com/bvaughn/react-virtualized/blob/master/docs/AutoSizer.md
  object AutoSize {
    @JSGlobal("RVAS")
    @js.native
    private object component extends js.Any

    @js.native
    @nowarn("cat=unused")
    sealed trait Dimensions extends js.Object {
      val width : Double = js.native
      val height: Double = js.native
    }

    type Children = js.Function1[Dimensions, raw.React.Node]

    @js.native
    trait Props extends js.Object {
      var children: Children = js.native
      // className     : String   // Optional custom CSS class name to attach to root AutoSizer element. This is an advanced property and is not typically necessary.
      // defaultHeight : Number   // Height passed to child for initial render; useful for server-side rendering. This value will be overridden with an accurate height after mounting.
      // defaultWidth  : Number   // Width passed to child for initial render; useful for server-side rendering. This value will be overridden with an accurate width after mounting.
      // disableHeight : Boolean  // Fixed height; if specified, the child's height property will not be managed
      // disableWidth  : Boolean  // Fixed width; if specified, the child's width property will not be managed
      // nonce         : String   // Nonce of the inlined stylesheets for Content Security Policy
      // onResize      : Function // Callback to be invoked on-resize; it is passed the following named parameters: ({ height: number, width: number }).
      // style         : Object   // Optional custom inline style to attach to root AutoSizer element. This is an advanced property and is not typically necessary.
    }

    val Component = JsComponent[Props, Children.None, Null](component)

    def apply(render        : Dimensions => VdomNode,
              requireNonZero: Boolean = true): VdomElement = {

      val p = (new js.Object).asInstanceOf[Props]
      p.children = dims => {
        // println(s"dims = ${dims.width} x ${dims.height}")
        if (requireNonZero && (dims.width == 0 || dims.height == 0))
          null
        else
          render(dims).rawNode
      }
      Component(p)
    }

  }
}
