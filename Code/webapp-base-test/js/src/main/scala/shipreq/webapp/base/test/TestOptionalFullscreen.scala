package shipreq.webapp.base.test

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.PackageBase.VdomNode
import shipreq.webapp.base.ui.OptionalFullscreen

final case class TestOptionalFullscreen() extends OptionalFullscreen {

  var currentlyFullscreen = false

  private def set(fs: Boolean): Callback =
    Callback {
      currentlyFullscreen = fs
    }

  private val impl = new OptionalFullscreen.Impl(set(true), set(false))

  override def apply(f: OptionalFullscreen.Ctx => VdomNode): VdomNode =
    impl.Component(f)
}
