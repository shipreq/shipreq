package shipreq.webapp.base.feature.clipboard

import japgolly.scalajs.react._
import japgolly.univeq.UnivEq
import scala.scalajs.js

object Clipboard {

  private var clipboard: Clipboard = {
    val clipboard: Option[AsyncCallback[ClipboardJs]] =
      ClipboardJs.instance.toOption.map(AsyncCallback.pure)

    def async[A](f: ClipboardJs => js.Promise[A]): Option[AsyncCallback[A]] =
      clipboard.map(_.flatMap(c => AsyncCallback.fromJsPromise(f(c))))

    val readText: Option[AsyncCallback[String]] =
      async(_.readText())

    new Clipboard {
      override val read: AsyncCallback[ClipboardData] =
        readText
          .getOrElse(AsyncCallback.pure(""))
          .map(Option(_).getOrElse(""))
          .map(ClipboardData.apply)

      override def write(d: ClipboardData): AsyncCallback[Unit] =
        async(_.writeText(d.text)).getOrElse(AsyncCallback.unit)
    }
  }

  /** for unit tests */
  private[clipboard] def setClipboardImpl(c: Clipboard): Unit =
    this.clipboard = c

  def instance: Clipboard =
    clipboard
}

// =====================================================================================================================

abstract class Clipboard private[clipboard] {
  def read: AsyncCallback[ClipboardData]
  def write(d: ClipboardData): AsyncCallback[Unit]
}

// =====================================================================================================================

// I'll improve this later to handle more than just text
final case class ClipboardData(text: String)

object ClipboardData {
  implicit def univEq     : UnivEq     [ClipboardData] = UnivEq.derive
  implicit def reusability: Reusability[ClipboardData] = Reusability.derive
}
