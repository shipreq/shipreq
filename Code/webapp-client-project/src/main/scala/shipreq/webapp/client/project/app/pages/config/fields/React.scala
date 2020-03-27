package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.extra.StateSnapshot
import scala.reflect.ClassTag

object ReactTemp { // TODO Remove after next scalajs-react upgrade

  final implicit def MonocleReactExt_StateSnapshotX[A](x: StateSnapshot[A]) = new InstanceX(x)

  final class InstanceX[A](private val self: StateSnapshot[A]) extends AnyVal {

    /** THIS WILL VOID REUSABILITY.
     *
     * The resulting `StateSnapshot[T]` will not be reusable.
     */
    def zoomStateO[B](o: monocle.Optional[A, B]): Option[StateSnapshot[B]] =
      o.getOption(self.value).map(StateSnapshot(_)((ob, cb) => self.modStateOption(a => ob.flatMap(o.setOption(_)(a)), cb)))

    def narrow[B <: A: ClassTag]: Option[StateSnapshot[B]] =
      self.value match {
        case b: B => Some(StateSnapshot(b)(self.setStateOption(_, _)))
        case _    => None
      }

    /** Unsafe because writes may be dropped. */
    def widenUnsafe[B >: A](implicit ct: ClassTag[A]): StateSnapshot[B] =
      StateSnapshot[B](self.value) { (ob, cb) =>
        val oa: Option[A] =
          ob match {
            case Some(a: A) => Some(a)
            case _          => None
          }
        self.setStateOption(oa, cb)
      }
  }

}
