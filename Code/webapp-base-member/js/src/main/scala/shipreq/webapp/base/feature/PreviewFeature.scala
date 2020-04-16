package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.univeq._
import monocle.Lens
import scalaz.Equal
import scalaz.syntax.equal.ToEqualOps
import shipreq.base.util.Intersection
import shipreq.webapp.base.jsfacade.ReactCollapse
import shipreq.webapp.base.lib.DataReusability._

/** Supplies logic to determine whether or not to show a preview for some rich-text editor.
  *
  * Preview will be available when:
  * - subject has focus
  * - subject wants the preview open
  * - focus has been maintained since preview was opened
  *
  *
  *                                    START
  *                                      |
  *                                      ↓
  *            focus !wantOpen      +--------+
  *         +---------------------  |        |            blur
  *         |                       |  None  |  <---------------------+
  *         |  +----------------->  |        |                        |
  *         |  |      blur          +--------+                        |
  *         |  |                       |  ↑                           |
  *         |  |                 focus |  | blur                      |
  *         |  |              wantOpen |  |                           |
  *         |  |                       |  |                           |
  *         ↓  |                       ↓  |       edit                |
  *     +----------+              +------------+  !wantOpen   +--------------+
  *     |          |              |            | -----------> |              |
  *     |  Closed  | -----------> |  NeedOpen  |              |  NeededOpen  |
  *     |          |   edit       |            | <----------- |              |
  *     +----------+   wantOpen   +------------+   edit       +--------------+
  *        ↑    |                     ↑    |       wantOpen        ↑    |
  *        |    |                     |    |                       |    |
  *        +----+                     +----+                       +----+
  *    edit !wantOpen              edit wantOpen               edit !wantOpen
  *
  *
  * Usage: Top-Most Component
  * =========================
  *
  * Create a type to use as an identifier of all possible editors that will use this feature.
  * Add `PreviewFeature.State[Id]` to the top-most component's state.
  * Initialise it with `PreviewFeature.State.init`.
  * In the component backend, add `val previewW = PreviewFeature.Write.Composite(…)`.
  *
  * Usage: Component with Multiple Editors
  * ======================================
  *
  * Add `PreviewFeature.ReadWrite.Composite[Id]` to the component's props.
  * Call `PreviewFeature.ReadWrite.Composite#apply(Id)` to get an instance of `PreviewFeature.ReadWrite.Single` to pass
  * down to a specific editor.
  *
  * Usage: Editor
  * =============
  *
  * Add `PreviewFeature.ReadWrite.Single` to the component's props.
  * Wire up all the `onXxxx` callbacks.
  * Use `showPreview` or similar to render a preview or not.
  */
object PreviewFeature {

  sealed abstract class Status(final val show: Boolean)
  object Status {
    case object Closed     extends Status(false)
    case object NeedOpen   extends Status(true)
    case object NeededOpen extends Status(true)

    implicit def equality: UnivEq[Status] = UnivEq.derive
    implicit def reusability: Reusability[Status] = Reusability.byUnivEq
  }

  type State[+Id] = Option[State.FocusData[Id]]

  @inline implicit class StateOps[Id](private val state: State[Id]) extends AnyVal {
    def filterId(id: Id)(implicit e: Equal[Id]): State[Id] =
      state.filter(_.key ≟ id)

    def mapId[A](i: Intersection[Id, A]): State[A] =
      state.flatMap(_ omap i.getOption)
  }

  object State {
    def init: State[Nothing] =
      None

    final case class FocusData[+K](key: K, status: Status) {
      def omap[A](f: K => Option[A]): Option[FocusData[A]] =
        f(key).map(FocusData(_, status))
    }

    implicit def reusabilityFocusData[Id: Reusability]: Reusability[FocusData[Id]] =
      Reusability.derive

    def status[Id: Equal](id: Id): Lens[State[Id], Option[Status]] =
      Lens[State[Id], Option[Status]](
        _.filterId(id).map(_.status))(
        newStatus => state => {
          val updated = newStatus.map(v => State.FocusData(id, v))
          updated.orElse {
            // updated=None means that we're clearing state
            // ...which is fine so long as we don't clear someone else's state
            state.filter(_.key ≠ id)
          }
        })

    def intersection[A, B](i: Intersection[A, B]): Lens[State[A], State[B]] =
      Lens[State[A], State[B]](_.mapId(i))(ob => _ => ob.mapId(i.reverse))
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {
    final case class Single(status: Option[Status]) extends AnyVal {
      def showPreview(wantOpen: => Boolean): Boolean =
        status.exists(_.show || wantOpen)
    }

    final case class Composite[+Id](state: State[Id]) extends AnyVal

    implicit def CompositeOps[Id](r: Composite[Id]): CompositeOps[Id] =
      new CompositeOps(r.state)

    final class CompositeOps[Id](private val state: State[Id]) extends AnyVal {
      def apply(id: Id)(implicit e: Equal[Id]): Single =
        Single(state.filterId(id)(e).map(_.status))

      def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite(state.mapId(i))
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {
    sealed trait Single {
      def onFocus(wantOpen: Boolean): Callback
      def onEdit(wantOpened: Boolean): Callback
      def onBlur: Callback
    }

    object Single {
      def apply($: StateAccessPure[Option[Status]]): Single = new Single {
        import Status._

        override def onFocus(wantOpen: Boolean): Callback = {
          val status = if (wantOpen) NeedOpen else Closed
          $.setState(Some(status))
        }

        override def onEdit(wantedOpen: Boolean): Callback =
          $.modState { prev =>
            val status =
              if (wantedOpen)
                NeedOpen
              else prev match {
                case Some(NeedOpen)
                   | Some(NeededOpen) => NeededOpen
                case Some(Closed)
                   | None             => Closed
              }
            Some(status)
          }

        override def onBlur: Callback =
          $.setState(None)
      }

      lazy val doNothing: Single = new Single {
        override def onFocus(wantOpen: Boolean) = Callback.empty
        override def onEdit (wantOpen: Boolean) = Callback.empty
        override def onBlur                     = Callback.empty
      }
    }

    final case class Composite[Id: Equal]($: StateAccessPure[State[Id]]) {
      def apply(id: Id): Single =
        Single($ zoomStateL State.status(id))

      def mapId[A: Equal](i: Intersection[Id, A]): Composite[A] =
        Composite($ zoomStateL State.intersection(i))

      def toReadWrite(r: Read.Composite[Id]): ReadWrite.Composite[Id] =
        ReadWrite.Composite(r, this)

      val toReadWriteCB: CallbackTo[ReadWrite.Composite[Id]] =
        $.state.map(s => toReadWrite(Read.Composite(s)))
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {
    final case class Single(read: Read.Single, write: Write.Single) {
      def onFocus(wantOpen: Boolean): Callback =
        write.onFocus(wantOpen)

      /** @param wantedOpen Did the pre-edit value want the editor open.
        *                   It's the pre-edit value in order to avoid re-parsing the same input twice (i.e. once in this
        *                   callback and once again in the next render cycle)
        */
      def onEdit(wantedOpen: Boolean): Callback =
        write.onEdit(wantedOpen)

      def onBlur: Callback =
        write.onBlur

      def showPreview(wantOpen: => Boolean): Boolean =
        read.showPreview(wantOpen)

      def reactCollapse(wantOpen: => Boolean) =
        ReactCollapse(showPreview(wantOpen))
    }

    object Single {
      def const(status: Option[Status]): Single =
        Single(Read.Single(status), Write.Single.doNothing)

      lazy val alwaysShow: Single = const(Some(Status.NeedOpen))
      lazy val neverShow : Single = const(None)
    }

    final case class Composite[Id: Equal](read: Read.Composite[Id], write: Write.Composite[Id]) {
      def apply(id: Id): Single =
        Single(read(id), write(id))

      def mapId[A: Equal](i: Intersection[Id, A]): Composite[A] =
        Composite(read mapId i, write mapId i)
    }
  }
}
