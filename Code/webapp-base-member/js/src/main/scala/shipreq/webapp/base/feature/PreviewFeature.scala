package shipreq.webapp.base.feature

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.univeq._
import monocle.Lens
import scala.annotation.nowarn
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

    implicit def univEq: UnivEq[Status] = UnivEq.derive
    implicit val reusability: Reusability[Status] = Reusability.byUnivEq
  }

  object State {

    type Single = Option[Status]

    sealed trait Composite[Id] {
      def iterator(): Iterator[(Id, Status)]
      def get(id: Id): Single
      def set(id: Id, s: Single): Composite[Id]
      def merge[B](c: Composite[B])(i: Intersection[Id, B]): Composite[Id]

      final def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite.Mapped(this, i)
    }

    object Composite {

      def init[Id: UnivEq]: Root[Id] =
        Root(UnivEq.emptyMap)

      final case class Root[Id](previews: Map[Id, Status]) extends Composite[Id] {
        override def iterator() =
          previews.iterator

        override def get(id: Id): Single =
          previews.get(id)

        override def set(id: Id, o: Single): Composite[Id] =
          o match {
            case Some(s) => copy(previews.updated(id, s))
            case None    => copy(previews - id)
          }

        override def merge[B](c: Composite[B])(i: Intersection[Id, B]): Composite[Id] = {
          val m = Map.newBuilder[Id, Status]
          m ++= c.iterator().flatMap(i.reverse.strengthR[Status].getOption)
          for (e <- previews.iterator)
            if (i.getOption(e._1).isEmpty)
              // A is not part of B. Retain A's values
              m += e
          Root(m.result())
        }
      }

      private final case class Mapped[A, B](c: Composite[A], i: Intersection[A, B]) extends Composite[B] {
        override def iterator(): Iterator[(B, Status)] =
          c.iterator().flatMap(i.strengthR[Status].getOption)

        override def get(id: B): Single =
          i.reverse.fold(id, c.get)(None)

        override def set(id: B, s: Single): Composite[B] =
          i.reverse.fold(id, c.set(_, s).mapId(i))(this)

        override def merge[C](cc: Composite[C])(j: Intersection[B, C]): Composite[B] =
          copy(c.merge(cc)(i <=> j))
      }

      def at[Id](id: Id): Lens[Composite[Id], Single] =
        Lens[Composite[Id], Single](_.get(id))(s => _.set(id, s))

      def intersection[A, B](i: Intersection[A, B]): Lens[Composite[A], Composite[B]] =
        Lens[Composite[A], Composite[B]](_.mapId(i))(b => _.merge(b)(i))

      private def _reusability[Id]: Reusability[Composite[Id]] = {
        @nowarn("cat=unused") implicit val rd: Reusability[Map[Id, Status]] = Reusability.byRef
        implicit val rr: Reusability[Root[Id]] = Reusability.derive
        var r: Reusability[Composite[Id]] = new Reusability(null)
        r =  Reusability.byRef || Reusability((xx, yy) => xx match {
            case x: Root[Id] => yy match {
              case y: Root[Id] => rr.test(x, y)
              case _           => false
            }
            case x: Mapped[_, Id] => yy match {
              case y: Mapped[_, Id] => (x.i eq y.i) && r.test(x.c.asInstanceOf[Composite[Id]], y.c.asInstanceOf[Composite[Id]])
              case _                => false
            }
          })
        r
      }

      private val _reusabilityInstance: Reusability[Composite[Any]] =
        _reusability[Any]

      implicit def reusability[Id]: Reusability[Composite[Id]] =
        _reusabilityInstance.asInstanceOf[Reusability[Composite[Id]]]
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {
    final case class Single(status: State.Single) extends AnyVal {
      def showPreview(wantOpen: => Boolean): Boolean =
        status.exists(_.show || wantOpen)
    }

    object Single {
      val empty = apply(None)
    }

    sealed trait Composite[Id] {
      def apply(id: Id): Single

      final def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite.mapped(this, i)
    }

    object Composite {
      def apply[Id](s: State.Composite[Id]): Composite[Id] =
        new Composite[Id] {
          override def apply(id: Id): Single =
            Single(s.get(id))
        }

      private def mapped[A, B](s: Composite[A], i: Intersection[A, B]): Composite[B] =
        new Composite[B] {
          override def apply(b: B): Single =
            i.reverse.fold(b, s.apply)(Single.empty)
        }
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
      def apply($: StateAccessPure[State.Single]): Single = new Single {
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

    final case class Composite[Id]($: StateAccessPure[State.Composite[Id]]) {
      def apply(id: Id): Single =
        Single($ zoomStateL State.Composite.at(id))

      def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite($ zoomStateL State.Composite.intersection(i))

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

    final case class Composite[Id](read: Read.Composite[Id], write: Write.Composite[Id]) {
      def apply(id: Id): Single =
        Single(read(id), write(id))

      def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite(read mapId i, write mapId i)
    }
  }
}
