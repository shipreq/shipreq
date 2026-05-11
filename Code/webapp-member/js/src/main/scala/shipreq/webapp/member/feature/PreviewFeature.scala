package shipreq.webapp.member.feature

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import java.time.Duration
import monocle.Lens
import scala.reflect.ClassTag
import scalacss.ScalaCssReact._
import shipreq.base.util.Intersection
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Transition}
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.{BaseStyles => *, OnlyVisibleOnMouseMove}

/** Supplies logic to determine whether or not to show a preview for some rich-text editor.
  *
  * By default, preview will be available when:
  * - subject has focus
  * - subject wants the preview open
  * - focus has been maintained since preview was opened
  *
  * Alternatively, you can use [[PreviewFeature.ReadWrite.Single.alwaysShow]] and similar.
  *
  * The normal state transition process is shown below.
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
  * It ignores the [[PreviewFeature.Status.Manual]] state, which can be manually set via
  * [[PreviewFeature.Write.Single.setManually()]].
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

  sealed trait Position
  object Position {
    case object Right extends Position
    case object Under extends Position

    implicit def univEq: UnivEq[Position] = UnivEq.derive
    implicit val reusability: Reusability[Position] = Reusability.byUnivEq
    def values = AdtMacros.adtValues[Position]
  }

  sealed abstract class Status(final val show: Boolean)
  object Status {
    case object Closed                                    extends Status(false)
    case object NeedOpen                                  extends Status(true)
    case object NeededOpen                                extends Status(true)
    final case class Manual(open: Boolean, pos: Position) extends Status(open)

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
        implicit val rd: Reusability[Map[Id, Status]] = Reusability.byRef
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
        _reusabilityInstance.unsafeSubst
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {
    final case class Single(status: State.Single) {
      def showPreview(wantOpen: => Boolean): Boolean =
        status match {
          case Some(m: Status.Manual) => m.show
          case Some(s)                => s.show || wantOpen
          case None                   => false
        }

      def isManual: Boolean =
        status match {
          case Some(_: Status.Manual) => true
          case _                      => false
        }

      def showManuallyControlledPreview(default: Boolean): Boolean =
        status match {
          case Some(m: Status.Manual) => m.show
          case _                      => default
        }

      def position(default: Position): Position =
        status match {
          case Some(m: Status.Manual) => m.pos
          case _                      => default
        }
    }

    object Single {
      val empty: Single =
        apply(None)

      implicit val reusability: Reusability[Single] =
        Reusability.derive
    }

    sealed trait Composite[Id] {
      def apply(id: Id): Single

      final def mapId[A](i: Reusable[Intersection[Id, A]]): Composite[A] =
        Composite.mapped(this, i)
    }

    object Composite {

      def apply[Id](s: State.Composite[Id]): Composite[Id] =
        Basic(s)

      private final case class Basic[Id](s: State.Composite[Id]) extends Composite[Id] {
        override def apply(id: Id): Single =
          Single(s.get(id))
      }

      private def mapped[A, B](s: Composite[A], i: Reusable[Intersection[A, B]]): Composite[B] =
        Mapped(s, i)

      private final case class Mapped[A, B](s: Composite[A], i: Reusable[Intersection[A, B]]) extends Composite[B] {
        override def apply(b: B): Single =
          i.reverse.fold(b, s.apply)(Single.empty)
      }

      private case object Empty extends Composite[Any] {
        override def apply(id: Any) =
          Single.empty
      }

      def empty[Id]: Composite[Id] =
        Empty.asInstanceOf[Composite[Id]]

      private def reusabilityBasic[Id]   : Reusability[Basic[Id]]    = Reusability.derive
      private def reusabilityMapped[A, B]: Reusability[Mapped[A, B]] = Reusability.derive

      private def _reusability[Id]: Reusability[Composite[Id]] = {
        val basic = reusabilityBasic[Id]
        val mapped = reusabilityMapped[Any, Id]
        type M = Mapped[Any, Id]

        Reusability[Composite[Id]]((x, y) =>
          x match {
            case a: Basic[Id] =>
              y match {
                case b: Basic[Id] => basic.test(a, b)
                case _            => false
              }
            case a: Mapped[_, Id] =>
              y match {
                case b: Mapped[_, Id] => mapped.test(a.asInstanceOf[M], b.asInstanceOf[M])
                case _                => false
              }
            case Empty =>
              Empty == y
          }
        )
      }

      private val reusabilityAny: Reusability[Composite[Any]] = _reusability
      implicit def reusability[Id]: Reusability[Composite[Id]] = reusabilityAny.unsafeSubst
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {
    sealed trait Single {
      def onFocus(wantOpen: Boolean): Callback
      def onEdit(wantOpened: Boolean): Callback
      def onBlur: Callback
      def clear: Callback
      def setManually(m: Status.Manual): Callback
    }

    object Single {
      def apply($: Reusable[StateAccessPure[State.Single]]): Single =
        Basic($)

      private final case class Basic($: Reusable[StateAccessPure[State.Single]]) extends Single {
        import Status._

        override def onFocus(wantOpen: Boolean): Callback = {
          $.modStateOption {
            case Some(_: Manual) => None
            case prev =>
              val newStatus = if (wantOpen) NeedOpen else Closed
              val noChange = prev.contains(newStatus)
              Option.unless(noChange)(Some(newStatus))
          }
        }

        override def onEdit(wantedOpen: Boolean): Callback =
          $.modStateOption {
            case Some(_: Manual) => None
            case prev =>
              val newStatus: Status =
                if (wantedOpen)
                  NeedOpen
                else prev match {
                  case Some(NeedOpen)
                     | Some(NeededOpen) => NeededOpen
                  case Some(Closed)
                     | None             => Closed
                  case Some(m: Manual)  => m
                }
              val noChange = prev.contains(newStatus)
              Option.unless(noChange)(Some(newStatus))
          }

        override def onBlur: Callback =
          $.modStateOption {
            case Some(_: Manual) => None
            case _               => Some(None)
          }.delayMs(100).toCallback // This delay is so that when EditControlsFeature.OpenPreview is MinimallyWithControls and a
                                    // user clicks move-right, there is enough time for the move-right button to receive
                                    // the click before the preview animates into non-existence.

        override def clear: Callback =
          $.modStateOption {
            case Some(_) => Some(None)
            case None    => None
          }

        override def setManually(m: Manual): Callback =
          $.setState(Some(m))
      }

      def doNothing: Single =
        DoNothing

      private case object DoNothing extends Single {
        override def onFocus(wantOpen: Boolean)    = Callback.empty
        override def onEdit (wantOpen: Boolean)    = Callback.empty
        override def onBlur                        = Callback.empty
        override def clear                         = Callback.empty
        override def setManually(m: Status.Manual) = Callback.empty
      }

      implicit val reusability: Reusability[Single] =
        Reusability.derive
    }

    final case class Composite[Id]($: Reusable[StateAccessPure[State.Composite[Id]]]) {
      def apply(id: Id)(implicit r: Reusability[Id], ct: ClassTag[Id]): Single =
        Single(
          Reusable.ap($, r.reusable(id))(($, id) =>
            $ zoomStateL State.Composite.at(id)))

      def mapId[A](i: Reusable[Intersection[Id, A]]): Composite[A] =
        Composite(
          Reusable.ap($, i)(($, i) =>
            $ zoomStateL State.Composite.intersection(i)))

      def toReadWrite(r: Read.Composite[Id]): ReadWrite.Composite[Id] =
        ReadWrite.Composite(r, this)

      val toReadWriteCB: CallbackTo[ReadWrite.Composite[Id]] =
        $.state.map(s => toReadWrite(Read.Composite(s)))
    }

    object Composite {
      private val _empty: Composite[Any] = {
        val getState = CallbackTo.pure(State.Composite.init[Any](UnivEq.force))
        apply(Reusable.byRef(StateAccess.const(getState)))
      }

      def empty[Id]: Composite[Id] =
        _empty.asInstanceOf[Composite[Id]]

      private val reusabilityAny: Reusability[Composite[Any]] =
        Reusability.derive

      implicit def reusability[Id]: Reusability[Composite[Id]] =
        reusabilityAny.unsafeSubst
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    private object ManualControls {
      private def button(tip: String, icon: Icon) =
        Button(
          tipe = Button.Type.IconOnly(icon),
          colour = Colour.Blue,
        ).tag(^.title := tip, TableNavigationFeature.ignore)

      val show  = button("Show preview", Icon.WindowRestore)
      val hide  = button("Hide preview", Icon.Close)
      val down  = button("Move down"   , Icon.AngleDoubleDown)
      val right = button("Move right"  , Icon.AngleDoubleRight)

      val wrapper1 = <.div(*.previewToggleWrapper1)
      val wrapper2 = <.div(*.previewToggleWrapper2)

      val decay = Duration.ofMillis(1200)
    }

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

      def clear: Callback =
        write.clear

      def manualControls(defaultPosition      : Position,
                         previewIsShown       : Boolean,
                         showControlsInitially: Boolean): VdomTag = {
        import Status.Manual

        val position =
          read.status match {
            case Some(m: Manual) => m.pos
            case _               => defaultPosition
          }

        val manualControls: VdomTag = {
          def setOnClick(m: Manual) = ^.onClick --> write.setManually(m)
          val inner: TagMod =
            if (previewIsShown) {
              val hide  = ManualControls.hide (setOnClick(Manual(false, position)))
              def down  = ManualControls.down (setOnClick(Manual(true, Position.Under)))
              def right = ManualControls.right(setOnClick(Manual(true, Position.Right)))
              position match {
                case Position.Right => TagMod(*.previewButtonsWhenRight, hide, down)
                case Position.Under => TagMod(*.previewButtonsWhenUnder, right, hide)
              }
            } else
              ManualControls.show(setOnClick(Manual(true, position)))
          ManualControls.wrapper2(inner)
        }

        val props =
          OnlyVisibleOnMouseMove.Props(
            content       = manualControls,
            transition    = Transition.fade,
            direction     = Transition.Direction.left,
            decay         = ManualControls.decay,
            showInitially = showControlsInitially,
          )

        ManualControls.wrapper1(props.render)
      }
    }

    object Single {
      def const(status: Option[Status]): Single =
        Single(Read.Single(status), Write.Single.doNothing)

      def show(show: Boolean): Single =
        if (show) alwaysShow else neverShow

      lazy val alwaysShow: Single = const(Some(Status.NeedOpen))
      lazy val neverShow : Single = const(None)

      implicit val reusability: Reusability[Single] =
        Reusability.derive
    }

    final case class Composite[Id](read: Read.Composite[Id], write: Write.Composite[Id]) {
      def apply(id: Id)(implicit r: Reusability[Id], ct: ClassTag[Id]): Single =
        Single(read(id), write(id))

      def mapId[A](i: Reusable[Intersection[Id, A]]): Composite[A] =
        Composite(read mapId i, write mapId i)
    }

    object Composite {
      private val _empty: Composite[Any] =
        apply(Read.Composite.empty, Write.Composite.empty)

      def empty[Id]: Composite[Id] =
        _empty.asInstanceOf[Composite[Id]]

      private val reusabilityAny: Reusability[Composite[Any]] =
        Reusability.derive

      implicit def reusability[Id]: Reusability[Composite[Id]] =
        reusabilityAny.unsafeSubst
    }
  }
}
