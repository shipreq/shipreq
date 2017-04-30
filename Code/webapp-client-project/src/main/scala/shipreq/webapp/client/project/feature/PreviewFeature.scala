package shipreq.webapp.client.project.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.Lens
import scalaz.Equal
import scalaz.std.option.optionEqual
import scalaz.syntax.equal.ToEqualOps
import shipreq.base.util.Intersection
import shipreq.webapp.client.base.jsfacade.ReactCollapse

/** Supplies logic to determine whether or not to show a preview for some rich-text editor.
  *
  * Preview will be available when:
  * - editing
  * - and focused
  * - and either dirty, or when editor has been edited since receiving focus
  *
  * Usage: Top-Most Component
  * =========================
  *
  * Create a type to use as an identifier of all possible editors that will use this feature.
  * Add `PreviewFeature.State[Id]` to the top-most component's state.
  * Initialise it with `PreviewFeature.State.init`.
  * In the component backend, add `val previewFeature = PreviewFeature.Write.Composite.init(…)`.
  * In the render method, call `previewFeature.toReadWrite` with the latest state and pass the props to children.
  *
  * Usage: Component with Multiple Editors
  * ======================================
  *
  * Add `PreviewFeature.ReadWrite.Composite[Id]` to the component's props.
  * Call `PreviewFeature.ReadWrite.Composite#apply(Id)` to get an instance of `PreviewFeature.Props.Single` to pass down to
  * children.
  *
  * Usage: Editor
  * =============
  *
  * Add `PreviewFeature.ReadWrite.Single` to the component's props.
  * Wire up all the `onXxxx` callbacks.
  * Use `show_?` or similar to render a preview or not.
  */
object PreviewFeature {

  type State[+Id] = Option[State.FocusData[Id]]

  object State {
    def init: State[Nothing] =
      None

    final case class FocusData[+K](key: K, changedSinceFocus: Boolean) {
      def omap[A](f: K => Option[A]): Option[FocusData[A]] =
        f(key).map(FocusData(_, changedSinceFocus))
    }

    implicit def reusabilityFocusData[Id: Reusability]: Reusability[FocusData[Id]] =
      Reusability.caseClass
  }

  @inline implicit class StateOps[Id](private val s: State[Id]) extends AnyVal {
    def mapId[A](i: Intersection[Id, A]): State[A] =
      s.flatMap(_ omap i.getOption)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {

    trait Single {
      def onFocus: Callback
      def onBlur: Callback
      def onEdit: Callback

      def show_?(isDirty: => Boolean): Boolean

      final def showOption[A](isDirty: => Boolean)(a: => A): Option[A] =
        if (show_?(isDirty)) Some(a) else None

      final def reactCollapse[A](isDirty: => Boolean) =
        ReactCollapse(show_?(isDirty))
    }

    object Single {
      def const(show: Boolean): Single =
        new Single {
          override def onFocus                     = Callback.empty
          override def onBlur                      = Callback.empty
          override def onEdit                      = Callback.empty
          override def show_?(isDirty: => Boolean) = show
        }
    }

    // =================================================================================================================

    final case class Composite[Id]($: Reusable[StateAccessPure[State[Id]]])(implicit ei: Equal[Id], ri: Reusability[Id]) {

      // I don't like the thought of 2 new instances per field, per req on every render on every screen in the SPA
      private val cachedReusability =
        ReadWrite.Single.reusabilityForKey[Id]

      def apply(id: Id, state: State[Id]): ReadWrite.Single =
        Reusable.explicitly(ReadWrite.Single.ForKey(id, state, $))(cachedReusability)

      def mapId[A](i: Intersection[Id, A]): Composite[A] = {
        val lens = Lens[State[Id], State[A]](_.mapId(i))(oa => _ => oa.mapId(i.reverse))

        val ea: Equal[A] = optionEqual[Id].contramap(i.reverse.getOption)
        val ka: Reusability[A] = Reusability.option[Id].contramap(i.reverse.getOption)

        // Not affecting $ reusability here because we can trust Intersection to be coherent
        Composite($.map(_ zoomStateL lens))(ea, ka)
      }

      def toReadWrite(state: State[Id]): ReadWrite.Composite[Id] =
        ReadWrite.Composite(this, state)

      def stateCB: CallbackTo[State[Id]] =
        $.state
    }

    object Composite {
      def init[Id: Equal: Reusability]($: StateAccessPure[State[Id]]): Composite[Id] =
        apply(Reusable.byRef($))

      implicit def reusability[Id: Reusability]: Reusability[Composite[Id]] =
        Reusability.caseClass
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    type Single = Reusable[Write.Single]

    object Single {
      lazy val AlwaysShow: Single =
        Reusable.byRef(Write.Single.const(show = true))

      lazy val NeverShow: Single =
        Reusable.byRef(Write.Single.const(show = false))

      final case class ForKey[Id: Equal](id: Id, state: State[Id], $: Reusable[StateAccessPure[State[Id]]]) extends Write.Single {
        import State.FocusData

        private val hasThisId: FocusData[Id] => Boolean =
          _.key === id

        def onFocus: Callback =
          $.modState(s =>
            if (s exists hasThisId)
              s
            else
              Some(FocusData(id, changedSinceFocus = false)))

        def onBlur: Callback =
          $.state.flatMap(s =>
            $.setState(None).when_(s exists hasThisId))

        def onEdit: Callback =
          $.setState(Some(FocusData(id, changedSinceFocus = true)))

        def show_?(isDirty: => Boolean): Boolean =
          state
            .filter(hasThisId)
            .exists(_.changedSinceFocus || isDirty)
      }

      implicit def reusabilityForKey[Id: Reusability]: Reusability[ForKey[Id]] =
        Reusability.caseClass
    }

    // =================================================================================================================

    final case class Composite[Id](write: Write.Composite[Id], read: State[Id]) {
      def apply(id: Id): ReadWrite.Single =
        write(id, read)

      def mapId[A](i: Intersection[Id, A]): Composite[A] =
        Composite(write mapId i, read mapId i)
    }

    object Composite {
      implicit def reusability[Id: Reusability]: Reusability[Composite[Id]] =
        Reusability.caseClass
    }
  }

}
