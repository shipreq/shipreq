package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.extra.Reusability
import japgolly.univeq._
import monocle.macros.Lenses
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.data.reqtable.SavedView.Id
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.lib.BaseReusability._

object SavedViewLogic {

  implicit class SavedViewsOptionalOps[A](private val self: SavedViews.Optional) extends AnyVal {
    def get(id: Id): Option[SavedView] =
      self.flatMap(_.get(id))
  }

  /** @param manualView      The view that exists because the user has manually changed the view.
    * @param referenceViewId The view that the user has either selected manually, or was selected when they modified the view.
    */
  @Lenses
  final case class State(manualView: Option[View], referenceViewId: Option[Id]) {

    /** The view that should (will) be presented to the user. */
    def activeView(savedViews: SavedViews.Optional, filterDeadFallback: FilterDead): View =
      manualView
        .orElse(referenceViewId.flatMap(id => savedViews.get(id).map(_.view)))
        .orElse(savedViews.map(_.default.view))
        .getOrElse(View.default(filterDeadFallback))
  }

  object State {
    def init: State = apply(None, None)
    implicit def univEq: UnivEq[State] = UnivEq.derive
    implicit def reusability: Reusability[State] = Reusability.byUnivEq
  }

  sealed abstract class Action
  object Action {
    final case class Modify(view: View) extends Action
    final case class Select(id: Id) extends Action
    final case class Delete(id: Id) extends Action

    // This is not part of the algebra because if it is applied before the async save completes and the savedviews are
    // updated, then the view will be out-of-sync... At least on failure...
    // Instead a Select should be applied after the Save has completed.
    // final case class Save(id: Id) extends Action

    def interpret(savedViews: SavedViews.Optional): Action => State => State = {
      def defaultSavedViewId = savedViews.map(_.default.id)

      {
        case Modify(view) =>
          s => State(Some(view), s.referenceViewId orElse defaultSavedViewId)

        case Select(id) =>
          _ => State(None, Some(id))

        case Delete(id) =>
          s => {
            def visible = s.referenceViewId orElse defaultSavedViewId
            State(
              s.manualView orElse visible.filter(_ ==* id).flatMap(savedViews.get(_).map(_.view)),
              s.referenceViewId.filter(_ !=* id))
          }
      }
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  sealed trait Menu {
    protected def unsortedItems: NonEmptyVector[Menu.Item]
    def isActive: Menu.Item => Boolean

    final val items: NonEmptyVector[Menu.Item] =
      unsortedItems.sortBy(_.name.toUpperCase)

    assert(items.whole.count(isActive) == 1)
  }

  object Menu {
    case object NoSaved extends Menu {
      override protected def unsortedItems = NonEmptyVector one Item.Unsaved(Action.dirty(None))
      override def isActive = _ => true
    }

    final case class SavedClean(default: Item.Default, nonDefaults: Set[Item.NonDefault], active: Id) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults.toVector)
      override def isActive = _.optionId.exists(_ ==* active)
    }

    final case class SavedDirty(default: Item.Default, nonDefaults: Set[Item.NonDefault], unsaved: Item.Unsaved) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults.toVector :+ unsaved)
      override def isActive = _.optionId.isEmpty
    }

    sealed trait Item {
      def optionId: Option[Id]
      def name: String
      def default: Boolean
      def actions: NonEmptyVector[Action]
    }

    object Item {
      final case class Unsaved(actions: NonEmptyVector[Action.Unsaved]) extends Item {
        override def optionId = None
        override def name = "Unsaved view" // TODO Prohibit in name validation
        override def default = false
      }

      sealed trait Saved extends Item {
        def id: Id
        final override val optionId = Some(id)
        override def actions: NonEmptyVector[Action.Saved]
      }

      final case class Default(id: Id, name: String) extends Saved {
        override def default = true
        def actions: NonEmptyVector[Action.Saved] = Action.default
      }

      final case class NonDefault(id: Id, name: String) extends Saved {
        override def default = false
        def actions: NonEmptyVector[Action.Saved] = Action.nonDefault
      }

      def dirty(refView: Option[SavedView]): Unsaved =
        Unsaved(Action.dirty(refView))

      def default(v: SavedView): Default =
        Default(v.id, v.name.value)

      def nonDefault(v: SavedView): NonDefault =
        NonDefault(v.id, v.name.value)
    }

    sealed trait Action
    object Action {
      sealed trait Unsaved extends Action
      sealed trait Saved   extends Action

      case object SaveAsNew                          extends Unsaved
      case object Rename                             extends Saved
      case object Delete                             extends Saved
      case object SetAsDefault                       extends Saved
      final case class Replace(id: Id, name: String) extends Unsaved

      val default: NonEmptyVector[Action.Saved] =
        NonEmptyVector(Rename, Delete)

      val nonDefault: NonEmptyVector[Action.Saved] =
        NonEmptyVector(SetAsDefault, Rename, Delete)

      def dirty(refView: Option[SavedView]): NonEmptyVector[Unsaved] = {
        var a = NonEmptyVector one[Unsaved] SaveAsNew
        refView.foreach(v => a :+= Action.Replace(v.id, v.name.value))
        a
      }
    }

    implicit def univEqActionU: UnivEq[Action.Unsaved ] = UnivEq.derive
    implicit def univEqItemD  : UnivEq[Item.Default   ] = UnivEq.derive
    implicit def univEqItemND : UnivEq[Item.NonDefault] = UnivEq.derive
    implicit def univEqItemS  : UnivEq[Item.Saved     ] = UnivEq.derive
    implicit def univEqItemU  : UnivEq[Item.Unsaved   ] = UnivEq.derive
    implicit def univEq       : UnivEq[Menu           ] = UnivEq.derive

    def determine(savedViews: SavedViews.Optional,
                  state     : State,
                  activeView: View): Menu =
      savedViews match {
        case None =>
          NoSaved
        case Some(svs) =>
          val ref         = state.referenceViewId.flatMap(svs.get).getOrElse(svs.default)
          val clean       = ref.view ==* activeView
          val default     = Item.default(svs.default)
          val nonDefaults = svs.nonDefault.valuesIterator.map(Item.nonDefault).toSet
          if (clean)
            SavedClean(default, nonDefaults, ref.id)
          else
            SavedDirty(default, nonDefaults, Item.dirty(Some(ref)))
      }
  }

}
