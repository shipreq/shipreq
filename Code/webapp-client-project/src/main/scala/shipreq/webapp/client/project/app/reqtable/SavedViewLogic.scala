package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.extra.Reusability
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import scalaz.\/
import shipreq.base.util.PotentialChange
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.data.reqtable.SavedView.{Id, Name}
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.event.SavedViewGD
import shipreq.webapp.base.filter.Filter.Implicits.univEqFilterValid
import shipreq.webapp.base.lib.BaseReusability._
import shipreq.webapp.base.protocol.SavedViewCmd
import shipreq.webapp.base.validation.Simple

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
      unsortedItems.sortBy(_.name.value.toUpperCase)

    assert(items.whole.count(isActive) == 1)
  }

  object Menu {
    final case class NoSaved(unsaved: Item.Unsaved) extends Menu {
      override protected def unsortedItems = NonEmptyVector one unsaved
      override def isActive = _ => true
    }

    final case class SavedClean(default: Item.Default, nonDefaults: Vector[Item.NonDefault], active: Id) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults.toVector)
      override def isActive = _.optionId.exists(_ ==* active)
    }

    final case class SavedDirty(default: Item.Default, nonDefaults: Vector[Item.NonDefault], unsaved: Item.Unsaved) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults.toVector :+ unsaved)
      override def isActive = _.optionId.isEmpty
    }

    sealed trait Item {
      def optionId: Option[Id]
      def name: Name
      def default: Boolean
      def actions: NonEmptyVector[Action]
    }

    object Item {
      final case class Unsaved(saveAsNew: Action.SaveAsNew,
                               replace  : Option[Action.Replace]) extends Item {
        override def optionId = None
        override def name = Name("Unsaved view") // TODO Prohibit in name validation
        override def default = false
        override val actions = replace.fold(NonEmptyVector.one[Action](saveAsNew))(NonEmptyVector(saveAsNew, _))
      }

      sealed trait Saved extends Item {
        def id: Id
        final override val optionId = Some(id)
        override def actions: NonEmptyVector[Action.Saved]
      }

      final case class Default(id    : Id,
                               name  : Name,
                               rename: Action.Rename,
                               delete: Action.Delete) extends Saved {
        override def default = true
        override val actions = NonEmptyVector(rename, delete)
      }

      final case class NonDefault(id         : Id,
                                  name       : Name,
                                  makeDefault: Action.MakeDefault,
                                  rename     : Action.Rename,
                                  delete     : Action.Delete) extends Saved {
        override def default = false
        override val actions = NonEmptyVector(makeDefault, rename, delete)
      }

      def default(validate: Name => String \/ Name, sv: SavedView): Default =
        Item.Default(
          sv.id,
          sv.name,
          Action.rename(validate, sv),
          Action.delete(sv.id))

      def nonDefault(validate: Name => String \/ Name, sv: SavedView): NonDefault =
        Item.NonDefault(
          sv.id,
          sv.name,
          Action.makeDefault(sv.id),
          Action.rename(validate, sv),
          Action.delete(sv.id))
    }

    sealed trait Action

    object Action {
      sealed trait Unsaved extends Action
      sealed trait Saved   extends Action

      final case class SaveAsNew  (cmd: Name => String \/ SavedViewCmd.Create)                extends Unsaved
      final case class Rename     (cmd: Name => PotentialChange[String, SavedViewCmd.Update]) extends Saved
      final case class Replace    (cmd: SavedViewCmd.Update, name: Name)                      extends Unsaved
      final case class Delete     (cmd: SavedViewCmd.Delete)                                  extends Saved
      final case class MakeDefault(cmd: SavedViewCmd.MakeDefault)                             extends Saved

      def saveAsNew(validate: Name => String \/ Name, view: View): SaveAsNew =
        SaveAsNew(validate(_).map(SavedViewCmd.Create(_, view)))

      def makeDefault(id: Id): MakeDefault =
        MakeDefault(SavedViewCmd.MakeDefault(id))

      def delete(id: Id): Delete =
        Delete(SavedViewCmd.Delete(id))

      def rename(validate: Name => String \/ Name, sv: SavedView): Rename =
        Rename(i =>
          PotentialChange.fromDisjunction(validate(i))
            .ignore(_ ==* sv.name)
            .map(n => SavedViewCmd.Update(sv.id, SavedViewGD.Name(n))))
    }

    private def diff(ref: SavedView, activeView: View): Option[SavedViewGD.NonEmptyValues] = {
      def changedAttr[A: UnivEq, B](lens: Lens[View, A]): Option[A] = {
        val a = lens.get(ref.view)
        val b = lens.get(activeView)
        Option.unless(a ==* b)(b)
      }

      NonEmpty(
        SavedViewGD.values(
          SavedViewGD.attrs.iterator.map {
            case SavedViewGD.Name           => None
            case a @ SavedViewGD.Columns    => changedAttr(View.columns)   .map(a.apply)
            case a @ SavedViewGD.Order      => changedAttr(View.order)     .map(a.apply)
            case a @ SavedViewGD.FilterDead => changedAttr(View.filterDead).map(a.apply)
            case a @ SavedViewGD.Filter     => changedAttr(View.filter)    .map(a.apply)
          }.filterDefined))
    }

    def determine(savedViews: SavedViews.Optional,
                  state     : State,
                  activeView: View): Menu = {

      def validateName(id: Option[Id], name: Name): String \/ Name =
        Name.validator(Name.State(id, savedViews))
          .unnamed
          .apply(name.value)
          .leftMap(Simple.Invalidity.toText)

      def saveAsNew: Action.SaveAsNew =
        Action.saveAsNew(validateName(None, _), activeView)

      savedViews match {
        case None =>
          NoSaved(Item.Unsaved(saveAsNew, replace = None))

        case Some(svs) =>

          val default = {
            val sv = svs.default
            Item.default(validateName(Some(sv.id), _), sv)
          }

          val nonDefaults = svs.nonDefault
            .valuesIterator
            .map(sv => Item.nonDefault(validateName(Some(sv.id), _), sv))
            .toVector

          state.referenceViewId.flatMap(svs.get) match {
            case None =>
              state.manualView match {
                case None =>
                  SavedClean(default, nonDefaults, svs.default.id)
                case Some(_) =>
                  val dirtyItem = Item.Unsaved(saveAsNew, None)
                  SavedDirty(default, nonDefaults, dirtyItem)
              }
            case Some(ref) =>
              diff(ref, activeView) match {
                case None =>
                  SavedClean(default, nonDefaults, ref.id)
                case Some(changes) =>
                  val replace   = Action.Replace(SavedViewCmd.Update(ref.id, changes), ref.name)
                  val dirtyItem = Item.Unsaved(saveAsNew, Some(replace))
                  SavedDirty(default, nonDefaults, dirtyItem)
              }
          }
      }
    }
  }

}
