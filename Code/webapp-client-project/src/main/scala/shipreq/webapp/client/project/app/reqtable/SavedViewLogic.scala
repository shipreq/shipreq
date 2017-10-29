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
    protected def unsortedItems: NonEmptyVector[MenuItem]
    def isActive: MenuItem => Boolean

    final val items: NonEmptyVector[MenuItem] =
      unsortedItems.sortBy(_.name.value.toUpperCase)

    assert(items.whole.count(isActive) == 1)
  }

  object Menu {
    final case class NoSaved(unsaved: MenuItem.Unsaved) extends Menu {
      override protected def unsortedItems = NonEmptyVector one unsaved
      override def isActive = _ => true
    }

    final case class SavedClean(default: MenuItem.Default, nonDefaults: Vector[MenuItem.NonDefault], active: Id) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults)
      override def isActive = _.optionId.exists(_ ==* active)
    }

    final case class SavedDirty(default: MenuItem.Default, nonDefaults: Vector[MenuItem.NonDefault], unsaved: MenuItem.Unsaved) extends Menu {
      override protected def unsortedItems = NonEmptyVector(default, nonDefaults :+ unsaved)
      override def isActive = _.optionId.isEmpty
    }
  }

  sealed trait MenuItem {
    def optionId: Option[Id]
    def name: Name
    def default: Boolean
    def actions: NonEmptyVector[MenuAction]
  }

  object MenuItem {
    final case class Unsaved(saveAsNew: MenuAction.SaveAsNew,
                             replace  : Option[MenuAction.Replace]) extends MenuItem {
      override def optionId = None
      override def name = Name("Unsaved view") // TODO Prohibit in name validation
      override def default = false
      override val actions = replace.fold(NonEmptyVector.one[MenuAction](saveAsNew))(NonEmptyVector(saveAsNew, _))
    }

    sealed trait Saved extends MenuItem {
      def id: Id
      final override val optionId = Some(id)
      override def actions: NonEmptyVector[MenuAction.Saved]
    }

    final case class Default(id    : Id,
                             name  : Name,
                             rename: MenuAction.Rename,
                             delete: MenuAction.Delete) extends Saved {
      override def default = true
      override val actions = NonEmptyVector(rename, delete)
    }

    final case class NonDefault(id         : Id,
                                name       : Name,
                                makeDefault: MenuAction.MakeDefault,
                                rename     : MenuAction.Rename,
                                delete     : MenuAction.Delete) extends Saved {
      override def default = false
      override val actions = NonEmptyVector(makeDefault, rename, delete)
    }

    def default(validate: String => String \/ Name, sv: SavedView): Default =
      MenuItem.Default(
        sv.id,
        sv.name,
        MenuAction.rename(validate, sv),
        MenuAction.delete(sv))

    def nonDefault(validate: String => String \/ Name, sv: SavedView): NonDefault =
      MenuItem.NonDefault(
        sv.id,
        sv.name,
        MenuAction.makeDefault(sv.id),
        MenuAction.rename(validate, sv),
        MenuAction.delete(sv))
  }

  sealed trait MenuAction

  object MenuAction {
    sealed trait Unsaved extends MenuAction
    sealed trait Saved   extends MenuAction

    final case class SaveAsNew  (cmd: String => String \/ SavedViewCmd.Create)                            extends Unsaved
    final case class Rename     (name: Name, cmd: String => PotentialChange[String, SavedViewCmd.Update]) extends Saved
    final case class Replace    (name: Name, cmd: SavedViewCmd.Update)                                    extends Unsaved
    final case class Delete     (name: Name, cmd: SavedViewCmd.Delete)                                    extends Saved
    final case class MakeDefault(cmd: SavedViewCmd.MakeDefault)                                           extends Saved

    def saveAsNew(validate: String => String \/ Name, view: View): SaveAsNew =
      SaveAsNew(validate(_).map(SavedViewCmd.Create(_, view)))

    def makeDefault(id: Id): MakeDefault =
      MakeDefault(SavedViewCmd.MakeDefault(id))

    def delete(sv: SavedView): Delete =
      Delete(sv.name, SavedViewCmd.Delete(sv.id))

    def rename(validate: String => String \/ Name, sv: SavedView): Rename =
      Rename(sv.name, i =>
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

  def menu(savedViews: SavedViews.Optional,
           state     : State,
           activeView: View): Menu = {

    def validateName(id: Option[Id], name: String): String \/ Name =
      Name.validator(Name.State(id, savedViews))
        .unnamed
        .apply(name)
        .leftMap(Simple.Invalidity.toText)

    def saveAsNew: MenuAction.SaveAsNew =
      MenuAction.saveAsNew(validateName(None, _), activeView)

    savedViews match {
      case None =>
        Menu.NoSaved(MenuItem.Unsaved(saveAsNew, replace = None))

      case Some(svs) =>

        val default = {
          val sv = svs.default
          MenuItem.default(validateName(Some(sv.id), _), sv)
        }

        val nonDefaults = svs.nonDefault
          .valuesIterator
          .map(sv => MenuItem.nonDefault(validateName(Some(sv.id), _), sv))
          .toVector

        state.referenceViewId.flatMap(svs.get) match {
          case None =>
            state.manualView match {
              case None =>
                Menu.SavedClean(default, nonDefaults, svs.default.id)
              case Some(_) =>
                val dirtyItem = MenuItem.Unsaved(saveAsNew, None)
                Menu.SavedDirty(default, nonDefaults, dirtyItem)
            }
          case Some(ref) =>
            diff(ref, activeView) match {
              case None =>
                Menu.SavedClean(default, nonDefaults, ref.id)
              case Some(changes) =>
                val replace   = MenuAction.Replace(ref.name, SavedViewCmd.Update(ref.id, changes))
                val dirtyItem = MenuItem.Unsaved(saveAsNew, Some(replace))
                Menu.SavedDirty(default, nonDefaults, dirtyItem)
            }
        }
    }
  }

}
