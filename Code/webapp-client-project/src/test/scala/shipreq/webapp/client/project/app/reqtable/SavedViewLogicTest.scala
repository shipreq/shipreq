package shipreq.webapp.client.project.app.reqtable

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.filter.Filter
import SortCriterion.SyntaxHelpers._
import SortMethod.{Asc, AscThenBlanks, BlanksThenDesc, Desc}
import SavedViewLogic._

object SavedViewLogicTest extends TestSuite {

  val SVb = SavedView(
    SavedView.Id(1),
    SavedView.Name("B!!"),
    View(
      columns    = Column.mandatory.toNEV,
      order      = SortCriteria.byPubidOnly,
      filterDead = HideDead,
      filter     = None))

  val SVa = SavedView(
    SavedView.Id(2),
    SavedView.Name("Aah"),
    View(
      columns    = Column.builtInValues.reverse,
      order      = SortCriteria(Vector(Column.Implications(Forwards) / AscThenBlanks), Column.Pubid / Desc),
      filterDead = ShowDead,
      filter     = Some(Filter.Valid.text("ah"))))

  val SVc = SavedView(
    SavedView.Id(3),
    SavedView.Name("CC's"),
    View(
      columns    = Column.builtInValues,
      order      = SortCriteria(Vector(Column.Title / AscThenBlanks), Column.Pubid / Asc),
      filterDead = ShowDead,
      filter     = None))

  val SVs = SavedViews(SVb) + SVa + SVc

  implicit def autoSome[A](a: A): Option[A] =
    Some(a)

  implicit def savedViewToFilterDead(v: SavedView): FilterDead =
    v.view.filterDead

  implicit final class SavedViewTestExt(private val self: SavedView) extends AnyVal {
    def asDefault: Menu.Item.Default =
      Menu.Item.Default(self.id, self.name.value)

    def asNonDefault: Menu.Item.NonDefault =
      Menu.Item.NonDefault(self.id, self.name.value)
  }

  override def tests = TestSuite {

    'menu {
      import Menu.{determine => _, _}

      def determine(savedViews        : SavedViews.Optional,
                    filterDeadFallback: FilterDead)
                   (manualView        : Option[View],
                    referenceView     : Option[SavedView.Id]): Menu = {
        val s = State(manualView, referenceView)
        Menu.determine(savedViews, s, s.activeView(savedViews, filterDeadFallback))
      }

      'noSaved {
        // no matter the current view state, the user can save it
        'clean {
          val m = determine(None, HideDead)(None, None)
          assertEq(m, NoSaved)
        }
        'dirty {
          val m = determine(None, HideDead)(SVa.view, None)
          assertEq(m, NoSaved)
        }
      }

      'savedClean {
        'defaultUnclicked {
          val m = determine(SVs, SVb)(None, None)
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'defaultClicked {
          val m = determine(SVs, SVb)(None, SVb.id)
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'nonDefaultClickedA {
          val m = determine(SVs, SVa)(None, SVa.id)
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVa.id))
        }

        'nonDefaultClickedC {
          val m = determine(SVs, SVc)(None, SVc.id)
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVc.id))
        }
      }

      'savedDirty {
        'postDeletion {
          val m = determine(SVs, SVa)(SVa.view, None)
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVb)))
        }

        'defaultClicked {
          val m = determine(SVs, ShowDead)(SVa.view, SVb.id)
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVb)))
        }

        'nonDefaultClickedA {
          val m = determine(SVs, ShowDead)(SVb.view, SVa.id)
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVa)))
        }

        'nonDefaultClickedC {
          val m = determine(SVs, ShowDead)(SVa.view, SVc.id)
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVc)))
        }
      }

    }
  }

}
