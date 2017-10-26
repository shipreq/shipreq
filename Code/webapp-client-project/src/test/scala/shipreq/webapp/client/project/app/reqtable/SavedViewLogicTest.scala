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
  val `SVs-A` = SavedViews(SVb) + SVc
  val `SVs-B` = SavedViews(SVa) + SVc
  val `SVs-C` = SavedViews(SVb) + SVa

  val Dirty1 = SVa.view.copy(filter = None)
  val Dirty2 = SVa.view.copy(filter = Some(Filter.Valid.text("WOAH!!")))

  implicit def autoSome[A](a: A): Option[A] =
    Some(a)

  implicit def savedViewToFilterDead(v: SavedView): FilterDead =
    v.view.filterDead

  implicit def savedViewToSomeId(v: SavedView): Option[SavedView.Id] =
    Some(v.id)

  implicit final class SavedViewTestExt(private val self: SavedView) extends AnyVal {
    def asDefault: Menu.Item.Default =
      Menu.Item.Default(self.id, self.name.value)

    def asNonDefault: Menu.Item.NonDefault =
      Menu.Item.NonDefault(self.id, self.name.value)
  }

  override def tests = TestSuite {

    'actions {
      import Action._

      def test(svs: SavedViews.Optional, state: State)
              (a: Action, svs2: SavedViews.Optional = null)
              (expectedState: State, expectedView: View) = {
        val s2 = Action.interpret(svs)(a)(state)
        assertEq("Next State", s2, expectedState)
        assertEq("Next View", s2.activeView(Option(svs2) getOrElse svs, HideDead), expectedView)
      }

      'modify {
        'e_u - test(None, State(None, None))(Modify(Dirty1))(State(Some(Dirty1), None), Dirty1)
        'sc_sd - test(SVs, State(None, SVa))(Modify(Dirty1))(State(Some(Dirty1), SVa), Dirty1)
        'sd_sd - test(SVs, State(Dirty1, SVa))(Modify(Dirty2))(State(Some(Dirty2), SVa), Dirty2)
        'sd_sc - test(SVs, State(Dirty1, SVa))(Modify(SVa.view))(State(Some(SVa.view), SVa), SVa.view)
      }

      'select {
        'sc_sc - test(SVs, State(None, None))(Select(SVa.id))(State(None, SVa), SVa.view)
        'sc_sc - test(SVs, State(None, SVb))(Select(SVa.id))(State(None, SVa), SVa.view)
        'sd_sc - test(SVs, State(Dirty1, SVb))(Select(SVa.id))(State(None, SVa), SVa.view)
      }

      'delete {
        // Three letters are:
        // 1. [_m]  - Manual view
        // 2. [_ab] - Ref view id
        // 3. [ab]  - Delete
        '__b - test(SVs, State(None  , None))(Delete(SVb.id), `SVs-B`)(State(SVb.view, None), SVb.view)
        '__a - test(SVs, State(None  , None))(Delete(SVa.id), `SVs-A`)(State(None    , None), SVb.view)
        '_bb - test(SVs, State(None  , SVb ))(Delete(SVb.id), `SVs-B`)(State(SVb.view, None), SVb.view)
        '_ba - test(SVs, State(None  , SVb ))(Delete(SVa.id), `SVs-A`)(State(None    , SVb ), SVb.view)
        '_ab - test(SVs, State(None  , SVa ))(Delete(SVb.id), `SVs-B`)(State(None    , SVa ), SVa.view)
        '_aa - test(SVs, State(None  , SVa ))(Delete(SVa.id), `SVs-A`)(State(SVa.view, None), SVa.view)
        'm_b - test(SVs, State(Dirty1, None))(Delete(SVb.id), `SVs-B`)(State(Dirty1  , None), Dirty1  )
        'm_a - test(SVs, State(Dirty1, None))(Delete(SVa.id), `SVs-A`)(State(Dirty1  , None), Dirty1  )
        'mbb - test(SVs, State(Dirty1, SVb ))(Delete(SVb.id), `SVs-B`)(State(Dirty1  , None), Dirty1  )
        'mba - test(SVs, State(Dirty1, SVb ))(Delete(SVa.id), `SVs-A`)(State(Dirty1  , SVb ), Dirty1  )
        'mab - test(SVs, State(Dirty1, SVa ))(Delete(SVb.id), `SVs-B`)(State(Dirty1  , SVa ), Dirty1  )
        'maa - test(SVs, State(Dirty1, SVa ))(Delete(SVa.id), `SVs-A`)(State(Dirty1  , None), Dirty1  )
      }
    }

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
