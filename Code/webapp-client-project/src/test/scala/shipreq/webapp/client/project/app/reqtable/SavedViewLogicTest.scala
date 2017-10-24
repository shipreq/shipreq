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
    id           = SavedView.Id(1),
    name         = SavedView.Name("B!!"),
    filterDead   = HideDead,
    columns      = Column.mandatory.toNEV,
    sortCriteria = SortCriteria.byPubidOnly,
    filter       = None)

  val SVa = SavedView(
    id           = SavedView.Id(2),
    name         = SavedView.Name("Aah"),
    filterDead   = ShowDead,
    columns      = Column.builtInValues.reverse,
    sortCriteria = SortCriteria(Vector(Column.Implications(Forwards) / AscThenBlanks), Column.Pubid / Desc),
    filter       = Some(Filter.Valid.text("ah")))

  val SVc = SavedView(
    id           = SavedView.Id(3),
    name         = SavedView.Name("CC's"),
    filterDead   = ShowDead,
    columns      = Column.builtInValues,
    sortCriteria = SortCriteria(Vector(Column.Title / AscThenBlanks), Column.Pubid / Asc),
    filter       = None)

  val SVs = SavedViews(SVb) + SVa + SVc

  implicit def savedViewsToOptional(svs: SavedViews.NonEmpty): SavedViews.Optional =
    Some(svs)

  implicit def savedViewToFilterDead(v: SavedView): FilterDead =
    v.filterDead

  implicit def savedViewToTableSettings(v: SavedView): TableSettings =
    toTableSettingsDirect(v)

  implicit final class SavedViewTestExt(private val self: SavedView) extends AnyVal {
    def asDefault: Menu.Item.Default =
      Menu.Item.Default(self.id, self.name.value)

    def asNonDefault: Menu.Item.NonDefault =
      Menu.Item.NonDefault(self.id, self.name.value)
  }

  override def tests = TestSuite {

    'menu {
      import Menu._

      'noSaved {
        // no matter the current view state, the user can save it
        val m = determine(None)(HideDead, TableSettings.default, None)
        assertEq(m, NoSaved)
      }

      'savedClean {
        'defaultUnclicked {
          val m = determine(SVs)(SVb, SVb, None)
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'defaultClicked {
          val m = determine(SVs)(SVb, SVb, Some(SVb.id))
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'nonDefaultClickedA {
          val m = determine(SVs)(SVa, SVa, Some(SVa.id))
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVa.id))
        }

        'nonDefaultClickedC {
          val m = determine(SVs)(SVc, SVc, Some(SVc.id))
          assertEq(m, SavedClean(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), SVc.id))
        }
      }

      'savedDirty {
        'defaultUnclicked {
          val m = determine(SVs)(!SVb, SVb, None)
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVb)))
        }

        'defaultClicked {
          val m = determine(SVs)(SVb, SVa, Some(SVb.id))
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVb)))
        }

        'nonDefaultClickedA {
          val m = determine(SVs)(!SVa, SVa, Some(SVa.id))
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVa)))
        }

        'nonDefaultClickedC {
          val m = determine(SVs)(SVc, SVa, Some(SVc.id))
          assertEq(m, SavedDirty(SVb.asDefault, Set(SVa.asNonDefault, SVc.asNonDefault), Item.dirty(SVc)))
        }
      }

    }
  }

}
