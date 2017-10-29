package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.scalaz_ext.ScalazMacros._
import scalaz.{-\/, Equal, \/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.event.SavedViewGD
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.{SavedViewCmd => Cmd}
import SavedViewLogic._
import SortCriterion.SyntaxHelpers._
import SortMethod.{Asc, AscThenBlanks, BlanksThenDesc, Desc}

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

  val `SVa->b`: SavedViewGD.NonEmptyValues =
    SavedViewGD.nev(
      SavedViewGD.ValueForColumns   (SVb.view.columns),
      SavedViewGD.ValueForOrder     (SVb.view.order),
      SavedViewGD.ValueForFilter    (SVb.view.filter),
      SavedViewGD.ValueForFilterDead(SVb.view.filterDead))

  val `SVb->a`: SavedViewGD.NonEmptyValues =
    SavedViewGD.nev(
      SavedViewGD.ValueForColumns   (SVa.view.columns),
      SavedViewGD.ValueForOrder     (SVa.view.order),
      SavedViewGD.ValueForFilter    (SVa.view.filter),
      SavedViewGD.ValueForFilterDead(SVa.view.filterDead))

  val `SVc->a`: SavedViewGD.NonEmptyValues =
    SavedViewGD.nev(
      SavedViewGD.ValueForColumns   (SVa.view.columns),
      SavedViewGD.ValueForOrder     (SVa.view.order),
      SavedViewGD.ValueForFilter    (SVa.view.filter))

  val `SVc->b`: SavedViewGD.NonEmptyValues =
    SavedViewGD.nev(
      SavedViewGD.ValueForColumns   (SVb.view.columns),
      SavedViewGD.ValueForOrder     (SVb.view.order),
      SavedViewGD.ValueForFilterDead(SVb.view.filterDead))

  val Dirty1 = SVa.view.copy(filter = None)
  val Dirty2 = SVa.view.copy(filter = Some(Filter.Valid.text("WOAH!!")))

  implicit def autoSome[A](a: A): Option[A] =
    Some(a)

  implicit def savedViewToFilterDead(v: SavedView): FilterDead =
    v.view.filterDead

  implicit def savedViewToSomeId(v: SavedView): Option[SavedView.Id] =
    Some(v.id)

  val leftBlank = -\/("Cannot be blank.")
  val leftTaken = -\/("Already in use.")

  implicit final class SavedViewTestExt(private val sv: SavedView) extends AnyVal {
    private def nv = nameValidationFn(
      sva = if (sv eq SVa) \/-(SVa.name) else leftTaken,
      svb = if (sv eq SVb) \/-(SVb.name) else leftTaken,
      svc = if (sv eq SVc) \/-(SVc.name) else leftTaken,
    )

    def asDefault: MenuItem.Default =
      MenuItem.default(nv, sv)

    def asNonDefault: MenuItem.NonDefault =
      MenuItem.nonDefault(nv, sv)
  }

  val testNames: List[SavedView.Name] =
    SVa.name ::
    SVb.name ::
    SVc.name ::
    SavedView.Name("  xxx  ") ::
    SavedView.Name("") :: Nil

  def testNameFn[A >: Null](default: SavedView.Name => A)
                           (sva  : A = null,
                            svb  : A = null,
                            svc  : A = null,
                            xxx  : A = null,
                            empty: A = null): String => A =
    i => SavedView.Name(i.trim) match {
      case s if s       ==* SVa.name => Option(sva  ).getOrElse(default(s))
      case s if s       ==* SVb.name => Option(svb  ).getOrElse(default(s))
      case s if s       ==* SVc.name => Option(svc  ).getOrElse(default(s))
      case s if s.value ==* "xxx"    => Option(xxx  ).getOrElse(default(s))
      case s if s.value ==* ""       => Option(empty).getOrElse(default(s))
    }

  def nameValidationFn(sva: String \/ SavedView.Name = null,
                       svb: String \/ SavedView.Name = null,
                       svc: String \/ SavedView.Name = null,
                       xxx: String \/ SavedView.Name = null): String => String \/ SavedView.Name =
    testNameFn[String \/ SavedView.Name](\/-(_))(sva = sva, svb = svb, svc = svc, xxx = xxx, empty = leftBlank)

  implicit def nameFnEquality[A: Equal]: Equal[String => A] =
    Equal.equal((f, g) => testNames.forall { nn =>
      val n = nn.value
      val pass = Equal[A].equal(f(n), g(n))
      if (!pass) {
        println(s"$n -- ${f(n)} ≠ ${g(n)}")
      }
      pass
    })

  implicit val equalMenuActionReplace    : Equal[MenuAction.Replace    ] = deriveEqual
  implicit val equalMenuActionDelete     : Equal[MenuAction.Delete     ] = deriveEqual
  implicit val equalMenuActionMakeDefault: Equal[MenuAction.MakeDefault] = deriveEqual
  implicit val equalMenuActionSaveAsNew  : Equal[MenuAction.SaveAsNew  ] = deriveEqual
  implicit val equalMenuActionRename     : Equal[MenuAction.Rename     ] = deriveEqual
  implicit val equalMenuActionUnsaved    : Equal[MenuAction.Unsaved    ] = deriveEqual
  implicit val equalMenuItemD            : Equal[MenuItem.Default      ] = deriveEqual
  implicit val equalMenuItemND           : Equal[MenuItem.NonDefault   ] = deriveEqual
  implicit val equalMenuItemS            : Equal[MenuItem.Saved        ] = deriveEqual
  implicit val equalMenuItemU            : Equal[MenuItem.Unsaved      ] = deriveEqual
  implicit val equalMenuU                : Equal[Menu.NoSaved          ] = deriveEqual
  implicit val equalMenuSD               : Equal[Menu.SavedDirty       ] = deriveEqual
  implicit val equalMenuSC               : Equal[Menu.SavedClean       ] = deriveEqual
  implicit val equalMenu                 : Equal[Menu                  ] = deriveEqual

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
      import Menu._

      def determine(savedViews        : SavedViews.Optional,
                    filterDeadFallback: FilterDead)
                   (manualView        : Option[View],
                    referenceView     : Option[SavedView.Id]): Menu = {
        val s = State(manualView, referenceView)
        menu(savedViews, s, s.activeView(savedViews, filterDeadFallback))
      }

      def dirtyAnon(activeView: View): MenuItem.Unsaved = {
        val nv        = nameValidationFn(sva = leftTaken, svb = leftTaken, svc = leftTaken)
        val saveAsNew = MenuAction.saveAsNew(nv, activeView)
        MenuItem.Unsaved(saveAsNew, None)
      }

      def dirty(ref: SavedView, changes: SavedViewGD.NonEmptyValues, activeView: View): MenuItem.Unsaved = {
        val nv        = nameValidationFn(sva = leftTaken, svb = leftTaken, svc = leftTaken)
        val saveAsNew = MenuAction.saveAsNew(nv, activeView)
        val replace   = MenuAction.Replace(ref.name, Cmd.Update(ref.id, changes))
        MenuItem.Unsaved(saveAsNew, Some(replace))
      }

      'noSaved {
        // no matter the current view state, the user can save it

        def saveAsNew(v: View) = MenuAction.saveAsNew(nameValidationFn(), v)

        'clean {
          val fd = HideDead
          val m = determine(None, fd)(None, None)
          assertEq(m, NoSaved(MenuItem.Unsaved(saveAsNew(View.default(fd)), None)))
        }

        'dirty {
          val m = determine(None, HideDead)(SVa.view, None)
          assertEq(m, NoSaved(MenuItem.Unsaved(saveAsNew(SVa.view), None)))
        }
      }

      'savedClean {

        'defaultUnclicked {
          val m = determine(SVs, SVb)(None, None)
          assertEq(m, SavedClean(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'defaultClicked {
          val m = determine(SVs, SVb)(None, SVb.id)
          assertEq(m, SavedClean(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), SVb.id))
        }

        'nonDefaultClickedA {
          val m = determine(SVs, SVa)(None, SVa.id)
          assertEq(m, SavedClean(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), SVa.id))
        }

        'nonDefaultClickedC {
          val m = determine(SVs, SVc)(None, SVc.id)
          assertEq(m, SavedClean(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), SVc.id))
        }
      }

      'savedDirty {
        'postDeletion {
          val m = determine(SVs, SVa)(SVa.view, None)
          assertEq(m, SavedDirty(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), dirtyAnon(SVa.view)))
        }

        'defaultClicked {
          val m = determine(SVs, ShowDead)(SVa.view, SVb.id)
          assertEq(m, SavedDirty(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), dirty(SVb, `SVb->a`, SVa.view)))
        }

        'nonDefaultClickedAB {
          val m = determine(SVs, ShowDead)(SVb.view, SVa.id)
          assertEq(m, SavedDirty(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), dirty(SVa, `SVa->b`, SVb.view)))
        }

        'nonDefaultClickedCA {
          val m = determine(SVs, ShowDead)(SVa.view, SVc.id)
          assertEq(m, SavedDirty(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), dirty(SVc, `SVc->a`, SVa.view)))
        }

        'nonDefaultClickedCB {
          val m = determine(SVs, ShowDead)(SVb.view, SVc.id)
          assertEq(m, SavedDirty(SVb.asDefault, Vector(SVa.asNonDefault, SVc.asNonDefault), dirty(SVc, `SVc->b`, SVb.view)))
        }
      }

    }
  }

}
