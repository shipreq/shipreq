package shipreq.webapp.base.event

import japgolly.microlibs.nonempty.NonEmptyVector
import sourcecode.Line
import utest._
import shipreq.base.util.Forwards
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.text.Text

object OtherEventTest extends TestSuite {
  import ApplyEventTestFns._
  import ContentEventTestHelp.{assertBadIdsRejected, fr, at1, issueType1}
  import Event._
  import FilterAst.Attr
  import SortCriterion.SyntaxHelpers._
  import SortMethod.{Asc, AscThenBlanks, BlanksThenDesc, Desc}

  private val SVIE = InitialEvents(
    ContentEventTestHelp.createCTF1,
    ContentEventTestHelp.createFR,
    ContentEventTestHelp.createAT1,
    ContentEventTestHelp.createIssueType1)

  private val ColCF1 = Column.CustomField(ContentEventTestHelp.cf1)

  private val SV1 = SavedView(
    SavedView.Id(1),
    SavedView.Name("View1"),
    View(
      filterDead = HideDead,
      columns    = Column.mandatory.toNEV,
      order      = SortCriteria.byPubidOnly,
      filter     = None))

  private val SV2 = SavedView(
    SavedView.Id(2),
    SavedView.Name("II"),
    View(
      filterDead = ShowDead,
      columns    = Column.builtInValues.reverse :+ ColCF1,
      order      = SortCriteria(Vector(ColCF1 / BlanksThenDesc, Column.Implications(Forwards) / AscThenBlanks), Column.Pubid / Desc),
      filter     = Some {
        import Filter.Valid._
        val reqSet = NonEmptyVector(IntensionalReqSet.WholeType(StaticReqType.UseCase))
        anyOf(
          not(text("hehe")),
          presence(Attr.AnyIssue),
          reqs(reqSet),
          impliesAnyOf(reqSet),
          impliedByAnyOf(reqSet),
          tag(at1),
          issue(issueType1),
          regex("[a-z]"),
          reqType(fr),
          reqType(StaticReqType.UseCase))
      }))

  private val SV3 = SavedView(
    SavedView.Id(3),
    SavedView.Name("Three!"),
    View(
      filterDead = ShowDead,
      columns    = Column.builtInValues,
      order      = SortCriteria(Vector(Column.Title / AscThenBlanks), Column.Pubid / Asc),
      filter     = None))

  private val badColImp = Column.CustomField(CustomField.Implication.Id(5000))
  private val badColTag = Column.CustomField(CustomField.Tag.Id(5000))
  private val badColTxt = Column.CustomField(CustomField.Text.Id(5000))

  private implicit def autoSavedViewId(i: Int) = SavedView.Id(i)
  private implicit def autoSavedViewName(s: String) = SavedView.Name(s)

  private implicit def autoCreateSV(sv: SavedView): SavedViewCreate =
    SavedViewCreate(
      id         = sv.id,
      name       = sv.name,
      filterDead = sv.view.filterDead,
      columns    = sv.view.columns,
      order      = sv.view.order,
      filter     = sv.view.filter)

  private implicit final class SavedViewTestExt(private val self: SavedView) extends AnyVal {
    type Mod[A] = A => A
    def modCols(f: Mod[NonEmptyVector[Column]]): SavedView = SavedView.columns.modify(f)(self)
    def modSort(f: Mod[SortCriteria]          ): SavedView = SavedView.order.modify(f)(self)

    def vmodCols(f: Mod[NonEmptyVector[Column]]) = SavedViewGD.ValueForColumns(f(self.columns))
    def vmodSort(f: Mod[SortCriteria]          ) = SavedViewGD.ValueForOrder  (f(self.order))

    def valuesWithoutName: SavedViewGD.NonEmptyValues =
      SavedViewGD.nev(
        SavedViewGD.ValueForFilterDead(self.filterDead),
        SavedViewGD.ValueForColumns   (self.columns),
        SavedViewGD.ValueForOrder     (self.order),
        SavedViewGD.ValueForFilter    (self.filter))

    def values: SavedViewGD.NonEmptyValues =
      SavedViewGD.nev(SavedViewGD.ValueForName(self.name), valuesWithoutName.value.values.toList: _*)
  }

  private implicit def autoManualIssueId(i: Int) = ManualIssueId(i)

  private implicit def autoManualIssueText(s: String): Text.ManualIssue.NonEmptyText =
    NonEmptyVector(Text.ManualIssue.Literal(s))

  override def tests = Tests {

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    'ProjectNameSet {
      import NoInitialEvents._

      'updates {
        val p  = _assertPass(ProjectNameSet("öljy loppui"))
        assertEq(p.name, "öljy loppui")
      }
      'rejects {
        assertFail("blank")(ProjectNameSet(""))
        assertFail("preprocess")(ProjectNameSet("   as   "))
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    'SavedView {
      import SavedViewGD._
      implicit def initialEvents = SVIE

      'create {
        'valid {
          def test(evs: SavedViewCreate*)(sv: SavedViews.NonEmpty): Unit = {
            val p = _assertPass(evs: _*)
            assertEq(p.reqtableViews, Some(sv))
          }
          "1"  - test(SV1)(SavedViews(SV1))
          "2"  - test(SV2)(SavedViews(SV2))
          "12" - test(SV1, SV2)(SavedViews(SV1) + SV2)
          "21" - test(SV2, SV1)(SavedViews(SV2) + SV1)
        }
        'invalid {
          'idBad          - assertBadIdsRejected(i => SV1.copy(id = i))
          'idInUse1       - assertFail("exists") (SV1, SV2.copy(id = SV1.id))
          'idInUse2       - assertFail("exists") (SV1, SV2, SV2.copy(name = "SV3"))
          'nameEmpty      - assertFail("blank")  (SV1.copy(name = ""))
          'nameNoAZ       - assertFail("letter") (SV1.copy(name = "123"))
          'nameUniqCS     - assertFail("in use") (SV1, SV2.copy(name = SV1.name))
          'nameUniqCI     - assertFail("in use") (SV1, SV2.copy(name = SV1.name.value.toUpperCase))
          'nameReserved   - assertFail("reserve")(SV1.copy(name = "UNSAVED VIEW"))
          'colsDup        - assertFail("dup")    (SV1.modCols(_ :+ Column.Pubid))
          'colsBadCFImp   - assertFail("resolve")(SV1.modCols(_ :+ badColImp))
          'colsBadCFTag   - assertFail("resolve")(SV1.modCols(_ :+ badColTag))
          'colsBadCFTxt   - assertFail("resolve")(SV1.modCols(_ :+ badColTxt))
          'sortDup        - assertFail("dup")    (SV1.modSort(_.copy(init = Vector(Column.Title / AscThenBlanks, Column.Title / BlanksThenDesc))))
          'sortBadCFImp   - assertFail("resolve")(SV1.modSort(_.copy(init = Vector(badColImp / AscThenBlanks))))
          'sortBadCFTag   - assertFail("resolve")(SV1.modSort(_.copy(init = Vector(badColTag / AscThenBlanks))))
          'sortBadCFTxt   - assertFail("resolve")(SV1.modSort(_.copy(init = Vector(badColTxt / AscThenBlanks))))
          'sortInvis      - assertFail("visible")(SV1.modSort(_.copy(init = Vector(ColCF1 / BlanksThenDesc))))
          'filterBadRT    - assertFail("resolve")(SV2)(initialEvents.filter(_ ≠ ContentEventTestHelp.createFR), implicitly)
          'filterBadTag   - assertFail("resolve")(SV2)(initialEvents.filter(_ ≠ ContentEventTestHelp.createAT1), implicitly)
          'filterBadIssue - assertFail("resolve")(SV2)(initialEvents.filter(_ ≠ ContentEventTestHelp.createIssueType1), implicitly)

          // Note: mandatory columns may increase in future in which case they will just be tacked on the SavedViews
          // without them at runtime. Not going to bother adding a mandatory check cos it will invalidate past events in
          // the future. Not going to copy the current mandatory columns and check those cos there's nothing special about
          // them and it will complicate logic for nothing. Just allow mandatory fields to be missing and handle it.
        }
      }

      'update {
        'valid {
          def test(evs: SavedViewCreate*)(id: SavedView.Id, nev: NonEmptyValues)
                  (expect: SavedViews.NonEmpty): Unit = {
            val p = _assertPass(evs :+ SavedViewUpdate(id, nev): _*)
            assertEq(p.reqtableViews, Some(expect))
          }
          "1to2"   - test(SV1)     (SV1.id, SV2.values)                (SavedViews(SV2.copy(id = SV1.id)))
          "2to1"   - test(SV2)     (SV2.id, SV1.values)                (SavedViews(SV1.copy(id = SV2.id)))
          'name1   - test(SV1, SV2)(SV1.id, ValueForName("hehe!"))     (SavedViews(SV1.copy(name = "hehe!")) + SV2)
          'name2   - test(SV1, SV2)(SV2.id, ValueForName("hehe!"))     (SavedViews(SV1) + SV2.copy(name = "hehe!"))
          'notName - test(SV1)     (SV1.id, SV2.valuesWithoutName)     (SavedViews(SV2.copy(id = SV1.id, name = SV1.name)))
          'filter  - test(SV1)     (SV1.id, ValueForFilter(SV2.filter))(SavedViews(SavedView.filter.set(SV2.view.filter)(SV1)))
        }
        'invalid {
          implicit def initialEvents = SVIE.add(SV1)
          'notFound       - assertFail("not found")(SavedViewUpdate(SV2.id, SV2.values))
          'nameEmpty      - assertFail("blank")    (SavedViewUpdate(SV1.id, ValueForName("")))
          'nameNoAZ       - assertFail("letter")   (SavedViewUpdate(SV1.id, ValueForName("123")))
          'nameUniqCS     - assertFail("in use")   (SavedViewUpdate(SV1.id, ValueForName(SV2.name)))(initialEvents add SV2, implicitly)
          'nameUniqCI     - assertFail("in use")   (SavedViewUpdate(SV1.id, ValueForName(SV2.name.value.toUpperCase)))(initialEvents add SV2, implicitly)
          'nameReserved   - assertFail("reserved") (SavedViewUpdate(SV1.id, ValueForName("UNSAVED VIEW")))
          'colsDup        - assertFail("dup")      (SavedViewUpdate(SV1.id, SV1.vmodCols(_ :+ Column.Pubid)))
          'colsBadCFImp   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodCols(_ :+ badColImp)))
          'colsBadCFTag   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodCols(_ :+ badColTag)))
          'colsBadCFTxt   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodCols(_ :+ badColTxt)))
          'sortDup        - assertFail("dup")      (SavedViewUpdate(SV1.id, SV1.vmodSort(_.copy(init = Vector(Column.Title / AscThenBlanks, Column.Title / BlanksThenDesc)))))
          'sortBadCFImp   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodSort(_.copy(init = Vector(badColImp / AscThenBlanks)))))
          'sortBadCFTag   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodSort(_.copy(init = Vector(badColTag / AscThenBlanks)))))
          'sortBadCFTxt   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV1.vmodSort(_.copy(init = Vector(badColTxt / AscThenBlanks)))))
          'sortInvis      - assertFail("visible")  (SavedViewUpdate(SV1.id, SV1.vmodSort(_.copy(init = Vector(ColCF1 / BlanksThenDesc)))))
          'filterBadRT    - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV2.values))(initialEvents.filter(_ ≠ ContentEventTestHelp.createFR), implicitly)
          'filterBadTag   - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV2.values))(initialEvents.filter(_ ≠ ContentEventTestHelp.createAT1), implicitly)
          'filterBadIssue - assertFail("resolve")  (SavedViewUpdate(SV1.id, SV2.values))(initialEvents.filter(_ ≠ ContentEventTestHelp.createIssueType1), implicitly)
        }
      }

      'defaultSet {
        def test(evs: Event*)(id: SavedView.Id)(expect: SavedViews.NonEmpty): Unit = {
          val p = _assertPass(evs :+ SavedViewDefaultSet(id): _*)
          assertEq(p.reqtableViews, Some(expect))
        }
        "123_2"   - test(SV1, SV2, SV3)(SV2.id)(SavedViews(SV2) + SV1 + SV3)
        "123_3"   - test(SV1, SV2, SV3)(SV3.id)(SavedViews(SV3) + SV1 + SV2)
        "321_1"   - test(SV3, SV2, SV1)(SV1.id)(SavedViews(SV1) + SV2 + SV3)
        "312_2"   - test(SV3, SV1, SV2)(SV2.id)(SavedViews(SV2) + SV1 + SV3)
        'empty    - assertFail("not found")(SavedViewDefaultSet(SV1.id))
        'notFound - assertFail("not found")(SavedViewDefaultSet(SV1.id))(initialEvents.add(SV3, SV2), implicitly)
      }

      'delete {
        implicit def initialEvents = SVIE.add(SV1, SV2, SV3)
        def test(evs: Event*)(del: SavedView.Id)(expect: SavedView*): Unit = {
          val p = _assertPass(evs :+ SavedViewDelete(del): _*)
          val expect2 = NonEmptyVector.option(expect.toVector).map(x => SavedViews(x.head) ++ x.tail)
          assertEq(p.reqtableViews, expect2)
        }
        'default2    - test()(SV1.id)(SV2, SV3)
        'default3a   - test(SavedViewUpdate(SV2.id, ValueForName("Z")))(SV1.id)(SV3, SV2.copy(name = "Z"))
        'default3b   - test(SavedViewUpdate(SV3.id, ValueForName("A")))(SV1.id)(SV3.copy(name = "A"), SV2)
        'nonDefault2 - test()(SV2.id)(SV1, SV3)
        'nonDefault3 - test()(SV3.id)(SV1, SV2)
        'empty       - assertFail("not found")(SavedViewDelete(SV1.id))(SVIE, implicitly)
        'notFound    - assertFail("not found")(SavedViewDelete(SV1.id), SavedViewDelete(SV1.id))
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    'ManualIssue {

      def test(evs: Event*)(expect: (Int, String)*)(nextId: ManualIssueId)
              (implicit ie: InitialEvents, l: Line): Unit = {
        val p    = _assertPass(evs: _*)
        val imap = ManualIssue.emptyIMap ++ expect.map{ case (i, t) => ManualIssue(i, t) }
        val e    = ManualIssues(imap, nextId)
        assertEq(p.manualIssues, e)
      }

      'create {
        import NoInitialEvents._
        'one    - test(ManualIssueCreate(1, "x"))(1 -> "x")(2)
        'two    - test(ManualIssueCreate(1, "x"), ManualIssueCreate(2, "z"))(1 -> "x", 2 -> "z")(3)
        'idGap  - test(ManualIssueCreate(2, "x"), ManualIssueCreate(4, "z"))(2 -> "x", 4 -> "z")(5)
        'exists - assertFail("exists")(ManualIssueCreate(1, "x"), ManualIssueCreate(1, "x"))
      }

      'update {
        implicit def initialEvents = InitialEvents(ManualIssueCreate(1, "one"), ManualIssueCreate(2, "two"))
        'one      - test(ManualIssueUpdate(1, "x"))(1 -> "x", 2 -> "two")(3)
        'two      - test(ManualIssueUpdate(2, "a"))(1 -> "one", 2 -> "a")(3)
        'twice    - test(ManualIssueUpdate(1, "x"), ManualIssueUpdate(1, "y"))(1 -> "y", 2 -> "two")(3)
        'notFound - assertFail("not found")(ManualIssueUpdate(3, "x"))
      }

      'delete {
        implicit def initialEvents = InitialEvents(ManualIssueCreate(1, "one"), ManualIssueCreate(2, "two"))
        'one      - test(ManualIssueDelete(1))(2 -> "two")(3)
        'two      - test(ManualIssueDelete(2))(1 -> "one")(3)
        'both     - test(ManualIssueDelete(1), ManualIssueDelete(2))()(3)
        'notFound - assertFail("not found")(ManualIssueDelete(3))
      }
    }

  }
}
