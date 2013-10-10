package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.mockito.Mockito._
import scalaz.Need
import db._
import Types._
import UseCaseFns._
import text.StepText
import test.NodeUtils._
import test.{TestData, TestHelpers}
import util.{BiMap, BiMapBuilder}

class StepFieldTest extends FunSpec with TestHelpers with TestData {
  type V = StepFieldValue

  implicit def autoTagLocalStepIds(s: String) = s.asLocalStepId
  implicit def autoTagNormalisedRefs(s: String) = s.hasNormalisedRefs

  val ucId = 123L.tag[IsUseCaseIdentId]

  def valueSaver(f: StepField, sfv: StepFieldValue) = {
    val stepsAndLabels: StepAndLabelBiMap = Need(BiMap(UseCaseFns.generateStepAndLabelMap(UCN, f, sfv.tree)))
    f.valueSaver(sfv, stepsAndLabels)
  }

  import MockUc3._
  val T1 = 401L.tag[IsTextIdentId]
  val T2 = 402L.tag[IsTextIdentId]
  val T3 = 403L.tag[IsTextIdentId]
  val T4 = 404L.tag[IsTextIdentId]
  val T5 = 405L.tag[IsTextIdentId]
  val MockSavedSteps: SavedSteps = {
    val b = new BiMapBuilder[TextIdentId, LocalStepId]
    b += (T1 -> X1)
    b += (T2 -> X2)
    b += (T3 -> X3)
    b += (T4 -> X4)
    b += (T5 -> X5)
    b.result
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Field.apply()") {
    it("should lookup the field value and cast result") {
      val tf1 = freeText("1")
      val tf2 = freeText("2")
      val m: FieldValues = Map(TF2 ~> tf1, TF3 ~> tf2, NCF ~> NCF.empty)
      val r2: StepFieldValue = NCF(m)
      r2 ==== NCF.empty
    }
  }

  describe("Loading") {
    implicit def int_to_textrevid(id: Int): TextRevId = id.toLong.tag[IsTextRevId]
    implicit def int_to_textident(id: Int): TextIdentId = id.toLong.tag[IsTextIdentId]
    implicit def autoLabel(x: String): StepLabel = x.asLabel
    implicit def autoLabelO(x: Option[String]): Option[StepLabel] = x.asLabelC
    implicit def parent(rel: UcFieldTextWithFK): Option[TextRevId] = Some(rel.id)
    val N70 = UcFieldTextWithFK(NCF, UcFieldText(Some("x.0"), None, 0, TextRev(10, 1, 100, "I'm the root [D.703]")))
    val N701 = UcFieldTextWithFK(NCF, UcFieldText(Some("x.0.1"), N70, 0, TextRev(11, 1, 101, "I was inserted")))
    val N702 = UcFieldTextWithFK(NCF, UcFieldText(Some("x.0.2"), N70, 1, TextRev(12, 1, 102, "blar")))
    val N702a = UcFieldTextWithFK(NCF, UcFieldText(Some("x.0.2.a"), N702, 0, TextRev(13, 1, 103, "deeper")))
    val N703 = UcFieldTextWithFK(NCF, UcFieldText(Some("x.0.3"), N70, 2, TextRev(703, 1, 104, "last")))
    val F201 = UcFieldTextWithFK(ECF, UcFieldText(Some("x.E.1"), None, 0, TextRev(201, 1, 21, "EC 1E1")))
    val F202 = UcFieldTextWithFK(ECF, UcFieldText(Some("x.E.2"), None, 1, TextRev(202, 1, 22, "EC 1E2")))
    val F211 = UcFieldTextWithFK(ECF, UcFieldText(Some("x.E.1.1"), Some(201), 0, TextRev(211, 1, 23, "EC 1E11")))
    val F_* = List(N70, N701, N702, N702a, N703, F201, F202, F211)
    val LoadCtx = FieldLoadCtx(UCH, F_*)

    def load(f: StepField, ctx: FieldLoadCtx, uch: UseCaseHeader = UCH, ucn: UseCaseNumber = UCN) = {
      val r = f.load(ctx)
      val savedSteps: SavedSteps = BiMap.swapped(r.savedSteps)
      val stepAndLabels = r.stepTree
                          .map(t => generateStepAndLabelBiMap(generateStepAndLabelMap(ucn, f, t) :: Nil))
                          .getOrElse(EmptyStepAndLabelBiMap)
      r.phase2(savedSteps, stepAndLabels)
    }

    it("should build a field value") {
      val (sfv, _) = load(NCF, LoadCtx)
      sfv.norm ==== NcSfv.norm
    }

    it("should retain used relations as SavedData") {
      val (_, sdOpt) = load(NCF, LoadCtx)
      sdOpt.get.values.toSet ==== List(N70, N701, N702, N702a, N703).map(_.rel).toSet
    }

    it("should realise normalised refs") {
      val (sfv, _) = load(NCF, LoadCtx)
      sfv.textmap(sfv.tree(0).id).text ==== "I'm the root [7.0.3]"
    }

    // TODO test empty steps

    it("should use a default tree instead of a blank tree for NC") {
      val (sfv, _) = load(NCF, EmptyLoadCtx)
      sfv.tree.sizeRecursive should be(2)
      sfv.tree.size should be(1)
      sfv.textmap(sfv.tree.head.id).text ==== UCH.title
    }

    it("should allow a blank tree for EC") {
      val (sfv, _) = load(ECF, EmptyLoadCtx)
      sfv.tree.size should be(0)
    }
  }

  describe("Saving") {

    describe("record_required_?()") {
      it("should not save when no steps") {
        valueSaver(ECF, ECF.empty).record_required_? ==== false
      }
      it("should save when has steps") {
        valueSaver(ECF, NcSfv).record_required_? ==== true
      }
    }

    describe("presave()") {
      def mockDao = {
        val dao = mock[DaoT]
        when(dao.createTextIdent(any, any)).thenAnswer(mockCreateInitialTextAnswer(657))
        dao
      }

      it("should save a new text row for each node") {
        val dao = mockDao
        val s = valueSaver(NCF, NcSfv)
        val newlySavedSteps = s.presave(dao, ucId, None)
        verify(dao, times(5)).createTextIdent(ucId, NCF.rec.id)
        verifyNoMoreInteractions(dao)
        newlySavedSteps.keys ==== NcStepText.keys
      }

      it("should NOP when no differences") {
        val dao = mockDao
        val s = valueSaver(NCF, NcSfv)

        val newlySavedSteps = s.presave(dao, ucId, Some(MockSavedSteps))
        verifyZeroInteractions(dao)
        newlySavedSteps.size ==== 0
      }

      it("should save a new text row for new steps") {
        val dao = mockDao
        val prev = parseStepTree("1.0. Root\n  1. Same Child\n1.1. Other").toStepTree
        val newTree = "1.0. XXX\n  1. Same Child\n  2. NEW CHILD\n1.1. Other2\n1.2. NEW ROOT STEP"
        val s = valueSaver(ECF, parseStepTree(newTree).toStepFieldValue(ECF))

        val newlySavedSteps = s.presave(dao, ucId, Some(mockSavedStepsFor(prev)))
        verify(dao, times(2)).createTextIdent(ucId, ECF.rec.id)
        verifyNoMoreInteractions(dao)
        newlySavedSteps.keys ==== Set("1.0.2", "1.2")
      }
    }

    describe("save()") {
      val ucRevId = 123L.tag[IsUseCaseRevId]

      def mockDao = {
        val dao = mock[DaoT]
        when(dao.createTextRev(any, any, any)).thenAnswer(mockCreateTextRevAnswer)
        when(dao.linkUcToStep(any, any, any, any, any)).thenAnswer(mockLinkUcToStepAnswer)
        dao
      }

      def firstSavedDataOfNcf = valueSaver(NCF, NcSfv).save(mockDao, ucId, ucRevId, None)(MockSavedSteps)

      it("should create a text_rev and uc_field row for each step when 1st save") {
        val dao = mockDao
        val s = valueSaver(NCF, NcSfv)
        val savedTextRevs  = s.save(dao, ucId, ucRevId, None)(MockSavedSteps)

        savedTextRevs.size ==== 5
        verify(dao, times(5)).createTextRev(any, any, any)
        verify(dao, times(5)).linkUcToStep(any, any, any, any, any)
        verifyNoMoreInteractions(dao)
      }

      it("should link reusable steps and save new steps") {
        val sfv2 = NcSfv.copy(
          tree = StepTree(NcSfv.tree.nodes :+ StepNode(X8, 0, 1, Nil)),
          textmap = NcSfv.textmap + (X8 -> StepText(X8, freeText("AHHH"), None, None))
        )
        val T8 = 408L.tag[IsTextIdentId]
        val mockSavedSteps2: SavedSteps = BiMap(MockSavedSteps.ab + (T8 -> X8))

        val dao = mockDao
        val s = valueSaver(NCF, sfv2)
        val savedTextRevs = s.save(dao, ucId, ucRevId, Some(firstSavedDataOfNcf))(mockSavedSteps2)

        verify(dao, times(1)).createTextRev(T8, 1, "AHHH")
        verify(dao, times(6)).linkUcToStep(any, any, any, any, any)
        verifyNoMoreInteractions(dao)
        savedTextRevs.size ==== 6 // savedTextRevs should include reused too
      }

      it("should link reusable steps and updated changed steps") {
        val sfv2 = NcSfv.copy(textmap = NcSfv.textmap + (X2 -> StepText(X2, freeText("DIFF"), None, None)))
        val dao = mockDao
        val s = valueSaver(NCF, sfv2)
        s.save(dao, ucId, ucRevId, Some(firstSavedDataOfNcf))(MockSavedSteps)

        verify(dao, times(1)).createTextRev(T2, 2, "DIFF")
        verify(dao, times(5)).linkUcToStep(any, any, any, any, any)
        verifyNoMoreInteractions(dao)
      }
    }
  }
}