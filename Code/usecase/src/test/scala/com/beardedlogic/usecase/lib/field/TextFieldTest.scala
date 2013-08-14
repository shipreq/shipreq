package com.beardedlogic.usecase
package lib.field

import org.scalatest.FunSpec
import org.mockito.Mockito._
import lib.FieldLoadCtx
import lib.Types._
import lib.text.FreeText
import model._
import test.TestHelpers

class TextFieldTest extends FunSpec with TestHelpers {
  type V = FreeText

  def parseExact(txt: String)(implicit stepsAndLabels: StepAndLabelBiMap) = {
    val v = FreeText.parse(txt)
    v.text should be(txt)
    v
  }

  val TI1 = 201L.tag[TextIdentIdTag]
  val TR1 = 301L.tag[TextRevIdTag]
  val TR2 = 302L.tag[TextRevIdTag]

  describe("Field.apply()") {
    it("should lookup the field value and cast result") {
      val tf1 = freeText("1")
      val tf2 = freeText("2")
      val m: FieldValues = Map(TF2 ~> tf1, TF3 ~> tf2, NCF ~> NCF.empty)
      var r: FreeText = TF2(m)
      r should be(tf1)
      r = TF3(m)
      r should be(tf2)
    }
  }

  def ucFieldText(fkId: FieldKeyId, id: TextRevId, text: String) =
    UcFieldTextWithFK(fkId, UcFieldText(None, None, -1, TextRev((id * 10000).tag[TextIdentIdTag], 1, id, text.hasNormalisedRefs)))

  describe("Loading") {
    val V1 = ucFieldText(TF1.rec, TR1, "Jord")
    val V2 = ucFieldText(TF2.rec, TR2, "puls")
    val LoadCtx = FieldLoadCtx(List(V1, V2))
    def load(f: TextField, ctx: FieldLoadCtx) = f.load(ctx).phase2(EmptySavedSteps, EmptyStepAndLabelBiMap)
    def loadV(f: TextField, ctx: FieldLoadCtx): FreeText = load(f, ctx)._1
    def loadSD(f: TextField, ctx: FieldLoadCtx) = load(f, ctx)._2

    it("should return a blank string when there is no value") {
      loadV(TF1, EmptyLoadCtx).text ==== ""
    }

    it("should return the loaded field value when available") {
      loadV(TF1, LoadCtx).text ==== "Jord"
      loadV(TF2, LoadCtx).text ==== "puls"
    }

    it("should not return SavedData when there is no value") {
      loadSD(TF1, EmptyLoadCtx) ==== None
    }

    it("should return the TextRev as SavedData when found") {
      loadSD(TF1, LoadCtx) ==== Some(V1.textRev)
      loadSD(TF2, LoadCtx) ==== Some(V2.textRev)
    }

    it("should denormalise text with refs") {
      val V3 = ucFieldText(TF1.rec, TR1, "look at [D.143]")
      val t = TF1.load(FieldLoadCtx(List(V3))).phase2(SavedSteps1, StepState1)._1
      t should be(FreeText("look at [S.3]", Map(X3 -> S3)))
    }
  }

  describe("Saving") {
    implicit def ss = StepState1
    val ucId = 123L.tag[UseCaseIdentIdTag]

    def saver(v: V) = TF1.valueSaver(v, EmptyStepAndLabelBiMap)

    describe("record_required_?") {
      it("should not require a record when no text") {
        saver(FreeText.empty).record_required_? ==== false
      }
      it("should require a record when text is present") {
        saver(parseExact("hello")).record_required_? ==== true
      }
    }

    describe("differsFromPrevSave_?") {
      implicit def ss = SavedSteps1
      implicit def sl = StepState1

      it("should compare simple text") {
        saver(parseExact("ah")).differsFromPrevSave_?(TextRev(TI1, 1, TR1, "ah".hasNormalisedRefs)) ==== false
        saver(parseExact("ah")).differsFromPrevSave_?(TextRev(TI1, 1, TR1, "30".hasNormalisedRefs)) ==== true
      }

      it("should normalise refs before comparison") {
        val tr = TextRev(TI1, 1, TR1, "look at [D.141]".hasNormalisedRefs)
        saver(FreeText.parse("look at [S.1]")).differsFromPrevSave_?(tr) ==== false
        saver(FreeText.parse("look at [S.2]")).differsFromPrevSave_?(tr) ==== true
      }
    }

    describe("save") {
      val ucRevId = 321L.tag[UseCaseRevIdTag]

      def mockDao = {
        val dao = mock[DAO]
        when(dao.createInitialText(any, any)).thenAnswer(mockCreateInitialTextAnswer(657))
        when(dao.createTextRev(any, any, any)).thenAnswer(mockCreateTextRevAnswer)
        dao
      }

      implicit val iss = EmptySavedSteps

      it("should create a text + text_rev row for first time") {
        val dao = mockDao
        val tr = saver(parseExact("hello")).save(dao, ucId, ucRevId, None)
        tr.rev ==== 1
        tr.identId.toLong ==== 657
        tr.text.toString ==== "hello"
        verify(dao, times(1)).createInitialText(any, any)
        verify(dao, times(1)).createTextRev(any, any, any)
        verify(dao, times(1)).linkUcToText(any, any)
        verifyNoMoreInteractions(dao)
      }

      it("should update changed text") {
        val dao = mockDao
        val prev = TextRev(TI1, 2, TR1, "OLD".hasNormalisedRefs)
        val tr = saver(parseExact("hello")).save(dao, ucId, ucRevId, Some(prev))
        tr.rev ==== 3
        tr.identId ==== TI1
        tr.text.toString ==== "hello"
        verify(dao, times(1)).createTextRev(any, any, any)
        verify(dao, times(1)).linkUcToText(any, any)
        verifyNoMoreInteractions(dao)
      }

      it("should reuse unchanged text") {
        val dao = mockDao
        val prev = TextRev(TI1, 2, TR1, "hello".hasNormalisedRefs)
        val tr = saver(parseExact("hello")).save(dao, ucId, ucRevId, Some(prev))
        tr ==== prev
        verify(dao, times(1)).linkUcToText(any, any)
        verifyNoMoreInteractions(dao)
      }
    }
  }
}
