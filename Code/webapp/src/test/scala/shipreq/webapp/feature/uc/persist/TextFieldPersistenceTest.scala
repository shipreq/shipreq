package shipreq.webapp
package feature.uc.persist

import org.scalatest.FunSpec
import org.mockito.Mockito._
import lib.Types._
import feature.uc.UcParsingCtx
import feature.uc.field.TextField
import feature.uc.text.FreeText
import feature.uc.text.FreeTextTerms._
import db._
import test.TestHelpers

class TextFieldPersistenceTest extends FunSpec with TestHelpers {
  type V = FreeText

  def parseExact(txt: String)(implicit sl: StepAndLabelBiMap) = {
    implicit val ctx = UcParsingCtx.Empty.copy(stepsAndLabels = sl)
    val v = FreeText.parse(txt)
    v.text should be(txt)
    v
  }

  val UCH = UseCaseHeader("AH".validated)
  val EmptyLoadCtx = FieldLoadCtx(UCH, List.empty)
  val TI1 = 201L.tag[IsTextIdentId]
  val TR1 = 301L.tag[IsTextRevId]
  val TR2 = 302L.tag[IsTextRevId]

  def ucFieldText(fkId: FieldKeyId, id: TextRevId, text: String) =
    UcFieldTextWithFK(fkId, UcFieldText(None, None, -1, TextRev((id * 10000).tag[IsTextIdentId], 1, id, text.tag[IsNormalised])))

  implicit def f2fp(f: TextField) = new TextFieldPersistence(f)

  describe("Loading") {
    val V1 = ucFieldText(TF1.rec, TR1, "Jord")
    val V2 = ucFieldText(TF2.rec, TR2, "puls")
    val LoadCtx = FieldLoadCtx(UCH, List(V1, V2))
    def load(f: TextField, ctx: FieldLoadCtx) = f.load(ctx).phase2(EmptySavedSteps, UcParsingCtx.Empty)
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
      val t = TF1.load(FieldLoadCtx(UCH, List(V3))).phase2(SavedSteps1, UcParsingCtx.Empty.copy(stepsAndLabels = StepState1))._1
      t shouldBe FreeText(PlainText("look at ") :: StepRef(X3, S3) :: Nil)
    }
  }

  describe("Saving") {
    implicit def ss = StepState1
    val ucId = 123L.tag[IsUseCaseIdentId]

    def saver(v: V) = TF1.saver(v, EmptyStepAndLabelBiMap)

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
      implicit val ctx = UcParsingCtx.Empty.copy(stepsAndLabels = sl)

      it("should compare simple text") {
        saver(parseExact("ah")).differsFromPrevSave_?(TextRev(TI1, 1, TR1, "ah".tag[IsNormalised])) ==== false
        saver(parseExact("ah")).differsFromPrevSave_?(TextRev(TI1, 1, TR1, "30".tag[IsNormalised])) ==== true
      }

      it("should normalise refs before comparison") {
        val tr = TextRev(TI1, 1, TR1, "look at [D.141]".tag[IsNormalised])
        saver(FreeText.parse("look at [S.1]")).differsFromPrevSave_?(tr) ==== false
        saver(FreeText.parse("look at [S.2]")).differsFromPrevSave_?(tr) ==== true
      }
    }

    describe("save") {
      val ucRevId = 321L.tag[IsUseCaseRevId]

      def mockDao = {
        val dao = mock[DaoT]
        when(dao.createTextIdent(any, any)).thenAnswer(mockCreateInitialTextAnswer(657))
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
        verify(dao, times(1)).createTextIdent(any, any)
        verify(dao, times(1)).createTextRev(any, any, any)
        verify(dao, times(1)).linkUcToText(any, any)
        verifyNoMoreInteractions(dao)
      }

      it("should update changed text") {
        val dao = mockDao
        val prev = TextRev(TI1, 2, TR1, "OLD".tag[IsNormalised])
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
        val prev = TextRev(TI1, 2, TR1, "hello".tag[IsNormalised])
        val tr = saver(parseExact("hello")).save(dao, ucId, ucRevId, Some(prev))
        tr ==== prev
        verify(dao, times(1)).linkUcToText(any, any)
        verifyNoMoreInteractions(dao)
      }
    }
  }
}
