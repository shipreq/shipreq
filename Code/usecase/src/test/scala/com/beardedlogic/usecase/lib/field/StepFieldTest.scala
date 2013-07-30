package com.beardedlogic.usecase
package lib.field

import org.scalatest.FunSpec
import org.mockito.Mockito._
import model._
import lib.Types._
import lib.UseCaseFns._
import lib.text.StepText
import test.NodeUtils._
import test.TestHelpers

// Covered in UseCaseTest: presave() with data refs
// Covered in UseCaseTest: save() with data refs
// Covered in UseCaseTest: compare() with new steps

class StepFieldTest extends FunSpec with TestHelpers {
  type V = StepFieldValue
  type S = NormalisedStepTree

  implicit def autoTagLocalStepIds(s: String) = s.asLocalId
  implicit def autoTagNormalisedRefs(s: String) = s.hasNormalisedRefs
  implicit def autoTypeStepValues(m: Map[String, PlainValue[DataType.Step]]) = m.asInstanceOf[Map[LocalIdStr, PlainValue[DataType.Step]]]
  def SVMap(pairs: (String, PlainValue[DataType.Step])*) = autoTypeStepValues(Map(pairs: _*))

  val EC2 = ExceptionCourseField(FieldKeyRec(662, ExceptionCourseFieldDefinition.fieldKeyType, ExceptionCourseFieldDefinition.fieldKeyData))

  val UCH = UseCaseHeader("Hello", 9)

  val NS1 = NormalisedStep(X2, "T1", List(NormalisedStep(X3, "T2", Nil), NormalisedStep(X4, "T3", Nil)))
  val NS4 = NormalisedStep(X5, "T4", List(NormalisedStep(X6, "T5", Nil), NormalisedStep(X7, "T6", Nil)))
  val Tree1State = NormalisedStepTree(NormalisedStep(X1, "Root", List(NS1, NS4)) :: NormalisedStep(X8, "Other", Nil) :: Nil)
  val Tree1bState = NormalisedStepTree(NormalisedStep(X1, "", List(NS1, NS4)) :: NormalisedStep(X8, "Other", Nil) :: Nil)
  val Tree2State = NormalisedStepTree(NormalisedStep(X1, "Root [D.143]", List(NS1, NS4)) :: NormalisedStep(X8, "Other", Nil) :: Nil)

  val Tree1Text = """
        9.0. Root
          1. T1
            a. T2
            b. T3
          2. T4
            a. T5
            b. T6
        9.1. Other """
  lazy val Tree1TextTree = parseStepTree(Tree1Text)
  lazy val Tree1 = Tree1TextTree.toStepTree
  lazy val Tree1FieldValue = Tree1TextTree.toStepFieldValue(ECF)
  // local-ids differ from Tree1State
  lazy val Tree1FieldValueState = ECF.valueSaver(Tree1FieldValue).normalisedState(EmptySavedSteps)

  def lastSaveFor(state: NormalisedStepTree) = {
    val (stepValues, _) = lastSave2For(state)
    val saveCtx = new FieldSaveCtx(null, stepValues)
    Some(saveCtx, state)
  }

  def lastSave2For(state: NormalisedStepTree) = {
    val oldStepValuesB = Map.newBuilder[LocalIdStr, PlainValue[DataType.Step]]
    val mockStepValuesByNameB = Map.newBuilder[TextWithNormalisedRefs, PlainValue[DataType.Step]]
    var i = 0
    //val savedSteps = new BiMapBuilder[Long_StepDataId, LocalIdStr]
    state.foreachRecursive(ss => {
      i += 1
      val dataId = (i * 1000).tag[StepDataId]
      val sv = PlainValue[DataType.Step](i, dataId, 1)
      oldStepValuesB += (ss.id -> sv)
      mockStepValuesByNameB += (ss.text -> sv)
      //savedSteps += (dataId -> ss.id)
    })
    (oldStepValuesB.result, mockStepValuesByNameB.result)
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Field.apply()") {
    it("should lookup the field value and cast result") {
      val tf1 = freeText("1")
      val tf2 = freeText("2")
      val m: FieldValues = Map(TF2 ~> tf1, TF3 ~> tf2, NCF ~> NCF.empty)
      val r2: StepFieldValue = NCF(m)
      r2 should be(NCF.empty)
    }
  }

  describe("NormalisedStepTree") {
    it("should build a step map") {
      val x = Tree1State
      x.stepMap.size should be(8)
      x.stepMap(X2) should be(NS1)
      x.stepMap(X4) should be(NormalisedStep(X4, "T3", Nil))
      x.stepMap(X1) should be(Tree1State.head)
    }
  }

  describe("Loading") {
    describe("denormalise()") {
      def denormalise(s: S, f: StepField, savedSteps: SavedSteps) = {
        val (stepTreeOp, fn) = f.denormalise(s, savedSteps)
        val stepLabelMap = stepTreeOp.map(generateStepAndLabelMap(f, _, UCH)).getOrElse(Map.empty)
        val stepsAndLabels = generateStepAndLabelBiMap(Seq(stepLabelMap))
        val tree = fn(stepsAndLabels)
        val txtTree = tree.toTextTree(f, savedSteps)
        (stepLabelMap, tree, txtTree)
      }

      it("should build a matching tree (NC/AC)") {
        val (stepLabelMap, _, txtTree) = denormalise(Tree1State, NCF, EmptySavedSteps)
        txtTree should matchTree(Tree1TextTree)
        stepLabelMap(X1) should be("9.0")
        stepLabelMap(X2) should be("9.0.1")
        stepLabelMap(X7) should be("9.0.2.b")
      }

      it("should build a matching tree (EC)") {
        val (stepLabelMap, _, txtTree) = denormalise(Tree1State, ECF, EmptySavedSteps)
        txtTree should matchTree(parseStepTree( """
          9.E.1. Root
            1. T1
              a. T2
              b. T3
            2. T4
              a. T5
              b. T6
          9.E.2. Other """))
        stepLabelMap(X1) should be("9.E.1")
        stepLabelMap(X2) should be("9.E.1.1")
        stepLabelMap(X7) should be("9.E.1.2.b")
      }

      it("should create a StepText for each step") {
        val (_, tree, _) = denormalise(Tree1State, NCF, EmptySavedSteps)
        tree.textmap.keySet should be(Set(X1, X2, X3, X4, X5, X6, X7, X8))
        tree.textmap(X2).text should be("T1")
      }

      it("should realise normalised refs") {
        val (_, tree, _) = denormalise(Tree2State, NCF, SavedSteps1)
        tree.textmap(X1).text should be("Root [9.0.1.a]")
      }

      it("should create empty StepTexts for blank steps") {
        val (_, tree, _) = denormalise(Tree1bState, NCF, EmptySavedSteps)
        tree.textmap(X1) should be(StepText.empty(X1))
      }
    }

    describe("load()") {
      val Value_NC = new FieldValueFullRec(10, 1, 1, NCF.rec.valueId, None)
      val Value_EC = new FieldValueFullRec(20, 2, 1, ECF.rec.valueId, None)
      val FieldValueMap = Map(NCF.rec.taggedId -> Value_NC, ECF.rec.taggedId -> Value_EC)
      val StepData = Map(100L -> "Root NC", 201L -> "EC 1E1", 202L -> "EC 1E2", 211L -> "EC 1E11")
                     .map {case (id, str) => (id.tag[StepValueId] ->(PlainValue[DataType.Step](id, id * 10, 1), str))}
      val Relations = Map((RelationType.Has: RelationType) -> Map(
        Value_NC.valueId -> List(100L),
        Value_EC.valueId -> List(201L, 202L)
        , 201L -> List(211L)
      ))
      val LoadCtx = new FieldLoadCtx(FieldValueMap, Relations, StepData)

      it("should create an empty tree when no field values exist") {
        val normalisedStepTree = EC2.load(LoadCtx, new MutableFieldSaveCtx)
        normalisedStepTree should be(NormalisedStepTree(List.empty))
      }

      it("should load a single node") {
        val normalisedStepTree = NCF.load(LoadCtx, new MutableFieldSaveCtx)
        normalisedStepTree should be(NormalisedStepTree(List(NormalisedStep("s100", "Root NC", Nil))))
      }

      it("should load a tree") {
        val normalisedStepTree = ECF.load(LoadCtx, new MutableFieldSaveCtx)
        normalisedStepTree should be(NormalisedStepTree(
          NormalisedStep("s201", "EC 1E1", List(NormalisedStep("s211", "EC 1E11", Nil)))
            :: NormalisedStep("s202", "EC 1E2", Nil)
            :: Nil
        ))
      }
    }
  }

  describe("Comparison") {
    import StepFieldValueSaver.compareAndSaveChanges

    it("should do nothing when no changes") {
      val oldStepValues = Tree1State.mapRecursive(ss => {
        val id = ss.id.replace("X", "").toLong
        (ss.id -> PlainValue[DataType.Step](id, id * 1000, 1))
      }).toMap
      val saveCtx = new MutableFieldSaveCtx
      val dao = mock[DAO]

      val changedDetected = compareAndSaveChanges(dao, Tree1State, oldStepValues, Tree1State)(saveCtx)

      changedDetected should be(false)
      saveCtx.stepValues.result should be('empty)
      verifyZeroInteractions(dao)
    }

    def testUpdate(treeBefore: String, treeAfter: String, expectedUpdates: Seq[String], useTextAsId: Boolean) {
      val before = parseStepTree(treeBefore, useTextAsId).toNState(NCF)
      val after = parseStepTree(treeAfter, useTextAsId).toNState(NCF)
      val (oldStepValues, mockStepValuesByName) = lastSave2For(before)
      val saveCtx = new MutableFieldSaveCtx
      val dao = mock[DAO]
      when(dao.createValue(any[PlainValue[DataType.Step]], any[Revision])).thenReturn(mock[PlainValue[DataType.Step]])

      val changedDetected = compareAndSaveChanges(dao, before, oldStepValues, after)(saveCtx)

      changedDetected should be(true)
      val updatedStepValues = saveCtx.stepValues.result
      updatedStepValues.size should be(expectedUpdates.size)
      for (name <- expectedUpdates) verify(dao).createValue(mockStepValuesByName(name), LatestRev)
      verifyNoMoreInteractions(dao)
    }

    it("should reuse matching and update changed (top-level, no children)") {
      testUpdate(
        "9.0. Root\n9.1. TII\n1.2. End",
        "9.0. xxxx\n9.1. TII\n1.2. xxx",
        List("Root", "End"), false)
    }

    it("should reuse matching and update changed (top-level, with children)") {
      testUpdate(
        "9.0. Root\n  1. TII\n1.2. End",
        "9.0. xxxx\n  1. TII\n1.2. End",
        List("Root"), false)
    }

    it("should reuse matching and update changed (2nd-level only, no children)") {
      testUpdate(
        "9.0. Root\n  1. TII\n1.2. End",
        "9.0. Root\n  1. xxx\n1.2. End",
        List("Root", "TII"), false)
    }

    it("should reuse matching and update changed (2nd-level only, with children)") {
      testUpdate(
        "9.0. Root\n  1. TII\n    a. Omg\n1.2. End",
        "9.0. Root\n  1. xxx\n    a. Omg\n1.2. End",
        List("Root", "TII"), false)
    }

    it("should reuse matching and update changed (Text change @ L3)") {
      testUpdate(Tree1Text, """
        9.0. Root
          1. T1
            a. T2000
            b. T3
          2. T4
            a. T5
            b. T6
        9.1. Other """,
        List("Root", "T1", "T2"), false)
    }

    it("should reuse matching and update changed (Text change @ L1)") {
      testUpdate(Tree1Text, """
              9.0. RUT
                1. T1
                  a. T2
                  b. T3
                2. T4
                  a. T5
                  b. T6
              9.1. Other """,
        List("Root"), false)
    }

    it("should reuse matching and update changed (Reorder @ L3)") {
      testUpdate(Tree1Text, """
              9.0. Root
                1. T1
                  a. T3
                  b. T2
                2. T4
                  a. T5
                  b. T6
              9.1. Other """,
        List("Root", "T1"), true)
    }

    it("should reuse matching and update changed (Reorder @ L1)") {
      testUpdate(Tree1Text, """
              9.0. Other
              9.1. Root
                1. T1
                  a. T2
                  b. T3
                2. T4
                  a. T5
                  b. T6 """,
        List(), true) // Still expect changedDetected = true
    }

    // TODO check compare() with deleted steps
  } // end describe Comparison

  describe("Saving") {
    // TODO test normalisedState

    describe("record_required_?()") {
      it("should not save when no steps") {
        ECF.valueSaver(ECF.empty).record_required_? should be(false)
      }
      it("should save when has steps") {
        ECF.valueSaver(StepFieldValue.forTree(ECF, Tree1)).record_required_? should be(true)
      }
    }

    describe("presave()") {
      def mockSaveCtxAndDao = {
        val dao = mock[DAO]
        when(dao.createInitialValue(DataType.Step)).thenReturn(mock[PlainValue[DataType.Step]])
        (new MutableFieldSaveCtx, dao)
      }

      it("should create 1 Step Value for each node and populate saveCtx.stepValues") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val s = ECF.valueSaver(Tree1FieldValue)
        s.presave(dao, None, EmptySavedSteps)(saveCtx) should be(true)
        verify(dao, times(8)).createInitialValue(DataType.Step)
        verifyNoMoreInteractions(dao)
        saveCtx.stepValues.result.size should be(8)
      }

      it("should NOP do return false when no differences") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val s = ECF.valueSaver(Tree1FieldValue)
        s.presave(dao, lastSaveFor(Tree1FieldValueState), EmptySavedSteps)(saveCtx) should be(false)
        verifyZeroInteractions(dao)
        saveCtx.stepValues.result.size should be(0)
      }

      it("should save delta and return true when changed since last save") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val s = ECF.valueSaver(parseStepTree("1.0. XXX\n  1. Same Child\n1.1. Other").toStepFieldValue(ECF))
        val prev = parseStepTree("1.0. Root\n  1. Same Child\n1.1. Other").toNState(ECF)
        s.presave(dao, lastSaveFor(prev), EmptySavedSteps)(saveCtx) should be(true)
        verify(dao, times(1)).createValue(any[PlainValue[DataType.Step]], any[Revision])
        verifyNoMoreInteractions(dao)
        saveCtx.stepValues.result.size should be(1)
      }
    }

    describe("save()") {
      it("should create step & relation rows") {
        val s = ECF.valueSaver(Tree1FieldValue)
        val (stepValues, mockStepValuesByName) = lastSave2For(Tree1FieldValueState)
        val fieldValues = Map(ECF.rec -> mock[PlainValue[DataType.FieldValue]])
        val saveCtx = FieldSaveCtx(fieldValues, stepValues)
        val dao = mock[DAO]

        val (fd, state) = s.save(dao, EmptySavedSteps, saveCtx, saveCtx)

        fd should be(None)
        state should be(Tree1FieldValueState)
        for ((name, v) <- mockStepValuesByName) verify(dao).createStep(v, name)
        verify(dao, times(8)).relate_stepParent_has_step(any[Value[_ <: StepParent]], any[Short], any[Value[DataType.Step]])
        verifyNoMoreInteractions(dao)
      }

      it("should link to reusable steps") {
        val treeBefore = Tree1Text
        val treeAfter = """
              1.0. Other
              1.1. New!!
              1.2. RootX|id=Root
                1. T1000|id=T1
                  a. T2
                  b. T3
                2. T4
                  a. T5
                  b. T6 """
        val after = parseStepTree(treeAfter, true)
        val s = ECF.valueSaver(after.toStepFieldValue(ECF))
        val before = parseStepTree(treeBefore, true).toNState(ECF)
        val (oldStepValues, _) = lastSave2For(before)
        val oldSaveCtx = FieldSaveCtx(Map.empty, oldStepValues)

        val newFieldValues = Map(ECF.rec -> mock[PlainValue[DataType.FieldValue]])
        val newStepValues = SVMap(
          "New!!" -> new PlainValue[DataType.Step](90, 90, 1),
          "Root" -> new PlainValue[DataType.Step](91, 92, 2),
          "T1" -> new PlainValue[DataType.Step](91, 92, 2)
        )
        val saveCtx = FieldSaveCtx(newFieldValues, newStepValues)
        val dao = mock[DAO]

        val (fd, state) = s.save(dao, EmptySavedSteps, saveCtx.combineWith(oldSaveCtx), saveCtx)

        fd should be(None)
        state should be(s.normalisedState(EmptySavedSteps))
        for ((id, v) <- newStepValues) verify(dao).createStep(v, if (id == "Root") "RootX" else if (id == "T1") "T1000" else id)
        verify(dao, times(3 + 2 + 2)) // FV->[Other,New,RootX] + RootX->[T1000,T4] + T1000->[T2,T3]
        .relate_stepParent_has_step(any[Value[_ <: StepParent]], any[Short], any[Value[DataType.Step]])
        verifyNoMoreInteractions(dao)
      }
    }
  }
}