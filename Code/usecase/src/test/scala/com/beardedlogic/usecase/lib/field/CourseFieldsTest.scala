package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.mockito.Mockito._
import model._
import test._
import test.NodeUtils._
import TestHelpers._
import TypeTags._
import net.liftweb.http.CometActor
import tree.TreeOps._
import StepLabels._
import util._

class CourseFieldsTest extends FunSpec with TestHelpers {

  // Covered in UseCaseTest: presave() with data refs
  // Covered in UseCaseTest: save() with data refs
  // Covered in UseCaseTest: compare() with new steps

  implicit def reactor = NoReaction
  implicit def CachedFunctionDelegation[R](c : CachedFunctionLike[R]): R = c.get
  implicit def autoTagLocalStepIds(s: String) = s.asLocalId
  implicit def autoTagNormalisedRefs(s: String) = s.hasNormalisedRefs
  implicit def autoTypeStepValues(m: Map[String, PlainValue[DataType.Step]]) = m.asInstanceOf[Map[LocalIdStr, PlainValue[DataType.Step]]]
  def SVMap(pairs: (String,PlainValue[DataType.Step])*) = autoTypeStepValues(Map(pairs:_*))

  val Key_NC = new FieldKey(1, FieldKeyType.NormalAndAlternateCourses, None)
  val Key_EC = new FieldKey(2, FieldKeyType.ExceptionCourses, None)

  val T1 = StepState("X2", "T1", List(StepState("X3", "T2", Nil), StepState("X4", "T3", Nil)))
  val T4 = StepState("X5", "T4", List(StepState("X6", "T5", Nil), StepState("X7", "T6", Nil)))
  val Tree1 = StepStateTree(StepState("X1", "Root", List(T1, T4)) :: StepState("X8", "Other", Nil) :: Nil)
  val Tree2 = StepStateTree(StepState("X1", "Root [D.800]", List(T1, T4)) :: StepState("X8", "Other", Nil) :: Nil)

  val Tree1Text = """
        1.0. Root
          1. T1
            a. T2
            b. T3
          2. T4
            a. T5
            b. T6
        1.1. Other """
  lazy val NodeTree1 = parseStepTree(Tree1Text)

  def assertCourseChangeRejected(cf: CourseFields)(test: Reactor => Boolean) {
    val c = cf.courses
    val t = cf.test__textFields
    val js = JavaScriptReaction { r => test(r) should be(false) }.toJsCmd
    cf.courses should be theSameInstanceAs(c)
    cf.test__textFields should be theSameInstanceAs(t)
    js should include("alert")
  }

  describe("Adding steps") {
    def testMaxSteps(cf: CourseFields, topStartsAtZero: Boolean) {
      def test(maxSteps: Int, lvl: Int, tree: => List[StepNode])(addStep: Reactor => Boolean) {
        maxSteps times { addStep(NoReaction) }
        tree.size should be(maxSteps)
        tree.last.labelIndex should be(MaxStepsPerLevel)

        assertCourseChangeRejected(cf)(addStep(_))
        tree.size should be(maxSteps)
      }

      // Top level
      val topMax = MaxStepsPerLevel + (if (topStartsAtZero) 1 else 0)
      test(topMax, 1, cf.courses)(cf.addTailStep(_))
      // Level 2
      test(MaxStepsPerLevel, 2, cf.courses(0).children)(cf.addStep(cf.courses(0).id)(_).isDefined)
      // Level 3
      val t3 = cf.courses(0)(-1)
      cf.increaseIndent(t3.id)
      test(MaxStepsPerLevel, 3, cf.courses(0)(-1).children)(cf.addStep(t3.id)(_).isDefined)
    }

    it("should not add when it reaches the max steps per level (NC/AC)") {
      testMaxSteps(new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC), true)
    }
    it("should not add when it reaches the max steps per level (EC)") {
      testMaxSteps(new ExceptionCourseFields(mockUseCaseCtx, Key_EC), false)
    }
  }

  describe("Removing steps") {
    it("should remove step from courses") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.removeStep(cf.courses(0)(0).id)
      cf.courses.nodes should have size(1)
    }
    it("should remove step from text-field map") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.init
      cf.test__textFields should have size(2)
      cf.removeStep(cf.courses(0)(0).id)
      cf.test__textFields should have size(1)
    }
  }

  describe("Indenting steps") {
    def ecWithThreeFullLevels = {
      val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
      MaxStepsPerLevel.times(cf.addTailStep) // Creates (1-99)
      MaxStepsPerLevel.times(cf.addStep(cf.courses.last.id)) // Creates 99.(1-99)
      cf.increaseIndent(cf.courses.last(-1).id) // Creates 99.98.a
      cf.addStep(cf.courses.last.id) // Creates 99.99
      MaxStepsPerLevel.times(cf.addStep(cf.courses.last(-1)(-1).id)) // Creates 99.98.(a-xx)
      cf.test__textFields.size should be(MaxStepsPerLevel * 3)
      cf
    }

    def ecWithDeepestLevel = {
      val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
      var last: StepNode = null
      (MaxStepDepth + 1) times {
        cf.addTailStep
        last = cf.courses.last
        MaxStepDepth times { cf.increaseIndent(last.id) }
      }
      (cf,last)
    }

    it("should decrease a step when it causes a breach of max steps per level") {
      val cf = ecWithThreeFullLevels
      assertCourseChangeRejected(cf)(cf.decreaseIndent(cf.courses.last(0).id)(_)) // L0 <-- L1
      assertCourseChangeRejected(cf)(cf.decreaseIndent(cf.courses.last(-1)(0).id)(_)) // L1 <-- L2
    }

    it("should increase a step when it causes a breach of max steps per level") {
      val cf = ecWithThreeFullLevels
      cf.removeStep(cf.courses(3).id)
      cf.addTailStep
      assertCourseChangeRejected(cf)(cf.increaseIndent(cf.courses.last.id)(_)) // L0 --> L1
    }

    it("should not increase the deepest level allowed") {
      val (cf, last) = ecWithDeepestLevel
      val n = cf.addStep(last.id).get
      assertCourseChangeRejected(cf)(cf.increaseIndent(n.id)(_))
    }

    it("should increase a step which has children at the lowest level") {
      val (cf, _) = ecWithDeepestLevel
      val n = cf.addStep(cf.courses(0).id).get // add 1.0.1
      cf.decreaseIndent(n.id) // dec 1.0.1 into 1.1
      assertCourseChangeRejected(cf)(cf.increaseIndent(n.id)(_)) // inc 1.1 with its new children
    }
  }

  describe("CourseFieldState") {
    it("should build a step map") {
      val x = CourseFieldState(Tree1)
      x.stepMap.size should be(8)
      x.stepMap("X2") should be(T1)
      x.stepMap("X4") should be(StepState("X4", "T3", Nil))
      x.stepMap("X1") should be(Tree1.head)
    }
  }

  describe("Building nodes from state") {
    it("should build a matching tree (NC/AC)") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.setState(CourseFieldState(Tree1))()
      cf.coursesWithText should matchTree(NodeTree1)
      cf.stepLabelMap("X1") should be("1.0")
      cf.stepLabelMap("X2") should be("1.0.1")
      cf.stepLabelMap("X7") should be("1.0.2.b")
    }

    it("should build a matching tree (EC)") {
      val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
      cf.setState(CourseFieldState(Tree1))()
      cf.coursesWithText should matchTree(parseStepTree( """
        1.E.1. Root
          1. T1
            a. T2
            b. T3
          2. T4
            a. T5
            b. T6
        1.E.2. Other """))
      cf.stepLabelMap("X1") should be("1.E.1")
      cf.stepLabelMap("X2") should be("1.E.1.1")
      cf.stepLabelMap("X7") should be("1.E.1.2.b")
    }

    it("should sync text fields (without refs)") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.setState(CourseFieldState(Tree1))()
      val tf = cf.test__textFields
      tf.keySet should be(Set("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))
      tf("X2").text should be("T1")
    }

    it("should sync text fields and realise normalised refs") {
      val ucCtx = mockUseCaseCtx
      val cf = new NormalAndAlternateCourseFields(ucCtx, Key_NC)
      val fn = cf.setState(CourseFieldState(Tree2))
      ucCtx.savedSteps << BiMap(800.tag[StepDataId] -> "X8".asLocalId)
      ucCtx.stepLabelMap << BiMap(cf.stepLabelMap.get)
      fn()
      val tf = cf.test__textFields
      tf.keySet should be(Set("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))
      tf("X1").text should be("Root [1.1]")
    }
  }

  def lastSaveFor(state: CourseFieldState) = {
    val (stepValues, _) = lastSave2For(state)
    val saveCtx = new FieldSaveCtx(null, stepValues)
    Some(saveCtx, state)
  }

  def lastSave2For(state: CourseFieldState) = {
    val oldStepValuesB = Map.newBuilder[LocalIdStr, PlainValue[DataType.Step]]
    val mockStepValuesByNameB = Map.newBuilder[String @@ NormalisedRefs, PlainValue[DataType.Step]]
    var i = 0
    //val savedSteps = new BiMapBuilder[Long_StepDataId, LocalIdStr]
    state.courses.foreachRecursive { ss =>
      i += 1
      val dataId = (i * 1000).tag[StepDataId]
      val sv = PlainValue[DataType.Step](i, dataId, 1)
      oldStepValuesB += (ss.id -> sv)
      mockStepValuesByNameB += (ss.text -> sv)
      //savedSteps += (dataId -> ss.id)
    }
    (oldStepValuesB.result, mockStepValuesByNameB.result)
  }

  describe("Comparison") {
    it("do nothing with no changes") {
      val oldState = CourseFieldState(Tree1)
      val oldStepValues = oldState.courses.mapRecursive { ss =>
        val id = ss.id.replace("X", "").toLong
        (ss.id -> PlainValue[DataType.Step](id, id * 1000, 1))
      }.toMap
      val saveCtx = new MutableFieldSaveCtx
      val dao = mock[DAO]

      val changedDetected = CourseFields.compareAndSaveChanges(oldState, oldStepValues, Tree1, saveCtx, dao)

      changedDetected should be(false)
      saveCtx.stepValues.result should be('empty)
      verifyZeroInteractions(dao)
    }

    def testUpdate(
      treeBefore: String,
      treeAfter: String,
      expectedUpdates: Seq[String],
      useTextAsId: Boolean
      ) {
      val before = buildStateForTest(parseStepTree(treeBefore, useTextAsId))
      val after = buildStateForTest(parseStepTree(treeAfter, useTextAsId))
      val oldState = CourseFieldState(before)
      val (oldStepValues, mockStepValuesByName) = lastSave2For(oldState)
      val saveCtx = new MutableFieldSaveCtx
      val dao = mock[DAO]
      when(dao.createValue(any[PlainValue[DataType.Step]], any[Revision])).thenReturn(mock[PlainValue[DataType.Step]])

      val changedDetected = CourseFields.compareAndSaveChanges(oldState, oldStepValues, after, saveCtx, dao)

      val stepValues = saveCtx.stepValues.result
      stepValues.size should be(expectedUpdates.size)
      for (name <- expectedUpdates) verify(dao).createValue(mockStepValuesByName(name), LatestRev)
      changedDetected should be(true)
      verifyNoMoreInteractions(dao)
    }

    it("should reuse matching and update changed (top-level, no children)") {
      testUpdate(
        "1.0. Root\n1.1. TII\n1.2. End",
        "1.0. xxxx\n1.1. TII\n1.2. xxx",
        List("Root", "End"), false)
    }
    it("should reuse matching and update changed (top-level, with children)") {
      testUpdate(
        "1.0. Root\n  1. TII\n1.2. End",
        "1.0. xxxx\n  1. TII\n1.2. End",
        List("Root"), false)
    }
    it("should reuse matching and update changed (2nd-level only, no children)") {
      testUpdate(
        "1.0. Root\n  1. TII\n1.2. End",
        "1.0. Root\n  1. xxx\n1.2. End",
        List("Root", "TII"), false)
    }
    it("should reuse matching and update changed (2nd-level only, with children)") {
      testUpdate(
        "1.0. Root\n  1. TII\n    a. Omg\n1.2. End",
        "1.0. Root\n  1. xxx\n    a. Omg\n1.2. End",
        List("Root", "TII"), false)
    }
    it("should reuse matching and update changed (Text change @ L3)") {
      testUpdate(Tree1Text, """
        1.0. Root
          1. T1
            a. T2000
            b. T3
          2. T4
            a. T5
            b. T6
        1.1. Other """,
        List("Root", "T1", "T2"), false)
    }
    it("should reuse matching and update changed (Text change @ L1)") {
      testUpdate(Tree1Text, """
              1.0. RUT
                1. T1
                  a. T2
                  b. T3
                2. T4
                  a. T5
                  b. T6
              1.1. Other """,
        List("Root"), false)
    }
    it("should reuse matching and update changed (Reorder @ L3)") {
      testUpdate(Tree1Text, """
              1.0. Root
                1. T1
                  a. T3
                  b. T2
                2. T4
                  a. T5
                  b. T6
              1.1. Other """,
        List("Root", "T1"), true)
    }
    it("should reuse matching and update changed (Reorder @ L1)") {
      testUpdate(Tree1Text, """
              1.0. Other
              1.1. Root
                1. T1
                  a. T2
                  b. T3
                2. T4
                  a. T5
                  b. T6 """,
        List(), true) // Still expect changedDetected = true
    }
  }

  describe("Saving") {
    describe("save_?()") {
      it("should not save when no courses") {
        val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
        cf.save_? should be(false)
        cf.setState(CourseFieldState(StepStateTree(Nil)))()
        cf.save_? should be(false)
      }
      it("should save when has courses") {
        val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
        cf.setState(CourseFieldState(Tree1))()
        cf.save_? should be(true)
      }
    }

    def sampleCF(courses: List[StepNodeWithText]) = {
      val cf = new ExceptionCourseFields(new UseCaseCtx(mock[CometActor]), Key_EC)
      cf.setCoursesWithTextAndInit(courses)
      cf.state.refresh
      cf
    }

    describe("presave() when no old state") {
      def mockSaveCtxAndDao = {
        val saveCtx = new MutableFieldSaveCtx
        val dao = mock[DAO]
        when(dao.createInitialValue(DataType.Step)).thenReturn(mock[PlainValue[DataType.Step]])
        (saveCtx, dao)
      }

      it("should create 1 Step Value for each node and populate saveCtx.stepValues") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val cf = sampleCF(NodeTree1)
        cf.presave(None, saveCtx, dao) should be(true)
        verify(dao, times(8)).createInitialValue(DataType.Step)
        verifyNoMoreInteractions(dao)
        saveCtx.stepValues.result.size should be(8)
      }

      it("should NOP do return false when no differences") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val cf = sampleCF(NodeTree1)
        cf.presave(lastSaveFor(cf.state.get), saveCtx, dao) should be(false)
        verifyZeroInteractions(dao)
        saveCtx.stepValues.result.size should be(0)
      }

      it("should save delta and return true when changed since last save") {
        val (saveCtx, dao) = mockSaveCtxAndDao
        val after = parseStepTree("1.0. XXX\n  1. Same Child\n1.1. Other")
        val cf = sampleCF(after)
        val before = cf.buildStateForTest(parseStepTree("1.0. Root\n  1. Same Child\n1.1. Other"))
        cf.presave(lastSaveFor(CourseFieldState(before)), saveCtx, dao) should be(true)
        verify(dao, times(1)).createValue(any[PlainValue[DataType.Step]], any[Revision])
        verifyNoMoreInteractions(dao)
        saveCtx.stepValues.result.size should be(1)
      }
    }

    // TODO check compare() with deleted steps

    describe("save()") {
      it("should create step & relation rows") {
        val cf = sampleCF(NodeTree1)
        val (stepValues, mockStepValuesByName) = lastSave2For(cf.state.get)
        val fieldValues = Map(cf.fieldKey -> mock[PlainValue[DataType.FieldValue]])
        val saveCtx = FieldSaveCtx(fieldValues, stepValues)
        val dao = mock[DAO]

        val (fd,state) = cf.save(saveCtx, saveCtx, dao)

        fd should be (None)
        state should be(cf.state.get)
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
        val cf = sampleCF(after)
        val before = cf.buildStateForTest(parseStepTree(treeBefore, true))
        val (oldStepValues, _) = lastSave2For(CourseFieldState(before))
        val oldSaveCtx = FieldSaveCtx(Map.empty, oldStepValues)

        val newFieldValues = Map(cf.fieldKey -> mock[PlainValue[DataType.FieldValue]])
        val newStepValues = SVMap(
          "New!!" -> new PlainValue[DataType.Step](90,90,1),
          "Root" -> new PlainValue[DataType.Step](91,92,2),
          "T1" -> new PlainValue[DataType.Step](91,92,2)
        )
        val saveCtx = FieldSaveCtx(newFieldValues, newStepValues)
        val dao = mock[DAO]

        val (fd,state) = cf.save(saveCtx.combineWith(oldSaveCtx), saveCtx, dao)

        fd should be (None)
        state should be(cf.state.get)
        for ((id, v) <- newStepValues) verify(dao).createStep(v, if (id=="Root") "RootX" else if (id=="T1") "T1000" else id)
        verify(dao, times(3 + 2 + 2)) // FV->[Other,New,RootX] + RootX->[T1000,T4] + T1000->[T2,T3]
          .relate_stepParent_has_step(any[Value[_ <: StepParent]], any[Short], any[Value[DataType.Step]])
        verifyNoMoreInteractions(dao)
      }
    }
  }

  /*
  describe("Loading") {
    val Value_NC = new FieldValue(10, Key_NC.valueId, None)
    val Value_EC = new FieldValue(20, Key_EC.valueId, None)
    val FieldValueMap = Map(1L -> Value_NC, 2L -> Value_EC)
    val StepValueMap = Map(100L -> "Root NC", 201L -> "EC 1E1", 202L -> "EC 1E2", 211L -> "EC 1E11")
    val Relations = Map((RelationType.Has: RelationType) -> Map(
      10L -> List(100L)
      , 20L -> List(201L, 202L)
      , 201L -> List(211L)
    ))
    val LoadCtx = new FieldLoadCtx(FieldValueMap, Relations, StepValueMap)

    def NodeId(id: Long) = "v" + id.toString

    it("should clear courses when no field value exists") {
      val ec = new ExceptionCourseFields(mockUCES, Key_EC)
      ec.courses = StepNode("id", 0, 0, NewStep, Nil) :: Nil
      ec.load(new FieldLoadCtx(Map(1L -> Value_NC), Relations, StepValueMap))
      ec.courses should be('empty)
    }

    it("should change its value to the loaded field value") {
      val ec = new ExceptionCourseFields(mockUCES, Key_EC)
      ec.load(LoadCtx)
      val expected =
        StepNode(NodeId(201), 0, 1, Step("EC 1E1"), List(new StepNode(NodeId(211), 1, 1, Step("EC 1E11")))) ::
          StepNode(NodeId(202), 0, 2, Step("EC 1E2"), Nil) ::
          Nil
      ec.courses should matchTree(expected)
      ec.courses should be(expected)
    }
  }
  */
}