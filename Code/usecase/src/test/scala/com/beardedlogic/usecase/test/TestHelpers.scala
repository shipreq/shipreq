package com.beardedlogic.usecase
package test

import java.io.File
import org.apache.commons.io.FileUtils
import org.mockito.Mockito.when
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.matchers.{ShouldMatchers, Matcher, MatchResult}
import org.scalatest.mock.MockitoSugar
import org.scalatest.prop.Tables.Table
import net.liftweb.common.Empty
import net.liftweb.http.{S, LiftSession, LiftRules}
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.util.StringHelpers
import net.liftweb.util.Helpers.stringToSuper
import scalaz.{NonEmptyList, Lens}
import scala.annotation.tailrec

import lib.change._
import lib.tree._
import lib.field._
import lib.text._
import lib._
import model._
import util._

import Types._
import NodeUtils._
import TreeOps._
import Changes.ExistingStepLabelsChanged

/**
 * @since 30/04/2013
 */
trait TestHelpers extends MockitoSugar with ShouldMatchers {

  if (!LiftRules.doneBoot) (new bootstrap.liftweb.Boot).configureLift
  //if (Defaults.FieldList.get == null) Defaults.FieldList << mockFieldList(Defaults.FieldListDefns)

  type Refs = Map[LocalIdStr, LabelStr]

  def savedSteps(tuples: (Int, LocalIdStr)*): BiMap[TextIdentId, LocalIdStr] =
    BiMap.apply(tuples.map(t => (t._1.toLong.tag[TextIdentIdTag], t._2)): _*)

  def mapToLabels(i: Traversable[LocalIdStr], stepState: StepAndLabelBiMap = StepState1) = i.map(id => (id, stepState.get.ab(id))).toMap
  def mapFromLabels(i: Traversable[LocalIdStr], stepState: StepAndLabelBiMap = StepState1) = i.map(id => (stepState.get.ab(id), id)).toMap

  def mapToIds(i: Traversable[LabelStr], stepState: StepAndLabelBiMap = StepState1) = i.map(l => (l, stepState.get.ba(l))).toMap
  def mapFromIds(i: Traversable[LabelStr], stepState: StepAndLabelBiMap = StepState1) = i.map(l => (stepState.get.ba(l), l)).toMap

  def flowFromClause(refs: (LocalIdStr, LabelStr)*) = if (refs.isEmpty) None else Some(FlowFromClause(Map(refs: _*)))
  def flowToClause(refs: (LocalIdStr, LabelStr)*) = if (refs.isEmpty) None else Some(FlowToClause(Map(refs: _*)))

  def MockExistingStepLabelsChanged = {
    val m = mock[ExistingStepLabelsChanged]
    when(m.asOnlyChange).thenReturn(NonEmptyList(m))
    m
  }

  val X0 = "X0".asLocalId
  val X1 = "X1".asLocalId
  val X2 = "X2".asLocalId
  val X3 = "X3".asLocalId
  val X4 = "X4".asLocalId
  val X5 = "X5".asLocalId
  val X6 = "X6".asLocalId
  val X7 = "X7".asLocalId
  val X8 = "X8".asLocalId
  val X9 = "X9".asLocalId
  val X3E1 = "X3E1".asLocalId
  val X3E2 = "X3E2".asLocalId

  val S0 = "S.0".asLabel
  val S1 = "S.1".asLabel
  val S2 = "S.2".asLabel
  val S3 = "S.3".asLabel
  val S4 = "S.4".asLabel
  val S5 = "S.5".asLabel
  val S6 = "S.6".asLabel
  val SA = "S.A".asLabel
  val SF = "S.F".asLabel

  val SavedSteps1: SavedSteps = savedSteps(
    140 -> X0,
    141 -> X1,
    142 -> X2,
    143 -> X3,
    144 -> X4,
    145 -> X5,
    146 -> X6
  )

  val StepState1: StepAndLabelBiMap = LazyVal <~ BiMap(X1 -> S1, X2 -> S2, X3 -> S3, X5 -> S5, X6 -> S6, X0 -> S0)
  val StepState2: StepAndLabelBiMap = LazyVal <~ BiMap(X1 -> SA, X2 -> S2, X4 -> S4, X5 -> SF, X6 -> S6)

  val TextWithFlowExamples = Table[String, String, List[String], List[String]](
    ("EXAMPLE", "TEXT", "REFS-FROM", "REFS-TO")
    , ("omg --> 1.0", "omg", Nil, List("1.0"))
    , ("omg <-- 1.0", "omg", List("1.0"), Nil)
    , ("omg --> 1.0 <-- 1.2", "omg", List("1.2"), List("1.0"))
    , ("omg <-- 1.0 --> 1.2", "omg", List("1.0"), List("1.2"))
    , ("hehe sweet! --> 1.3 [1.0]", "hehe sweet!", Nil, List("1.3", "1.0"))
    , ("excellent yo --> 3.E.1,3.E.2", "excellent yo", Nil, List("3.E.1", "3.E.2"))
    , ("excellent --> yo --> 1.0", "excellent --> yo", Nil, List("1.0"))
  )

  val StepStateB: StepAndLabelBiMap = LazyVal <~ BiMap(
    X1 -> "1.0".asLabel,
    X2 -> "1.2".asLabel,
    X3 -> "1.3".asLabel,
    X3E1 -> "3.E.1".asLabel,
    X3E2 -> "3.E.2".asLabel)

  implicit def FieldToFKRec(f: Field): FieldKeyRec = f.rec
  implicit def FieldToFkId(f: Field): FieldKeyId = f.rec.id
  implicit def SfvToStepTree(sfv: StepFieldValue): StepTree = sfv.tree

  lazy val TF1 = mockTextField("Stuff #1", 111)
  lazy val TF2 = mockTextField("Stuff #2", 222)
  lazy val TF3 = mockTextField("Stuff #3", 333)
  lazy val TF4 = mockTextField("Stuff #4", 444)

  lazy val NCF = NormalCourseField(FieldKeyRec(55.tag[FieldKeyIdTag], NormalCourseFieldDefinition.fieldKeyType, NormalCourseFieldDefinition.fieldKeyData))
  lazy val ECF = ExceptionCourseField(FieldKeyRec(66.tag[FieldKeyIdTag], ExceptionCourseFieldDefinition.fieldKeyType, ExceptionCourseFieldDefinition.fieldKeyData))

  val EmptyLoadCtx = new FieldLoadCtx(List.empty)

  // -------------------------------------------------------------------------------------------------------------------

  def mockSavedStepsFor(tree: StepTree): SavedSteps = {
    val savedSteps = new BiMapBuilder[TextIdentId, LocalIdStr]
    var i = 0
    tree.foreachRecursive(s => {
      i += 1
      val textIdentId = (i * 1000).tag[TextIdentIdTag]
      savedSteps += (textIdentId -> s.id)
    })
    savedSteps.result
  }

  def mockCreateInitialTextAnswer(startingId: Long) = new Answer[TextIdentId] {
    var id = startingId - 1
    override def answer(invocation: InvocationOnMock) = {
      id += 1
      id.tag[TextIdentIdTag]
    }
  }

  def mockCreateTextRevAnswer = new Answer[TextRev] {
    override def answer(i: InvocationOnMock) = {
      val identId = i.getArguments()(0).asInstanceOf[TextIdentId]
      val rev = i.getArguments()(1).asInstanceOf[Short]
      val text = i.getArguments()(2).asInstanceOf[TextWithNormalisedRefs]
      val id = (identId*10).tag[TextRevIdTag]
      TextRev(identId, rev, id, text)
    }
  }

  def mockLinkUcToStepAnswer = new Answer[UcFieldText] {
    override def answer(i: InvocationOnMock) = {
      val uc = i.getArguments()(0).asInstanceOf[UseCaseRevId]
      val label = i.getArguments()(1).asInstanceOf[LabelStr]
      val index = i.getArguments()(2).asInstanceOf[Short]
      val parentId = i.getArguments()(3).asInstanceOf[Option[TextRevId]]
      val text = i.getArguments()(4).asInstanceOf[TextRev]
      UcFieldText(Some(label), parentId, index, text)
    }
  }

  val stepTreeLens = {
    import LensFns._
    FieldLenses.uc.stepField >@==> scalaz.Lens.lensg[StepFieldValue, StepTree](
      sfv => t => sfv.withNewTree(t),
      sfv => sfv.tree
    )
  }

  def mockTextField(title: String, id: Long) = {
    val defn = TextFieldDefinition(title)
    TextField(defn, FieldKeyRec(id.tag[FieldKeyIdTag], FieldKeyType.Text, defn.fieldKeyData))
  }

//  def mockFieldList(defs: List[FieldDefinition]): FieldListRec = {
//    val pv = PlainValue[DataType.FieldList](-666, -1, 1)
//    var i = -999
//    val keys = defs.map {
//      case d: TextFieldDef                        => i -= 1; FieldKeyRec(i, FieldKeyType.Text, Some(d.title))
//      case _: NormalAndAlternateCourseFields.type => FieldKeyRec(-1100, FieldKeyType.NormalAndAlternateCourses, None)
//      case _: ExceptionCourseFields.type          => FieldKeyRec(-1101, FieldKeyType.ExceptionCourses, None)
//    }
//    FieldListRec(pv, keys)
//  }

  def eventually(cond: => Any) {
    val test = (sleep: Int) => try { cond; true } catch { case _: Throwable => Thread.sleep(sleep); false }
    if (!test(10))
      if (!test(10))
        if (!test(20))
          if (!test(20))
            if (!test(50))
              if (!test(100))
                if (!test(1000))
                  cond
  }

  def eventuallyIf(wait: Boolean)(cond: => Any) { if (wait) eventually(cond) else cond }

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  @tailrec final def deepestLast[N <: TreeNodeLike[N]](n: N): N =
    if (n.children.isEmpty) n else deepestLast(n.children.last)

  def createTempDir(prefix: String, suffix: String = ""): File = {
    val tmpDir = File.createTempFile(prefix, suffix)
    tmpDir.delete
    tmpDir.mkdir
    FileUtils.forceDeleteOnExit(tmpDir)
    tmpDir
  }

  def testListOfZeroOrOne[T](expectation: Option[Any], actual: List[T])(testFn: T => Any) {
    if (expectation.isEmpty)
      actual should be('empty)
    else {
      actual should have size (1)
      testFn(actual(0))
    }
  }

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  def withSessionAttrs[U](attrs: (String, String)*)(block: => U): U = withSessionAttrs(Map(attrs: _*))(block)
  def withSessionAttrs[U](attrs: Map[String, String])(block: => U): U = inMockSession{S.withAttrs(S.mapToAttrs(attrs))(block)}

  def withSessionParams[U](params: Map[String, String])(block: => U): U = withSessionParams(params.toSeq: _*)(block)
  def withSessionParams[U](params: (String, String)*)(block: => U): U = {
    val req = new MockHttpServletRequest()
    req.parameters = params.toList
    MockWeb.testS(req)(block)
  }

  def assertJsAlert(js: JavaScriptReaction, errorMsg: Option[String]) { assertJsAlert(js.result.toJsCmd, errorMsg) }
  def assertJsAlert(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should include ("alert")
      jsReaction should include(errorMsg.get)
    } else
      jsReaction.toLowerCase should not include ("alert")
  }

  def assertJsErrorNotice(js: JavaScriptReaction, errorMsg: Option[String]) { assertJsErrorNotice(js.result.toJsCmd, errorMsg) }
  def assertJsErrorNotice(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should (include ("#notices") and include("alert-error"))
      jsReaction should include(errorMsg.get.encJs.replaceAll("^\"|\"$", ""))
    } else {
      jsReaction.toLowerCase should not include ("#notices")
      jsReaction.toLowerCase should not include ("alert-error")
    }
  }

  def time[U](logFn: Float => Any)(fn: => U): U = {
    val start = System.currentTimeMillis
    val result = fn
    val end = System.currentTimeMillis
    val time = (end - start).toFloat / 1000f
    logFn(time)
    result
  }

  def fixTopLevelIndices(nodes: List[StepNode]): List[StepNode] =
    for ((n, i) <- nodes.zipWithIndex) yield n.copy(labelIndex = i)

  def assertStepsAndLabelsRegen(uc: UseCase) {
    uc.stepsAndLabels.get should be(UseCaseFns.generateStepAndLabelBiMap(uc.fieldValues, uc.header).get)
  }

  def assertUseCasesMatchIgnoringStepsAndLabels(actual: UseCase, expected: UseCase) {
    actual.copy(stepsAndLabels = EmptyStepAndLabelBiMap) should be(expected.copy(stepsAndLabels = EmptyStepAndLabelBiMap))
  }

  def assertUseCasesMatch(actual: UseCase, expected: UseCase) {
    actual.header should be(expected.header)
    actual.fields should be(expected.fields)
    actual.fieldValues.norm should be(expected.fieldValues.norm)
  }

  def freeText(txt: String) = FreeText(txt, Map.empty)

  def normaliseFieldValues(fieldValues: FieldValues): FieldValues = fieldValues.mapValues{
    case v: StepFieldValue => v.norm.asInstanceOf[Field#Value]
    case v: FreeText => v.norm.asInstanceOf[Field#Value]
    case v => v
  }

  def normaliseFreeText(s: FreeText): FreeText = s.copy(refs = s.refs.norm)
  def normaliseStepText(s: StepText): StepText = s.copy(
    mainClause = s.mainClause.norm,
    flowFromClause = s.flowFromClause.map(normaliseFlowFromClause),
    flowToClause = s.flowToClause.map(normaliseFlowToClause)
  )
  def normaliseFlowFromClause(c: FlowFromClause): FlowFromClause = c.copy(refs = c.refs.norm)
  def normaliseFlowToClause(c: FlowToClause): FlowToClause = c.copy(refs = c.refs.norm)

  def normaliseRefs(r: Refs): Refs = r.map {
    case (id, ref) => (ref.asLocalId -> ref)
  }

  def normaliseStepFieldValue(s: StepFieldValue): StepFieldValue = {
    var newTextmap = Map.empty[LocalIdStr, StepText]
    // Add treeroot map
    val newNodes = s.tree.nodes.map(n => n.deepCopy[StepNode] {
      (stepNode, children) =>
        val newId = s"id/${stepNode.level}.${stepNode.labelIndex}".asLocalId
        s.textmap.get(stepNode.id).map(
          txt => newTextmap += (newId -> txt.copy(stepId = newId).norm)
        )
        stepNode.copy(id = newId, children = children)
    })
    s.copy(tree = StepTree(newNodes), textmap = newTextmap)
  }

  def removeNcField(uc: UseCase): UseCase = {
    val noNc: PartialFunction[Field, Boolean] = { case _:NormalCourseField => false; case _=>true}
    val f = uc.fields.filter(noNc)
    val fv = uc.fieldValues.filterKeys(noNc)
    uc.copy(fields = f, fieldValues = fv).regenerateStepsAndLabels
  }

  def assertStepTree(uc: UseCase, f: StepField, expectedTreeText: String) {
    f.getTextTree(uc) should matchTree(parseStepTree(expectedTreeText))
  }

  /**
   * ScalaTest matcher for text trees.
   */
  case class TextTreeMatcher(expected: List[StepNodeWithText]) extends Matcher[List[StepNodeWithText]] {
    def apply(actual: List[StepNodeWithText]): MatchResult = {
      val result = removeIds(actual) == removeIds(expected)
      MatchResult(result,
        "Trees didn't match.\n" + inspectTrees("EXPECTED", expected, "ACTUAL", actual),
        "Trees matched but shouldn't have.\n" + inspectTree(actual))
    }
  }
  def matchTree(expected: List[StepNodeWithText]) = TextTreeMatcher(expected)

  // ===================================================================================================================

  /**
   * Extensions for: Any
   */
  implicit class AnyExt[T](val v: T) {
    // Equality assertion with type equivalence ala Specs2
    def ====(that: T): Unit = v should be(that)
  }

  /**
   * Extensions for: Int
   */
  implicit class MyRichInt(val i: Int) {
    def times(block: => Any) { 1 to i foreach(_ => block) }
  }

  /**
   * Extensions for: StepFieldValue
   */
  implicit class StepFieldValueExt(val v: StepFieldValue) {
    def norm = normaliseStepFieldValue(v)

    def toTextTree(field: StepField, savedSteps: SavedSteps = EmptySavedSteps): List[StepNodeWithText] =
      convertNodeTree[StepNode, StepNodeWithText](v.tree, {
        case (n, lvl, lbl, children) =>
          val txt = v.textmap.get(n.id).map(_.textWithNormalisedRefs(savedSteps)).getOrElse("".hasNormalisedRefs)
          StepNodeWithText(n.id, lvl, lbl, txt, children)
      }, field.sli.startingLabelIndex _)
  }

  /**
   * Extensions for: List[StepNodeWithText]
   */
  implicit class TextTreeExt(val x: List[StepNodeWithText]) {
    def toStepTree = StepTree(x.map(_.toStepNode))

    def toTextmap(savedSteps: SavedSteps = EmptySavedSteps, stepsAndLabels: StepAndLabelBiMap = EmptyStepAndLabelBiMap) =
      TreeLike(x).mapRecursive[(LocalIdStr, StepText)](n => {
        val t = StepText.load(n.id, n.text.hasNormalisedRefs)(savedSteps, stepsAndLabels)
        (n.id, t)
      }).toMap

    def toStepFieldValue(f: StepField, savedSteps: SavedSteps = EmptySavedSteps, stepsAndLabels: StepAndLabelBiMap = EmptyStepAndLabelBiMap) =
      StepFieldValue(f, toStepTree, toTextmap(savedSteps, stepsAndLabels))
  }

  /**
   * Extensions for: TextField
   */
  implicit class TextFieldExt(val f: TextField) {
    def lens = Lens.lensg[UseCase, FreeText](
      u => v => FieldLenses.uc.textField.set((u, f), v),
      u => FieldLenses.uc.textField.get(u, f))
  }

  /**
   * Extensions for: StepField
   */
  implicit class StepFieldExt(val f: StepField) {
    def lens = Lens.lensg[UseCase, StepFieldValue](
      u => v => FieldLenses.uc.stepField.set((u, f), v),
      u => FieldLenses.uc.stepField.get(u, f))
    def getTextTree(uc: UseCase) = lens.get(uc).toTextTree(f)
  }

  /**
   * Extensions for: FieldValues
   */
  implicit class FieldValuesExt(val x: FieldValues) {
    def norm = normaliseFieldValues(x)
  }

  /**
   * Extensions for: FreeText
   */
  implicit class FreeTextExt(val v: FreeText) {
    def norm = normaliseFreeText(v)
  }

  /**
   * Extensions for: StepText
   */
  implicit class StepTextExt(val v: StepText) {
    def norm = normaliseStepText(v)
  }

  /**
   * Extensions for: Refs
   */
  implicit class RefsExt(val v: Refs) {
    def norm = normaliseRefs(v)
  }

  /**
   * Extensions for: UseCaseRev
   */
  implicit class UseCaseRevExt(val v: UseCaseRev) {
    def withTitle(t: String) = v.copy(header = v.header.copy(title = t))
  }

  /**
   * Extensions for: ChangeResultF
   */
  implicit class ChangeResultFExt[V, C](val r: ChangeResultF[V, C]) {
    def gimme: V = openChange._1

    def openChange: (V, List[C]) = r match {
      case Changed(v, c) => (v, c.list)
      case _ => fail(s"Change expected. Got: $r")
    }

    def openFailure(r: ChangeResultF[_, _]): String = r match {
      case ChangeFailure(err) => err
      case _ => fail(s"ChangeFailure expected. Got: $r")
    }
  }
}

object TestHelpers extends TestHelpers
