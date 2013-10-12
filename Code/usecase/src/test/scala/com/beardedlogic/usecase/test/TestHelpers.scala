package com.beardedlogic.usecase
package test

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.mockito.Mockito.when
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.{BeforeAndAfterEach, Suite, BeforeAndAfterAll}
import org.scalatest.Matchers
import org.scalatest.matchers.{Matcher, MatchResult}
import org.scalatest.mock.MockitoSugar
import org.scalatest.prop.Tables.Table
import net.liftweb.common.{Logger, Failure, Box, Empty}
import net.liftweb.http.{ResponseShortcutException, S, LiftSession, LiftRules}
import net.liftweb.http.js.JsCmd
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.util.StringHelpers
import net.liftweb.util.Helpers.stringToSuper
import scalaz.{Lens, NonEmptyList, Value}
import scala.annotation.tailrec
import scala.util.Random

import lib.change._
import lib.tree._
import lib.field._
import lib.text._
import lib._
import db._
import security.SecurityProvider
import util._

import Types._
import Lenses._
import LensFns._
import NodeUtils._
import TreeOps._
import Changes.ExistingStepLabelsChanged

case class FixedUser(ud: Option[UserDescriptor]) extends SecurityProvider {
  override def loggedInUser = ud
  def install[R](fn: => R): R = DI.SecurityProvider.doWith(this)(fn)
}

private object TestHelperConsts {
  val Random = new Random()
}

/**
 * @since 30/04/2013
 */
trait TestHelpers2 extends MockitoSugar with Matchers with DebugImplicits with Logger {

  val Cores = Math.max(1, Runtime.getRuntime().availableProcessors - 1)

  implicit def JsCmdToStr(js: JsCmd): String = js.toJsCmd

  type Refs = Map[LocalStepId, StepLabel]

  def savedSteps(tuples: (Int, LocalStepId)*): BiMap[TextIdentId, LocalStepId] =
    BiMap.apply(tuples.map(t => (t._1.toLong.tag[IsTextIdentId], t._2)): _*)

  def mapToLabels(i: Traversable[LocalStepId], stepState: StepAndLabelBiMap = StepState1) = i.map(id => (id, stepState.value.ab(id))).toMap
  def mapFromLabels(i: Traversable[LocalStepId], stepState: StepAndLabelBiMap = StepState1) = i.map(id => (stepState.value.ab(id), id)).toMap

  def mapToIds(i: Traversable[StepLabel], stepState: StepAndLabelBiMap = StepState1) = i.map(l => (l, stepState.value.ba(l))).toMap
  def mapFromIds(i: Traversable[StepLabel], stepState: StepAndLabelBiMap = StepState1) = i.map(l => (stepState.value.ba(l), l)).toMap

  def flowFromClause(refs: (LocalStepId, StepLabel)*) = if (refs.isEmpty) None else Some(FlowFromClause(Map(refs: _*)))
  def flowToClause(refs: (LocalStepId, StepLabel)*) = if (refs.isEmpty) None else Some(FlowToClause(Map(refs: _*)))

  def MockExistingStepLabelsChanged = {
    val m = mock[ExistingStepLabelsChanged]
    when(m.asOnlyChange).thenReturn(NonEmptyList(m))
    m
  }

  val UD1 = UserDescriptor(5001.tag[UserId], "U1", "U1@TEST")
  val UD2 = UserDescriptor(5002.tag[UserId], "U2", "U2@TEST")

  val X0 = "X0".asLocalStepId
  val X1 = "X1".asLocalStepId
  val X2 = "X2".asLocalStepId
  val X3 = "X3".asLocalStepId
  val X4 = "X4".asLocalStepId
  val X5 = "X5".asLocalStepId
  val X6 = "X6".asLocalStepId
  val X7 = "X7".asLocalStepId
  val X8 = "X8".asLocalStepId
  val X9 = "X9".asLocalStepId
  val X3E1 = "X3E1".asLocalStepId
  val X3E2 = "X3E2".asLocalStepId

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

  val StepState1: StepAndLabelBiMap = Value(BiMap(X1 -> S1, X2 -> S2, X3 -> S3, X5 -> S5, X6 -> S6, X0 -> S0))
  val StepState2: StepAndLabelBiMap = Value(BiMap(X1 -> SA, X2 -> S2, X4 -> S4, X5 -> SF, X6 -> S6))

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

  val StepStateB: StepAndLabelBiMap = Value(BiMap(
    X1 -> "1.0".asLabel,
    X2 -> "1.2".asLabel,
    X3 -> "1.3".asLabel,
    X3E1 -> "3.E.1".asLabel,
    X3E2 -> "3.E.2".asLabel))

  implicit def FieldToFKRec(f: Field): FieldKeyRec = f.rec
  implicit def FieldToFkId(f: Field): FieldKeyId = f.rec.id
  implicit def SfvToStepTree(sfv: StepFieldValue): StepTree = sfv.tree

  lazy val TF1 = mockTextField("Stuff #1", 111)
  lazy val TF2 = mockTextField("Stuff #2", 222)
  lazy val TF3 = mockTextField("Stuff #3", 333)
  lazy val TF4 = mockTextField("Stuff #4", 444)

  lazy val NCF = NormalCourseField(FieldKeyRec(55.tag[IsFieldKeyId], NormalCourseFieldDefinition.fieldKeyType, NormalCourseFieldDefinition.fieldKeyData))
  lazy val ECF = ExceptionCourseField(FieldKeyRec(66.tag[IsFieldKeyId], ExceptionCourseFieldDefinition.fieldKeyType, ExceptionCourseFieldDefinition.fieldKeyData))

  def rnd = TestHelperConsts.Random

  // -------------------------------------------------------------------------------------------------------------------

  def withUserLoggedIn[R](loggedInUser: Option[UserDescriptor])(fn: => R): R =
    FixedUser(loggedInUser).install(fn)

  def mockSavedStepsFor(tree: StepTree): SavedSteps = {
    val savedSteps = new BiMapBuilder[TextIdentId, LocalStepId]
    var i = 0
    tree.foreachRecursive(s => {
      i += 1
      val textIdentId = (i * 1000).tag[IsTextIdentId]
      savedSteps += (textIdentId -> s.id)
    })
    savedSteps.result
  }

  def mockCreateInitialTextAnswer(startingId: Long) = new Answer[TextIdentId] {
    var id = startingId - 1
    override def answer(invocation: InvocationOnMock) = {
      id += 1
      id.tag[IsTextIdentId]
    }
  }

  def mockCreateTextRevAnswer = new Answer[TextRev] {
    override def answer(i: InvocationOnMock) = {
      val identId = i.getArguments()(0).asInstanceOf[TextIdentId]
      val rev = i.getArguments()(1).asInstanceOf[Short]
      val text = i.getArguments()(2).asInstanceOf[TextWithNormalisedRefs]
      val id = (identId*10).tag[IsTextRevId]
      TextRev(identId, rev, id, text)
    }
  }

  def mockLinkUcToStepAnswer = new Answer[UcFieldText] {
    override def answer(i: InvocationOnMock) = {
      val uc = i.getArguments()(0).asInstanceOf[UseCaseRevId]
      val label = i.getArguments()(1).asInstanceOf[StepLabel]
      val index = i.getArguments()(2).asInstanceOf[Short]
      val parentId = i.getArguments()(3).asInstanceOf[Option[TextRevId]]
      val text = i.getArguments()(4).asInstanceOf[TextRev]
      UcFieldText(Some(label), parentId, index, text)
    }
  }

  val stepTreeLens = {
    ucStepFieldL >@=> Lens.lensg[StepFieldValue, StepTree](
      sfv => t => sfv.withNewTree(t),
      sfv => sfv.tree
    )
  }

  def mockTextField(title: String, id: Long) = {
    val defn = TextFieldDefinition(title)
    TextField(defn, FieldKeyRec(id.tag[IsFieldKeyId], FieldKeyType.Text, defn.fieldKeyData))
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
  def meq[T](v: T) = org.mockito.Matchers.eq(v)

  def countOccurrences(str1: String, str2: String): Int = {
    @tailrec def count(pos: Int, c: Int): Int = {
      val idx = str1 indexOf(str2, pos)
      if (idx == -1) c else count(idx + str2.size, c + 1)
    }
    count(0, 0)
  }

  @tailrec final def deepestLast[N <: TreeNodeLike[N]](n: N): N =
    if (n.children.isEmpty) n else deepestLast(n.children.last)

  def createTempDir(prefix: String, suffix: String = ""): File = {
    val tmpDir = File.createTempFile(prefix, suffix)
    tmpDir.delete
    tmpDir.mkdir
    FileUtils.forceDeleteOnExit(tmpDir)
    tmpDir
  }

  def findTransformable[I, O](inputs: IndexedSeq[I], eval: I => O)(test: O => Boolean): Option[(I, O)] = {
    val listSize = inputs.length
    @tailrec def go(attemptsRem: Int, pos: Int): Option[(I, O)] = {
      if (attemptsRem == 0) None
      else {
        val i = inputs(pos)
        val o = eval(i)
        if (test(o)) Some((i, o))
        else go(attemptsRem - 1, (pos + 1) % listSize)
      }
    }
    if (listSize == 0) None else go(listSize, rnd.nextInt(listSize))
  }

  def findSuitable[T](get: => T)(check: T => Boolean): T = {
    var t = get
    while (!check(t)) t = get
    t
  }

  def testListOfZeroOrOne[T](expectation: Option[Any], actual: List[T])(testFn: T => Any) {
    if (expectation.isEmpty)
      actual shouldBe empty
    else {
      actual should have size (1)
      testFn(actual(0))
    }
  }

  def login(username: String, password: String): Unit =
    SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password))

  def logout(): Unit = SecurityUtils.getSubject.logout

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

  def assertJsAlert(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should include ("alert")
      jsReaction should include(errorMsg.get)
    } else
      jsReaction.toLowerCase should not include ("alert")
  }

  def assertJsErrorNotice(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should (include ("#notices") and include("alert-danger"))
      jsReaction should include(errorMsg.get.encJs.replaceAll("^\"|\"$", ""))
    } else {
      jsReaction.toLowerCase should not include ("#notices")
      jsReaction.toLowerCase should not include ("alert-danger")
    }
  }

  def assertRedirect(block: => Any): ResponseShortcutException = {
    val err = intercept[ResponseShortcutException](block)
    err.redirectTo should not be empty
    err
  }

  class Timer {
    val start = System.currentTimeMillis
    def elapsedMs = System.currentTimeMillis - start
    def elapsedSec = elapsedMs.toFloat / 1000f
    def elapsedSec2dp = "%.2f".format(elapsedSec)
  }

  def time[U](logFn: Float => Any)(fn: => U): U = {
    val t = new Timer
    val result = fn
    logFn(t.elapsedSec)
    result
  }

  def fixTopLevelIndices(nodes: List[StepNode]): List[StepNode] =
    for ((n, i) <- nodes.zipWithIndex) yield n.copy(labelIndex = i)

  def assertStepsAndLabelsRegen(uc: UseCase) {
    uc.stepsAndLabels.value ==== UseCaseFns.generateStepAndLabelBiMap(uc).value
  }

  def assertUseCasesMatchIgnoringStepsAndLabels(actual: UseCase, expected: UseCase) {
    actual.copy(stepsAndLabels = EmptyStepAndLabelBiMap) should be(expected.copy(stepsAndLabels = EmptyStepAndLabelBiMap))
  }

  // TODO deprecate assertUseCasesMatch and rely on userView?
  def assertUseCasesMatch(actual: UseCase, expected: UseCase) {
    actual.header should be(expected.header)
    actual.fields should be(expected.fields)
    actual.fieldValues.norm should be(expected.fieldValues.norm)
  }

  def assertUseCasesLookSameToUser(actual: UseCase, expected: UseCase): Unit =
    actual.userView ==== expected.userView

  def freeText(txt: String) = FreeText(txt, Map.empty, false)

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
    case (id, lbl) => (lbl.asLocalStepId -> lbl)
  }

  def normaliseStepFieldValue(s: StepFieldValue): StepFieldValue = {
    var newTextmap = Map.empty[LocalStepId, StepText]
    // Add treeroot map
    val newNodes = s.tree.nodes.map(n => n.deepCopy[StepNode] {
      (stepNode, children) =>
        val newId = s"id/${stepNode.level}.${stepNode.labelIndex}".asLocalStepId
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
   * Extensions for: AnyRef
   */
  implicit class AnyRefExt2[T <: AnyRef](val v: T) {
    def validated = v.tag[Validated]
  }

  /**
   * Extensions for: Int
   */
  implicit class MyRichInt(val i: Int) {
    def times(block: => Any) { 1 to i foreach(_ => block) }
  }

  /**
   * Extensions for: String
   */
  implicit class MyRichString(val self: String) {
    def occurrences(of: String) = countOccurrences(self, of)
  }

  /**
   * Extensions for: LocalStepId
   */
  implicit class LocalStepIdExt(val id: LocalStepId) {
    def withLabel(uc: UseCase): String =
      uc.stepsAndLabels.value.ab.get(id).map(l => s"{$id:$l}").getOrElse(s"{$id:LABEL NOT FOUND}")
  }

  /**
   * Extensions for: JsCmd
   */
  implicit class MyRichJsCmd(val j: JsCmd) {
    def assertJsAlert(errorMsg: Option[String]) = TestHelpers2.this.assertJsAlert(j.toJsCmd, errorMsg)
    def assertJsErrorNotice(errorMsg: Option[String]) = TestHelpers2.this.assertJsErrorNotice(j.toJsCmd, errorMsg)
  }

  /**
   * Extensions for: Box
   */
  implicit class MyRichBox[T](val b: Box[T]) {
    def gimme: T = b.openOrThrowException(s"Box was expected to be Full, but was: $b")
    def gimmeErr: String = b match {
      case Failure(err,_,_) => err
      case r => fail(s"Failure expected. Got: $r")
    }
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

    def toTextmap(savedSteps: SavedSteps = EmptySavedSteps, sl: StepAndLabelBiMap = EmptyStepAndLabelBiMap) =
      TreeLike(x).mapRecursive[(LocalStepId, StepText)](n => {
        val t = StepText.load(n.id, n.text.hasNormalisedRefs)(savedSteps, UcParsingCtx.Empty.copy(stepsAndLabels = sl))
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
      u => v => ucTextFieldL.set((u, f), v),
      u => ucTextFieldL.get(u, f))
  }

  /**
   * Extensions for: StepField
   */
  implicit class StepFieldExt(val f: StepField) {
    def lens = Lens.lensg[UseCase, StepFieldValue](
      u => v => ucStepFieldL.set((u, f), v),
      u => ucStepFieldL.get(u, f))
    def getTextTree(uc: UseCase) = ucStepFieldL.get(uc, f).toTextTree(f)
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
    def withTitle(t: String @@ Validated) = v.copy(header = v.header.copy(title = t))
  }

  /**
   * Extensions for: ChangeResultF
   */
  implicit class ChangeResultFExt[V, C](val r: ChangeResultF[V, C]) {
    def gimme: V = openChange._1

    def gimmeOrElse(d: V): V = try gimme catch {case _: Throwable => d}

    def openChange: (V, List[C]) = r match {
      case Changed(v, c) => (v, c.list)
      case _ => fail(s"Change expected. Got: $r")
    }

    def openFailure(r: ChangeResultF[_, _]): String = r match {
      case ChangeFailure(err) => err
      case _ => fail(s"ChangeFailure expected. Got: $r")
    }
  }

  implicit class CreateProjectResultExt(r: CreateProjectResult) {
    def gimme: ProjectId = r match {
      case CreateProjectResult.Success(x) => x
      case x => fail("Failed to create random project id: " + x)
    }
  }
}

object TestHelpers extends TestHelpers2

trait TestHelpers extends TestHelpers2 with BeforeAndAfterAll with BeforeAndAfterEach {
  this: Suite =>

  if (!LiftRules.doneBoot) {
    val b = new bootstrap.liftweb.Boot
    b.configureLift
    b.preloadTemplates
  }

  override def beforeAll(): Unit = {
    if (!logoutBeforeEach) logout()
    super.beforeAll
  }

  override def beforeEach(): Unit = {
    if (logoutBeforeEach) logout()
    super.beforeEach
  }

  /** Logout performed before each test when true, and once before all tests when false. */
  var logoutBeforeEach = true
}
