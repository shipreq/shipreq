package shipreq.webapp
package feature.uc
package persist

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalatest.FunSuite
import org.scalatest.prop._
import Prop._

import lib.Misc._
import lib.Types._
import field._
import step._
import test.TestDatabaseSupport
import test.DataGenerators._
import app.Defaults
import shipreq.webapp.lib.Locks

class UseCasePersistenceProps extends FunSuite with TestDatabaseSupport with Checkers {

  // Each check pass will run in its own transaction
  override val wrapTestsInTransaction = false

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 10, workers = 1)

  // -------------------------------------------------------------------------------------------------------------------

  test("load (save uc) = uc") {check(SaveAndLoadP)}

  test("save (save uc) = NOP") {check(SecondSaveIsNopP)}

  test("ids <$> load = load ids") {check(BulkLoadP)}

  test("(mutate a = mutate a) && (mutate b = mutate b)") {check(ConsistentMutationPerUseCaseInstance)}

  test("m x ⇒ load (save (m x)) = m x") {
    try check(IncrementalSaveAndLoad) finally IncrementalSaveAndLoad.system.uninit()
  }

  test("Caught Failure #1") {
    import scalaz.Name, shipreq.base.util.BiMap, db._, field._, text._, FreeTextTerms._

    def testSave(uc: UseCase, projectId: ProjectId, prev: Option[UseCaseSaveCheckpoint] = None) = {
      val cpO = save(uc, prev, projectId)
      val l = load(cpO.getOrElse(prev.get).rec, projectId)
      assertUseCasesLookSameToUser(l.uc, uc)
      cpO
    }

    // ###### UC 1/3 ######
    val uc1 = UseCase.as(UseCaseNumber(1),UseCaseHeader("Do Stuff")
      ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(FieldKeyId(10),FieldKeyType.Text,Some("Description")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(FieldKeyId(11),FieldKeyType.Text,Some("Actors")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(FieldKeyId(12),FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(FieldKeyId(13),FieldKeyType.Text,Some("Post-Conditions")))~>FreeText.empty
        ,NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None))~>StepFieldValue(NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode(LocalStepId("F1197205450689KYVCAS"),0,0,List(StepNode(LocalStepId("F1197205450688HDKO1W"),1,1,Nil))))),Map(LocalStepId("F1197205450689KYVCAS")->StepText(FreeText(List(PlainText("Do Stuff"))),None,None),LocalStepId("F1197205450688HDKO1W")->StepText.empty))
        ,ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None)),StepTree(Nil),Map())
        ,FlowGraphField(FieldKeyRec(FieldKeyId(33),FieldKeyType.FlowGraph,None))~>()
        ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(FieldKeyId(16),FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(FieldKeyId(17),FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(FieldKeyId(18),FieldKeyType.Text,Some("Frequency of Use")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(FieldKeyId(19),FieldKeyType.Text,Some("Special Requirements")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(FieldKeyId(20),FieldKeyType.Text,Some("Assumptions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(FieldKeyId(21),FieldKeyType.Text,Some("Notes and Issues")))~>FreeText.empty
      ),Name(BiMap(LocalStepId("F1197205450688HDKO1W")->StepLabel("1.0.1"),LocalStepId("F1197205450689KYVCAS")->StepLabel("1.0"))))
    // ###### UC 2/3 ######
    val uc2 = UseCase.as(UseCaseNumber(1),UseCaseHeader("Do Stuff")
      ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(FieldKeyId(10),FieldKeyType.Text,Some("Description")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(FieldKeyId(11),FieldKeyType.Text,Some("Actors")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(FieldKeyId(12),FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(FieldKeyId(13),FieldKeyType.Text,Some("Post-Conditions")))~>FreeText.empty
        ,NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None))~>StepFieldValue(NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode(LocalStepId("F1197205450689KYVCAS"),0,0,List(StepNode(LocalStepId("F1197205450688HDKO1W"),1,1,Nil))))),Map(LocalStepId("F1197205450689KYVCAS")->StepText(FreeText(List(PlainText("Do Stuff"))),None,None),LocalStepId("F1197205450688HDKO1W")->StepText.empty))
        ,ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None)),StepTree(Nil),Map())
        ,FlowGraphField(FieldKeyRec(FieldKeyId(33),FieldKeyType.FlowGraph,None))~>()
        ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(FieldKeyId(16),FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(FieldKeyId(17),FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(FieldKeyId(18),FieldKeyType.Text,Some("Frequency of Use")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(FieldKeyId(19),FieldKeyType.Text,Some("Special Requirements")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(FieldKeyId(20),FieldKeyType.Text,Some("Assumptions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(FieldKeyId(21),FieldKeyType.Text,Some("Notes and Issues")))~>FreeText.empty
      ),Name(BiMap(LocalStepId("F1197205450688HDKO1W")->StepLabel("1.0.1"),LocalStepId("F1197205450689KYVCAS")->StepLabel("1.0"))))
    // ###### UC 3/3 ######
    val uc3 = UseCase.as(UseCaseNumber(1),UseCaseHeader("Do Stuff")
      ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(FieldKeyId(10),FieldKeyType.Text,Some("Description")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(FieldKeyId(11),FieldKeyType.Text,Some("Actors")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(FieldKeyId(12),FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(FieldKeyId(13),FieldKeyType.Text,Some("Post-Conditions")))~>FreeText.empty
        ,NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None))~>StepFieldValue(NormalCourseField(FieldKeyRec(FieldKeyId(14),FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode(LocalStepId("F1197205450689KYVCAS"),0,0,Nil))),Map(LocalStepId("F1197205450689KYVCAS")->StepText(FreeText(List(PlainText("Do Stuff"))),None,None)))
        ,ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(FieldKeyId(15),FieldKeyType.ExceptionCourses,None)),StepTree(Nil),Map())
        ,FlowGraphField(FieldKeyRec(FieldKeyId(33),FieldKeyType.FlowGraph,None))~>()
        ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(FieldKeyId(16),FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(FieldKeyId(17),FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(FieldKeyId(18),FieldKeyType.Text,Some("Frequency of Use")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(FieldKeyId(19),FieldKeyType.Text,Some("Special Requirements")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(FieldKeyId(20),FieldKeyType.Text,Some("Assumptions")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(FieldKeyId(21),FieldKeyType.Text,Some("Notes and Issues")))~>FreeText.empty
      ),Name(BiMap(LocalStepId("F1197205450689KYVCAS")->StepLabel("1.0"))))

    rollbackAfter{
      val pid = newProjectId()
      val ui = dao.createUseCaseIdentWithForcedNumber(pid, uc1.number)
      val r1 = dao.createUseCaseRev(ui, 1:Short, uc1.header)
      val cp1 = Some(load(r1, pid))

      val cp2 = testSave(uc2, pid, cp1)
      val cp3 = testSave(uc3, pid, cp2)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  lazy val ConsistentMutationPerUseCaseInstance = forAll((a: UseCase, b: UseCase, m: UseCaseMutator) => {
    val aa = tryMutate(a, m)
    val bb = tryMutate(b, m)
    (aa =/= bb) ==> ((tryMutate(a, m) <==> aa) && (tryMutate(b, m) <==> bb))
  })

  lazy val SaveAndLoadP = forAll((uc: UseCase) => timedDbProp("SaveAndLoad", uc) {
    val pid = newProjectId()
    uc <==> saveAndLoad(uc, pid).uc
  })

  lazy val BulkLoadP = forAll((a: UseCase, b: UseCase) => dbProp {
    val pid = newProjectId()
    val x = save(a, None, pid).get
    val y = save(b, None, pid).get
    val both = List(x, y)
    val individually = both.map(cp => load(cp.rec, pid).uc).sorted
    val bulk = Locks.UseCaseNumbers.readP(pid)(UseCasePersistence.loadAll(pid).run(dao, _).map(_.uc)).sorted
    bulk.size == 2 &&
    bulk(0) <==> individually(0) &&
    bulk(1) <==> individually(1)
  })

  lazy val SecondSaveIsNopP = forAll((uc: UseCase) => timedDbProp("SecondSaveIsNop", uc) {
    val pid = newProjectId()
    val cp1 = saveAndLoad(uc, pid)
    val (save2, diffs) = collectTableDiffs {save(cp1.uc, Some(cp1), pid)}
    val tableChanges = diffs.filterNot {case (_, diff) => diff == 0}
    (tableChanges.isEmpty && save2.isEmpty) :| s"Shouldn't save the second time. UC content =\n${uc.inspect}"
  })

  // -------------------------------------------------------------------------------------------------------------------

  def tryMutate(uc: UseCase, m: UseCaseMutator): UseCase = m(uc)._1.getValueOrElse(uc)

  val load = loadUseCase _
  def save = saveUseCase _

  def saveAndLoad(uc: UseCase, projectId: ProjectId, prev: Option[UseCaseSaveCheckpoint] = None): UseCaseSaveCheckpoint =
    load(save(uc, prev, projectId).getOrElse(prev.get).rec, projectId)

  // -------------------------------------------------------------------------------------------------------------------

  implicit lazy val arbUseCase: Arbitrary[UseCase] =
    Arbitrary(useCaseGen(Defaults.fieldList.value, UseCaseNumber(1)))

//  implicit lazy val arbUseCaseMutators: Arbitrary[List[UseCaseMutator]] =
//    Arbitrary(Gen.listOfN(mutationsPerRun, useCaseMutator))

  def all(ps: Seq[Prop]) = Prop.all(ps: _*)

  def failMsg(name: String)(a: Any, b: Any) = s"$name failure:\n A=$a\n B=$b"

  def equal[T](name: => String)(a: T, b: T): Prop = (a == b) :|| failMsg(name)(a, b)

  def timedProp(name: String, uc: UseCase)(f: => Prop): Prop =
    time(t => info("%s: %6d steps in %.2f sec.".format(name, uc.stepsAndLabels.value.size, t)))(f)

  def timedDbProp(name: String, uc: UseCase)(f: => Prop): Prop = timedProp(name, uc)(dbProp(f))

  def dbProp[R](f: => R): R = rollbackAfter(f)

  implicit class UseCaseExt(val x: UseCase) {
    def <==>(y: UseCase): Prop = useCasesEqual(x, y)
    def =/=(y: UseCase): Prop = useCasesDiffer(x, y)
    def textFields: List[TextField] = filterCovar[TextField](x.fields)
    def stepFields: List[StepField] = filterCovar[StepField](x.fields)
  }

  def useCasesEqual(a: UseCase, b: UseCase): Prop = {
    val aa = a.userView
    val bb = b.userView
    (aa == bb) :|| s"Use cases don't match.\nA:\n$aa\nB:\n$bb"
  }

  def useCasesDiffer(a: UseCase, b: UseCase): Prop = {
    val aa = a.userView
    val bb = b.userView
    (aa != bb) :|| s"Use cases should differ.\n$aa"
  }
}
