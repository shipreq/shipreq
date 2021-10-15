package shipreq.webapp.member.project.data

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import nyaya.util._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen
import shipreq.base.util._
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.project.RandomData
import utest._

object UseCaseStepTreeTest extends TestSuite {
  import shipreq.webapp.member.project.data.{StaticField => SF}
  import SF.{UseCaseStepTree => UCF}
  import Event._
  import VectorTree.Location
  import VectorTree.LocationOps

  val genUseCaseStepId: Gen[UseCaseStepId] =
    BaseUtilGen.counter().map(UseCaseStepId)

  val genUseCaseStep: Gen[UseCaseStep] =
    genUseCaseStepId.map(UseCaseStep(_, Text.empty, Live))

  def genUseCaseSteps(f: UCF): Gen[UseCaseSteps] =
    RandomData.useCaseSteps(genUseCaseStep, f)(0 to 3)

  val genUseCase: Gen[UseCase] =
    for {
      na <- genUseCaseSteps(SF.NormalAltStepTree)
      e  <- genUseCaseSteps(SF.ExceptionStepTree)
    } yield
      UseCase(UseCaseId(1), ReqTypePos(1), Text.empty, na, e, Live)

  val genProject: Gen[Project] =
    genUseCase.map { uc =>
      val ucs = UseCases.Stateless(emptyDataMap(UseCase) + uc, UseCases.StepFlow.emptyBiDir).withState
      val pr  = PubidRegister(PubidRegister.emptyMM.add(StaticReqType.UseCase, uc.id))
      val p   = (Project.useCases.replace(ucs) compose Project.pubidRegister.replace(pr))(Project.empty)
      val ids = IdCeilings.calculate(p)
      p.copy(idCeilings = ids)
    }

  class Tester(p: Project) {
    val uc = p.content.reqs.useCases.imap.valuesIterator.next()
    val nextStepId = UseCaseStepId(p.idCeilings.useCaseStep + 1)

    def compare(actual: Permission, event: Event) = {
      val result = ApplyEvent.untrusted.partialApplyUnverified(event)(p)
      result match {
        case \/-(_) => Eval.equal(event.toString, actual, actual, Allow)
        case -\/(e) => Eval.atom(event.toString, actual, actual match {
          case Deny  => None
          case Allow => Some(s"Expected Deny, got Allow. Event failed because: $e")
        })
      }
    }

    def step(f: UCF, l: Location, v: Location => Validity, mdt: VectorTree[Int], id: UseCaseStepId): EvalL =
      ( compare(f.canDelete(l),             UseCaseStepDelete(id))
      & compare(f.canShiftLeft(l),          UseCaseStepShiftLeft(id))
      & compare(f.canShiftRight(l, v, mdt), UseCaseStepShiftRight(id))
      & compare(f.canInsertAfter(l),        UseCaseStepCreate(nextStepId, uc.id, f, l.asParentLoc))
      ).rename(s"${f.name} / $id / ${l.whole mkString "."}")

    def tree(f: UCF) = {
      val steps = f.useCaseSteps.get(uc)
      val tree  = steps.tree
      val mdt   = tree.maxDepthTree
      val data  = tree.locAndValueIterator((l, s) => (l, s.id)).toList
      Eval.forall((), data)(x => step(f, x._1, steps.locValidity, mdt, x._2)).rename(f.name)
    }

    def all =
      tree(SF.NormalAltStepTree) & tree(SF.ExceptionStepTree)
  }

  val prop = Prop.eval[Project](new Tester(_).all) // & DataProp.project.allIncludingConfig.rename("Input is valid")

  override def tests = Tests {
    "UseCaseStepTree.canXxx" - {
      genProject.mustSatisfy(prop)(defaultPropSettings.setSampleSize(10 `JVM|JS` 3))
      // genProject.bugHunt(0, samplesPerSeed = 24)(prop)(defaultPropSettings.setSampleSize(1))
    }
  }
}
