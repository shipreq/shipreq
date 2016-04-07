package shipreq.webapp.base.data

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen
import shipreq.base.util._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.event._
import VectorTree.LocationOps

object UseCaseStepTreeTest extends TestSuite {
  import shipreq.webapp.base.data.{StaticField => SF}
  import SF.{UseCaseStepTree => UCF}

  val genUseCaseStepId: Gen[UseCaseStepId] =
    BaseUtilGen.counter().map(UseCaseStepId)

  val genUseCaseStep: Gen[UseCaseStep] =
    genUseCaseStepId.map(UseCaseStep(_, Vector.empty))

  def genUseCaseSteps(f: UCF): Gen[UseCaseSteps] =
    RandomData.useCaseSteps(genUseCaseStep, f)(0 to 4)

  val genUseCase: Gen[UseCase] = {
    val base = UseCase(UseCaseId(1), ReqTypePos(1), Vector.empty, UseCaseSteps.empty, UseCaseSteps.empty, Live)
    for {
      na <- genUseCaseSteps(SF.NormalAltStepTree)
      e  <- genUseCaseSteps(SF.ExceptionStepTree)
    } yield base.copy(stepsNA = na, stepsE = e)
  }

  val genProject: Gen[Project] =
    genUseCase.map { uc =>
      val ucs = UseCases.Stateless(emptyDataMap(UseCase) + uc, UseCases.StepFlow.emptyBiDir).withState
      val pr  = PubidRegister(PubidRegister.emptyMM.add(StaticReqType.UseCase, uc.id))
      val p   = (Project.useCases.set(ucs) compose Project.pubidRegister.set(pr))(Project.empty)
      val ids = IdCeilings.calculate(p)
      p.copy(idCeilings = ids)
    }

  class Tester(p: Project) {
    val uc = p.reqs.useCases.imap.valuesIterator.next()
    val nextStepId = UseCaseStepId(p.idCeilings.useCaseStep + 1)

    def compare(actual: Permission, event: Event) = {
      val result = ApplyEvent.untrusted.apply1(event)(p)
      result match {
        case \/-(_) => Eval.equal(event.toString, actual, actual, Allow)
        case -\/(e) => Eval.atom(event.toString, actual, actual match {
          case Deny  => None
          case Allow => Some(s"Expected Deny, got Allow. Event failed because: $e")
        })
      }
    }

    def step(f: UCF, l: VectorTree.Location, id: UseCaseStepId, mdt: VectorTree[Int]): EvalL =
      ( compare(f.canDelete(l),          DeleteUseCaseStep(id))
      & compare(f.canShiftLeft(l),       ShiftUseCaseStepLeft(id))
      & compare(f.canShiftRight(l, mdt), ShiftUseCaseStepRight(id))
      & compare(f.canAdd(l),             AddUseCaseStep(nextStepId, uc.id, f, l.asParentLoc))
      ).rename(s"${f.name} / $id / ${l.whole mkString "."}")

    def tree(f: UCF) = {
      val tree = f.useCaseStepTree.get(uc)
      val mdt  = tree.maxDepthTree
      val data = tree.locAndValueIterator((l, s) => (l, s.id)).toList
      Eval.forall((), data)(x => step(f, x._1, x._2, mdt)).rename(f.name)
    }

    def all =
      tree(SF.NormalAltStepTree) & tree(SF.ExceptionStepTree)
  }

  val prop = Prop.eval[Project](new Tester(_).all)

  override def tests = TestSuite {
    "UseCaseStepTree.canXxx" - genProject.mustSatisfy(prop) //(defaultPropSettings.setDebug)
  }
}
