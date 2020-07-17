package shipreq.webapp.base.event

import nyaya.prop._
import nyaya.test.PropTest._
import nyaya.util.NyayaUtilAnyExt
import scalaz.std.list.listInstance
import scalaz.std.vector.vectorInstance
import shipreq.base.util.DeletionMethod
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.test.DataTestExt._
import utest._

object EventPropTests extends TestSuite {

  val AE = ApplyEvent.untrusted

  final class Tester(p: Project) {
    private val E = EvalOver(p)

    // Ensure project is valid. Only need to do this once globally and here it is.
    p assertSatisfies DataProp.project.allIncludingConfig

    private def deletableStepProps =
      E.forall(p.useCaseStepsDeletable.map(_.id).toList) { id =>
        p.deletionMethodForUseCaseStep(id) match {

          case DeletionMethod.Soft =>
            val a = UseCaseStepDelete (id)
            val b = UseCaseStepRestore(id)
            E.equal("DeleteUseCaseStep + RestoreUseCaseStep = id",
              actual = AE.apply1(a)(p).flatMap(AE.apply1(b)),
              expect = \/-(p))

          case DeletionMethod.Hard =>
            val stepIds = p.content.reqs.useCases.stepIdSet
            val f       = p.content.reqs.useCases.focusStep(id)
            val allIds  = f.subtree.locAndValueIterator(f.loc, (_, a) => a.id).toSet + id
            val e       = UseCaseStepDelete(id)
            E.equal("DeleteUseCaseStep hard delete",
              actual = AE.apply1(e)(p).map(_.content.reqs.useCases.stepIdSet),
              expect = \/-(stepIds -- allIds))
        }
      }

    // Any live custom req-types can be deleted
    // Whether deletion is soft or hard doesn't matter, so long as the event succeeds and the resulting project is valid
    // This is to catch the case that a req-type is hard-deleted but still referenced elsewhere in the project
    private def customReqTypeDeletion =
      E.forall(p.config.reqTypes.liveCustomReqTypes) { rt =>
        val event = CustomReqTypeDelete(rt.id)
        val error = ApplyEvent.untrusted.apply1(event)(p).swap.toOption
        E.equal(s"$event must succeed", error, None)
      }

    def all: EvalL =
      customReqTypeDeletion & deletableStepProps
  }

  val prop = Prop.eval((p: Project) => new Tester(p).all)

  override def tests = Tests {
    RandomData.project.mustSatisfy(prop)(defaultPropSettings
      .setGenSize(4 `JVM|JS` 2)
      .setSampleSize(10 `JVM|JS` 4)
    )
  }
}
