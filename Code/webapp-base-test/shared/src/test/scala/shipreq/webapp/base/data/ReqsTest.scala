package shipreq.webapp.base.data

import nyaya.prop._
import nyaya.gen._
import nyaya.test.PropTest._
import nyaya.test._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._
import shipreq.base.util.{VectorTree, NonEmptyVector}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData

object ReqsTest extends TestSuite { // TODO Update for UCs

  val oneReqPerReqtypeProp =
    Prop.distinctC[Vector, ReqId]("Req ID").forall((_: PubidRegister).value.m.values.toStream)

  case class PubidRegisterProps(register: PubidRegister, req: ReqIdC, reqType: CustomReqTypeId) {
    val E            = EvalOver(this)
    val (reg2, pid2) = register.allocC(reqType)(req)
    val (reg3, pid3) = reg2    .allocC(reqType)(req)

    def allocPubidLookup =
      E.equal("lookup(alloc(req)) = req", reg2(pid2), req.some)

    def allocTwiceIsNoop = "allocTwiceIsNoop" rename_: (
      E.equal("Pubid",    pid2, pid3) ∧
      E.equal("Register", reg2, reg3))

    def oneReqPerReqtype(name: String, r: PubidRegister) =
      (oneReqPerReqtypeProp rename s"$name: One req/reqType")(r).liftL

    def all = "PubidRegister" rename_: (
      allocPubidLookup ∧ allocTwiceIsNoop ∧
      oneReqPerReqtype("Input", register) ∧ oneReqPerReqtype("After alloc", reg2)
    )
  }

  def gen: Gen[PubidRegisterProps] =
    for {
      reqTypeIds ← RandomData.customReqTypeId.vector1
      grCount    ← Gen.chooseSize
      ucCount    ← Gen.chooseSize
      prAndIds   ← RandomData.pubidRegisterAndIds(reqTypeIds, grCount, ucCount)
      req        ← Gen.newOrOld(RandomData.genericReqId: Gen[ReqIdC], prAndIds.grIds)
      reqType    ← Gen.newOrOld(RandomData.customReqTypeId, reqTypeIds)
    } yield PubidRegisterProps(prAndIds.pr, req, reqType)

  override def tests = TestSuite {
    'pubidRegister - gen.mustSatisfyE(_.all)

    'ucStepLabels {
      import StaticField.{NormalAltStepTree => N, ExceptionStepTree => E}
      implicit def autoPos(i: Int) = ReqTypePos(i)
      implicit def str2loc(s: String) = NonEmptyVector force s.split('.').toVector.map(_.toInt)

      def test(f: StaticField.UseCaseStepTree, uc: ReqTypePos, l: VectorTree.Location, exp: String): Unit = {
        assertEq(f.stepLabel(uc, l, false), exp)
        assertEq(f.stepLabel(uc, l, true), "UC-" + exp)
      }

      'na {
        test(N, 7, "0",         "7.0")
        test(N, 7, "0.0",       "7.0.1")
        test(N, 7, "0.0.0",     "7.0.1.a")
        test(N, 7, "0.0.0.0",   "7.0.1.a.i")
        test(N, 7, "0.0.0.0.0", "7.0.1.a.i.1")
        test(N, 3, "2.5.6.4.1", "3.2.6.g.v.2")
      }
      'e {
        test(E, 7, "0",       "7.E.1")
        test(E, 7, "0.0",     "7.E.1.a")
        test(E, 7, "0.0.0",   "7.E.1.a.i")
        test(E, 7, "0.0.0.0", "7.E.1.a.i.1")
        test(E, 3, "5.6.4.1", "3.E.6.g.v.2")
      }
    }
  }
}
