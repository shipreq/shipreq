package shipreq.webapp.base.data

import japgolly.nyaya._
import japgolly.nyaya.test.PropTest._
import japgolly.nyaya.test._
import utest._
import scalaz.std.AllInstances._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData, RandomData.CustomGenExt
import shipreq.webapp.base.data._

object ReqsTest extends TestSuite {

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
      (allocPubidLookup ==> allocTwiceIsNoop) ∧
      (oneReqPerReqtype("Input", register) ==> oneReqPerReqtype("After alloc", reg2))
    )
  }

  def gen: Gen[PubidRegisterProps] =
    for {
      reqTypeIds ← RandomData.customReqTypeId.nev
      (pr, reqs) ← RandomData.pubidRegisterAndIds(reqTypeIds)
      req        ← Gen.newOrOld(RandomData.genericReqId.subst[ReqIdC])(reqs)
      reqType    ← Gen.newOrOld(RandomData.customReqTypeId)(reqTypeIds.whole)
    } yield PubidRegisterProps(pr, req, reqType)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all)
  }
}