package shipreq.webapp.base.data

import japgolly.nyaya._
import japgolly.nyaya.test.PropTest._
import japgolly.nyaya.test._
import utest._
import scalaz.std.AllInstances._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._

object ReqsTest extends TestSuite {

  val oneReqPerReqtypeProp =
    Prop.distinctC[Vector, Req.Id]("Req ID").forall((_: Pubid.Register).m.values.toStream)

  case class PubidRegisterProps(register: Pubid.Register, req: Req.Id, reqType: ReqType.Id) {
    val E            = EvalOver(this)
    val (reg2, pid2) = Pubid.alloc(req, reqType, register)
    val (reg3, pid3) = Pubid.alloc(req, reqType, reg2)

    def allocPubidLookup =
      E.equal("lookup(alloc(req)) = req", Pubid.lookup(reg2, pid2), req.some)

    def allocTwiceIsNoop = "allocTwiceIsNoop" rename_: (
      E.equal("Pubid",    pid2, pid3) ∧
      E.equal("Register", reg2, reg3))

    def oneReqPerReqtype(name: String, r: Pubid.Register) =
      (oneReqPerReqtypeProp rename s"$name: One req/reqType")(r).liftL

    def all = "Pubid Register" rename_: (
      (allocPubidLookup ==> allocTwiceIsNoop) ∧
      (oneReqPerReqtype("Input", register) ==> oneReqPerReqtype("After alloc", reg2))
    )
  }

  import ReqCodesTest.newOrOld // TODO DELETE
  def gen: Gen[PubidRegisterProps] =
    for {
      reqTypeIds ← RandomData.reqTypeId.list1
      (pr, reqs) ← RandomData.pubidRegisterAndIds(reqTypeIds)
      req        ← newOrOld(RandomData.reqId)(reqs)
      reqType    ← newOrOld(RandomData.reqTypeId)(reqTypeIds.list)
    } yield PubidRegisterProps(pr, req, reqType)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all)
  }
}