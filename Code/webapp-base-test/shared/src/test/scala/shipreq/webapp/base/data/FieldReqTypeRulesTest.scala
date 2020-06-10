package shipreq.webapp.base.data

import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import utest._
import shipreq.base.util.univeq._
import shipreq.webapp.base.RandomData

object FieldReqTypeRulesTest extends TestSuite {
  import PropTest._

  private class Laws[D: UnivEq](rules: FieldReqTypeRules[D]) {
    val E = EvalOver(this)

    def eval = (
      E.equal("byResolution.toRules = id", rules, rules.byResolution.toRules)
    )

//    ( E.equal("set.remove = revert.sync", t.set(k, t.getP(k)(s))(t.remove(k)(s)),         t.setStatus(k, Sync)(revertBoth(k)(s)))
//    ∧ E.equal("get.set(v).set = v",       t.getI(k)(setab2(k)(setab(k)(s))),              (a2, b2))
//    ∧ E.equal("get.set(v).set = v",       t.getI(k)(setab2(k)(setab(k)(s))),              (a2, b2))
//    ∧ E.equal("revertField 1",            t.getI(k)(t.revertField(k, f.f1)(setab(k)(s))), (t.getP(k)(s).a, b))
//    ∧ E.equal("revertField 2",            t.getI(k)(t.revertField(k, f.f2)(setab(k)(s))), (a, t.getP(k)(s).b))
//    ) rename "SavedStore"

  }

  override def tests = Tests {
    "prop" - {
      val gen = RandomData.fieldReqTypeRules(Some(RandomData.reqTypeId), Some(Gen.chooseInt(4))).map(new Laws(_))
      gen.mustSatisfyE(_.eval)
    }
  }
}
