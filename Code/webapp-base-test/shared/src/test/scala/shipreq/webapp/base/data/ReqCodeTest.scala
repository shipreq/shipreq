package shipreq.webapp.base.data

import nyaya.util.Multimap
import shipreq.webapp.base.text.Grammar
import utest._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ReqCode._
import Validators.reqCode._

object ReqCodeTest extends TestSuite {

  val sampleCodeTrie = Trie.empty
      .put("aa",     ActiveReq(1, 100, None, emptyReqInactive))
      .put("aa.b.c", ActiveReq(2, 100, None, emptyReqInactive))
      .put("aa.b.d", ActiveGroup(LiveReqCodeGroup(3, ∅), emptyReqInactive))
      .put("aa.b.e", Inactive(Some(DeadReqCodeGroup(1, "ah")), emptyReqInactive))

  val vs0 = VS(sampleCodeTrie, Set.empty)
  val vs2 = VS(sampleCodeTrie, Set("aa.b.c"))

  override def tests = TestSuite {

    'codeValidation {

      def testFail(vs: VS, i: String): Unit = {
        val r = code.correctAndValidate(vs, i)
        assert(r.isFailure)
      }

      def testPass(vs: VS, i: String): Unit =
        testPass2(vs, i, i)

      def testPass2(vs: VS, i: String, exp: Value): Unit = {
        val r = code.correctAndValidate(vs, i)
        assert(r.isSuccess)
        assertEq(r.toOption.get, exp)
      }

      'simple {
        testPass(vs0, "b")
        testPass(vs0, "1")
        testPass(vs0, "qweas_123")
        testFail(vs0, "_")
        testFail(vs0, "_a")
        testPass(vs0, "a_")
      }

      'mkLowercase -
        testPass2(vs0, "AB.CD", "ab.cd")

      'squashUnderscores {
        testPass2(vs0, "ab__", "ab_")
        testPass2(vs0, "ab__2", "ab_2")
        testPass2(vs0, "ab___.c__2", "ab_.c_2")
      }

      'freeMiddleNode -
        testPass(vs0, "aa.b")

      'maxNodes -
        testPass(vs0, List.fill(Grammar.reqCode.maxNodes)("x").mkString("."))

      'idempotent -
        testPass(vs2, "aa.b.c")

      'tombstone -
        testPass(vs0, "aa.b.e")

      'empty -
        testFail(vs0, "")

      'tooManyNodes -
        testFail(vs0, List.fill(Grammar.reqCode.maxNodes + 1)("x").mkString("."))

      'unique {
        testFail(vs0, "aa")
        testFail(vs0, "aa.b.c")
        testFail(vs0, "aa.b.d")
      }
    }
  }
}
