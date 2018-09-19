package shipreq.webapp.base.data

import scalaz.\/-
import shipreq.webapp.base.text.Grammar
import utest._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ReqCode._
import DataValidators.{reqCode => V}
import shipreq.base.util.Invalid

object ReqCodeTest extends TestSuite {

  val sampleCodeTrie = Trie.empty
      .put("aa",     ActiveReq(1, 100, None, emptyReqInactive))
      .put("aa.b.c", ActiveReq(2, 100, None, emptyReqInactive))
      .put("aa.b.d", ActiveGroup(LiveCodeGroup(3, ∅), emptyReqInactive))
      .put("aa.b.e", Inactive(Some(DeadCodeGroup(1, "ah")), emptyReqInactive))

  val vs0 = V.State(sampleCodeTrie, Set.empty)
  val vs2 = V.State(sampleCodeTrie, Set("aa.b.c"))

  override def tests = Tests {

    'codeValidation {

      def testFail(vs: V.State, i: String): Unit = {
        val r = V.code(vs).validity(i)
        assert(r is Invalid)
      }

      def testPass(vs: V.State, i: String): Unit =
        testPass2(vs, i, i)

      def testPass2(vs: V.State, i: String, exp: Value): Unit = {
        val r = V.code(vs).unnamed(i)
        assertEq(r, \/-(exp))
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
