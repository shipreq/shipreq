package shipreq.webapp.base.data

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import scalaz.std.AllFunctions._
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.MTrie, MTrie.Ops
import shipreq.webapp.base.RandomData

object ReqCodesTest extends TestSuite {
  import ReqCode._

  case class TrieProps(trie: Trie, target: Target, code: ReqCode) {
    val E          = EvalOver(this)
    val flat       = trie.flattenTrie
    val flatStream = trie.flatStream

    def put = {
      val a = flat.updated(code.code, target)
      val n = trie.put(code.code, target).flattenTrie
      E.equal("put", a, n)
    }

    def createFromFlatten = {
      val n = flat.foldLeft(emptyTrie) { case (q, (c, t)) => q.put(c, t) }
      E.equal("createFromFlatten", trie, n)
    }

    def flattenEqualsFlatStream =
      E.equal("flatten = flatStream.toMap", flat, flatStream.toMap)

    def all = "Trie props" rename_: (
      flattenEqualsFlatStream ∧ (put ==> createFromFlatten))
  }

  def gen: Gen[TrieProps] =
    for {
      targets ← RandomData.reqId.set.sup
      trie    ← RandomData.reqCodeTrie(targets.toSeq).lim(10)
      target  ← Gen.newOrOld(RandomData.reqId)(targets)
      code    ← Gen.newOrOld(RandomData.reqCode)(trie.flatStream.map(_._1 |> ReqCode.apply))
    } yield TrieProps(trie, target, code)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all)
  }
}