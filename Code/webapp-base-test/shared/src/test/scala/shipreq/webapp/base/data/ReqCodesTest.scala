package shipreq.webapp.base.data

import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test.PropTest._
import utest._
import shipreq.base.util.MTrie, MTrie.Ops
import shipreq.base.util.univeq._
import shipreq.webapp.base.RandomData

object ReqCodesTest extends TestSuite {
  import ReqCode._

  final private case class TrieProps(trie: Trie, data: Data, code: ReqCode.Value) {
    val E          = EvalOver(this)
    val flat       = trie.flattenTrie
    val flatStream = trie.flatIterator().toList

    def put = {
      val a = flat.updated(code, data)
      val n = trie.put(code, data).flattenTrie
      E.equal("put", a, n)
    }

    def createFromFlatten = {
      val n = flat.foldLeft(ReqCode.Trie.empty) { case (q, (c, t)) => q.put(c, t) }
      E.equal("createFromFlatten", trie, n)
    }

    def flattenEqualsFlatStream =
      E.equal("flatten = flatStream.toMap", flat, flatStream.toMap)

    def all = "Trie props" rename_: (
      flattenEqualsFlatStream ∧ put ∧ createFromFlatten)
  }

  private def gen: Gen[TrieProps] = {
    val someGenReqId = Some(RandomData.reqId)
    val genData      = RandomData.reqCode.data(someGenReqId, someGenReqId)(0 to 3)
    for {
      trie <- RandomData.reqCode.trie(genData, 3)
      data <- Gen.newOrOld(genData, trie.allValues)
      code <- Gen.newOrOld(RandomData.reqCode.value, trie.flatIterator().map(_._1))
    } yield TrieProps(trie, data, code)
  }

  override def tests = Tests {
    "props" - gen.mustSatisfyE(_.all)
  }
}
