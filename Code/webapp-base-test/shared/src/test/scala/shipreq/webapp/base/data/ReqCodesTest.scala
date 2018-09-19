package shipreq.webapp.base.data

import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import scalaz.std.AllFunctions._
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.MTrie, MTrie.Ops
import shipreq.webapp.base.RandomData

/*
// TODO ReqCodesTest disabled
object ReqCodesTest extends TestSuite {
  import ReqCode._

  case class TrieProps(trie: Trie, data: Data, code: ReqCode.Value) {
    val E          = EvalOver(this)
    val flat       = trie.flattenTrie
    val flatStream = trie.flatStream

    def put = {
      val a = flat.updated(code, data)
      val n = trie.put(code, data).flattenTrie
      E.equal("put", a, n)
    }

    def createFromFlatten = {
      val n = flat.foldLeft(emptyTrie) { case (q, (c, t)) => q.put(c, t) }
      E.equal("createFromFlatten", trie, n)
    }

    def flattenEqualsFlatStream =
      E.equal("flatten = flatStream.toMap", flat, flatStream.toMap)

    def all = "Trie props" rename_: (
      flattenEqualsFlatStream ∧ put ∧ createFromFlatten)
  }

  def gen: Gen[TrieProps] =
    for {
      targets ← RandomData.reqId.list.sup
      trie    ← RandomData.reqCode.trie(Gen oneofO targets).lim(10)
      target  ← Gen.newOrOld(RandomData.reqId)(targets) // add SHRs
      maxId   = ReqCodeId(trie.cataV(0L)((q,_,d) => (q #:: d.ids.map(_.value)).max))
      code    ← Gen.newOrOld(RandomData.reqCode.value)(trie.flatStream.map(_._1))
    } yield TrieProps(trie, Data(maxId + 1, target), code)

  override def tests = Tests {
    gen.mustSatisfyE(_.all)
  }
}
*/