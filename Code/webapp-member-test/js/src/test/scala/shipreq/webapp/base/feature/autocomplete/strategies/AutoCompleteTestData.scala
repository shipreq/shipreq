package shipreq.webapp.base.feature.autocomplete.strategies

import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.member.text.{PlainText, TextSearch}

object AutoCompleteTestData {

  lazy val fakeTrie: ReqCode.Trie = {
    import shipreq.webapp.base.test.UnsafeTypes._

    val codes = Set[ReqCode.Value](
      "aaaa1", "abc", "amp", "apple", "apply",
      "abc.around.1", "abc.around.2", "abc.around.tbc", "abc.around.torn", "abc.around.now",
      "abc.art", "abc.aqua", "abc.bark",
      "baa", "bcd", "c", "cant", "eggs", "1", "2a", "2b",
      "bcd.aaaz", "shit.eggs", "goat.damn.egg.stuff", "goat.damn.egg.crap", "goat.damn.egglike"
    )

    val nextApReqCodeId: () => ApReqCodeId = {
      var v = 0
      () => { v += 1; ApReqCodeId(v)}
    }
    def nextReqCodeGroupId(): ReqCodeGroupId = {
      val id = nextApReqCodeId().value
      ReqCodeGroupId(id)
    }
    def tgt: ReqCode.Data = ReqCode.ActiveReq(nextApReqCodeId(), 1, None, ReqCode.emptyReqInactive)
    val t1 = codes.foldLeft(ReqCode.Trie.empty)((t, c) => t.put(c, tgt))

    def tomb = ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(nextReqCodeGroupId(), "asdf")))
    val tombCodes = Set[ReqCode.Value](
      "apple.dead", "ahhdead", "dead.eggs"
    )
    tombCodes.foldLeft(t1)((t, c) => t.put(c, tomb))
  }

  lazy val project2 = {
    import ProjectDsl._
    import UnsafeTypes._
    val p = Project.reqCodes.set(ReqCodes(fakeTrie))(SampleProject2.project)
    (DeadReqCode("dead.ref", oldReqId = 1, id = Some(ApReqCodeId(90))) +
      DeadReqCode("dead.group", id = Some(ReqCodeGroupId(91)))) ! p
  }

  lazy val plainText2 = PlainText.ForProject.noCtx(project2)
  lazy val textSearch2 = TextSearch(project2, plainText2)

}
