package shipreq.webapp.base.event

import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.util.TypeclassDerivation._
import ApplyEventTestFns._
import DeletionAction._
import UnivEq.option
import Text.GenericReqTitle

case class ReqFull(req      : GenericReq,
                   tags     : Set[ApplicableTagId],
                   impliedBy: Set[ReqId],
                   implies  : Set[ReqId],
                   reqCodes : Set[ReqCode.Value])

object ReqFull {
  implicit val equality: UnivEq[ReqFull] = deriveUnivEq

  def extract(p: Project, id: GenericReqId): Option[ReqFull] =
    p.reqs.data.req(id).map{ r2 =>
      val r = r2 match {case x: GenericReq => x}
      val tags      = p.reqTags.data(id)
      val impliedBy = p.implications.data.tgtToSrc(id)
      val implies   = p.implications.data.srcToTgt(id)
      val reqCodes  = p.reqCodes.data.activeReqCodesByTarget(id)
      ReqFull(r, tags, impliedBy, implies, reqCodes)
    }
}

object GenericReqEventTest extends TestSuite {

  val mf: CustomReqTypeId = 100
  val createMF = {
    import CustomReqTypeGD._
    CreateCustomReqType(mf, nev(Mnemonic("MF"), Name("MajFea"), Imp(false)))
  }

  val at1: ApplicableTagId = 10
  val createAT1 = {
    import ApplicableTagGD._
    CreateApplicableTag(at1, nev(Name("AT #1"), Desc(None), Key("at-one")))
  }

  val tg1: TagGroupId = 20
  val createTG1 = {
    import TagGroupGD._
    CreateTagGroup(tg1, nev(Name("TG #1"), Desc(None), MutexChildren(false)))
  }

  implicit val init = InitialEvents(createMF, createAT1, createTG1)

  implicit class ProjectExt(private val p: Project) extends AnyVal {
    def @@(id: GenericReqId) = ReqFull.extract(p, id)
  }

  import GenericReqGD._

  implicit def autoNES[A, B: UnivEq](a: A)(implicit f: A => B) = NonEmptySet[B](f(a))

  val empty1 = CreateGenericReq(1, mf, emptyValues)
  val implied2 = CreateGenericReq(2, mf, emptyValues + ValueForImpSrcs(NonEmptySet(empty1.id)))

  def assertReq(p: Project, id: GenericReqId)(req      : GenericReq,
                                              tags     : Set[ApplicableTagId] = UnivEq.emptySet,
                                              impliedBy: Set[ReqId]           = UnivEq.emptySet,
                                              implies  : Set[ReqId]           = UnivEq.emptySet,
                                              reqCodes : Set[ReqCode.Value]   = UnivEq.emptySet): Unit =
    assertEq(p @@ id, Some(ReqFull(req, tags, impliedBy, implies, reqCodes)))

  override def tests = TestSuite {

    'create {
      'empty {
        val p = _assertPass(empty1)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live))
      }

      'title {
        val t = NonEmptyVector(GenericReqTitle.Literal("cool"))
        val p = _assertPass(empty1.copy(vs = nev(ValueForTitle(t))))
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), t.whole, Live))
      }

      'tags {
        val t = NonEmptySet(at1)
        val p = _assertPass(empty1.copy(vs = nev(ValueForTags(t))))
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), tags = t.whole)
      }

      'impSrc {
        val v = NonEmptySet[ReqId](empty1.id)
        val p = _assertPass(empty1, CreateGenericReq(5, mf, nev(ValueForImpSrcs(v))))
        assertReq(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), impliedBy = v.whole)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), implies = Set(5))
      }

      'impTgt {
        val v = NonEmptySet[ReqId](empty1.id)
        val p = _assertPass(empty1, CreateGenericReq(5, mf, nev(ValueForImpTgts(v))))
        assertReq(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), implies = v.whole)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), impliedBy = Set(5))
      }

      // reqCodes

      'badId           - List(0, -1).foreach(i => assertFail("id")(empty1.copy(id = i)))
      'idInUse         - assertFail("exists")(empty1, empty1)
      'reqTypeNotFound - assertFail("found")(empty1.copy(rt = 666))
      'reqTypeDead     - assertFail("live")(DeleteCustomReqType(mf, SoftDel), empty1)
      'tagNotFound     - assertFail("tag")(empty1.copy(vs = nev(ValueForTags(6.AT))))
      'tagIsGroup      - assertFail("tag")(empty1.copy(vs = nev(ValueForTags(tg1.value.AT))))
      // tagIsDead - allow it
      'impSrcNotFound     - assertFail("")(empty1.copy(vs = nev(ValueForImpSrcs(123))))
      'impTgtNotFound     - assertFail("")(empty1.copy(vs = nev(ValueForImpTgts(123))))
      'impSrcSelf         - assertFail("")(empty1.copy(vs = nev(ValueForImpSrcs(1))))
      'impTgtSelf         - assertFail("")(empty1.copy(vs = nev(ValueForImpTgts(1))))
      'impCycle           - assertFail("")(empty1, implied2, CreateGenericReq(3, mf, nev(ValueForImpSrcs(2), ValueForImpTgts(1))))
    }
  }
}
