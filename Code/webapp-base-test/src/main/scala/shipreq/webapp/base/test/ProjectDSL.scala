package shipreq.webapp.base.test

import scalaz.{IMap => _, _}
import scalaz.std.AllInstances._
import scalaz.syntax.bind._
import scalaz.syntax.semigroup._
import shipreq.base.util._, MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import ReqFieldData.{Implications, ImplicationsU}

object ProjectDSL {

  type S = ProjectState

  type Mod[A] = State[S, A]

  trait ToState {
    def state: Mod[_]
  }

  case class ProjectState(p             : Project,
                          nextId        : Long,
                          defaultReqType: Option[CustomReqType],
                          reqs          : IMapK[ReqTypeId, ReqIdT, ReqT],
                          pubids        : PubidRegister,
                          reqCodeTrie   : ReqCode.Trie,
                          maxReqCodeId  : Long,
                          text          : ReqFieldData.Text,
                          tags          : ReqFieldData.Tags,
                          imps          : ImplicationsU) {
    def done: Project =
      p.copy(
        reqs         = succ(p.reqs,         Requirements(reqs, pubids)),
        reqCodes     = succ(p.reqCodes,     ReqCodes(reqCodeTrie)),
        reqFieldData = succ(p.reqFieldData, ReqFieldData(text, tags, Implications(imps))))
  }

  private def succ[A](r: RevAnd[A], a: A): RevAnd[A] =
    RevAnd(r.rev.succ, a)

  def projectState(p: Project) = ProjectState(p,
    nextId         = p.reqs.data.reqs.keySet.ifelse(_.isEmpty, _ => 1, _.max.value),
    defaultReqType = p.customReqTypes.data.values.headOption,
    reqs           = p.reqs.data.reqs,
    pubids         = p.reqs.data.pubids,
    reqCodeTrie    = p.reqCodes.data.trie,
    maxReqCodeId   = p.reqCodes.data.cataA(0L)((q,_,d) => q max d.id.value),
    text           = p.reqFieldData.data.text,
    tags           = p.reqFieldData.data.tags,
    imps           = p.reqFieldData.data.implications.srcToTgt)

  type CFTextId    = CustomField.Text.Id
  type CFTextValue = Text.CustomTextField.NonEmptyText

  case class GReq(title  : Text.GenericReqTitle.OptionalText = Vector.empty,
                  id     : Option[GenericReqId]              = None,
                  reqType: Option[CustomReqTypeId]           = None,
                  alive  : Alive                             = Alive,
                  codes  : Set[ReqCode.Value]                = Set.empty,
                  tags   : Set[ApplicableTagId]              = Set.empty,
                  impSrcs: Set[ReqId]                        = Set.empty,
                  impTgts: Set[ReqId]                        = Set.empty,
                  cftexts: Map[CFTextId, CFTextValue]        = Map.empty) extends ToState {

    def code   (rcs: ReqCode.Value*)         = copy(codes   = this.codes   ++ rcs)
    def tag    (ids: ApplicableTagId*)       = copy(tags    = this.tags    ++ ids)
    def impSrc (ids: ReqId*)                 = copy(impSrcs = this.impSrcs ++ ids)
    def impTgt (ids: ReqId*)                 = copy(impTgts = this.impTgts ++ ids)
    def cftext (k: CFTextId, v: CFTextValue) = copy(cftexts = this.cftexts.updated(k,v))
    def cftextS(k: CFTextId, s: String)      = if (s.isEmpty) this else cftext(k, s)

    def times(n: Int): Composite =
      Stream.fill(n - 1)(this).foldLeft(autoComposite(this))(_ + _)

    def state: Mod[GenericReq] =
      State[S, GenericReq]{ p =>
        val id = this.id getOrElse GenericReqId(p.nextId)

        var maxReqCodeId = p.maxReqCodeId
        def nextReqCodeId() = {
          maxReqCodeId += 1
          ReqCodeId(maxReqCodeId)
        }
        def reqCodeData() =
          ReqCode.Data(Some(ReqCode.ActiveData(nextReqCodeId(), id)), UnivEq.emptySet, UnivEq.emptyMultimap)

        val reqTypeId   = this.reqType.getOrElse(p.defaultReqType.get.id)
        val (pr, pubid) = p.pubids.alloc(id, reqTypeId)
        val req         = GenericReq(id, pubid, title, alive)
        val text        = cftexts.mapValues(t => Map.empty[ReqId, CFTextValue].updated(id, t))
        val tags        = p.tags.addvs(id, this.tags)
        val imps        = p.imps.addks(impSrcs, id).addvs(id, impTgts)
        val codeTrie    = codes.foldLeft(p.reqCodeTrie)((t, c) => t.put(c, reqCodeData()))
        val p2          = p.copy(nextId       = this.id.fold(id.value + 1)(_ => p.nextId),
                                 pubids       = pr,
                                 reqs         = p.reqs + req,
                                 reqCodeTrie  = codeTrie,
                                 maxReqCodeId = maxReqCodeId,
                                 text         = p.text |+| text,
                                 tags         = tags,
                                 imps         = imps)
        (p2, req)
      }
  }

  case class RCGroup(code : ReqCode.Value,
                     title: Text.ReqCodeGroupTitle.OptionalText = Vector.empty) extends ToState {
    def state: Mod[ReqCodeGroup] =
      State[S, ReqCodeGroup]{ p =>

        var maxReqCodeId = p.maxReqCodeId
        def nextReqCodeId() = {
          maxReqCodeId += 1
          ReqCodeId(maxReqCodeId)
        }

        val g  = ReqCodeGroup(title)
        val ad = ReqCode.ActiveData(nextReqCodeId(), g)
        val d  = ReqCode.Data(Some(ad), UnivEq.emptySet, UnivEq.emptyMultimap)
        val t  = p.reqCodeTrie.put(code, d)
        val p2 = p.copy(reqCodeTrie = t, maxReqCodeId = maxReqCodeId)
        (p2, g)
      }
  }

  case class Composite(ss: NonEmptyVector[Mod[_]], defaultReqType: Option[CustomReqType]) {

    def +(n: ToState): Composite =
      copy(ss = n.state +: ss)

    def state: Mod[Unit] = {
      var s = ss.whole.reduce((a, b) => b >> a).map(_ => ())
      for (rt <- defaultReqType)
        s = State.modify[S](_.copy(defaultReqType = Some(rt))) >> s
      s
    }

    def shuffle: Composite = {
      val x = scala.util.Random.shuffle(ss.whole)
      copy(ss = NonEmptyVector(x.head, x.tail))
    }

    def setDefaultReqType(rt: CustomReqType): Composite =
      copy(defaultReqType = Some(rt))

    def !(p: Project): Project =
      state.exec(projectState(p)).done

    def !!(p: Project): Project =
      shuffle.!(p)
  }

  implicit def autoComposite(s: ToState) = Composite(NonEmptyVector.one(s.state), None)

  implicit def parseCTF(i: String): Text.CustomTextField.NonEmptyText = {
    if (i.isEmpty) sys.error("Text.CustomTextField can't be empty.") else NonEmptyVector(Text.CustomTextField.Literal(i))
  }

  implicit def parseGRD(i: String): Text.GenericReqTitle.OptionalText =
    if (i.isEmpty) Vector.empty else Vector1(Text.GenericReqTitle.Literal(i))
}
