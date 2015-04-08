package shipreq.webapp.base.test

import scalaz._
import scalaz.std.AllInstances._
import scalaz.syntax.bind._
import scalaz.syntax.semigroup._
import shipreq.base.util.{NonEmptyVector, IMap, Vector1}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import ReqFieldData.{Implications, ImplicationsU}

object ProjectDSL {

  type S = ProjectState

  type Mod[A] = State[S, A]

  case class ProjectState(p             : Project,
                          nextId        : Long,
                          defaultReqType: ReqType,
                          reqs          : IMap[Req.Id, Req],
                          pubids        : Pubid.Register,
                          reqCodeTrie   : ReqCode.Trie,
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
    defaultReqType = p.customReqTypes.data.values.headOption.getOrElse(StaticReqType.values.head),
    reqs           = p.reqs.data.reqs,
    pubids         = p.reqs.data.pubids,
    reqCodeTrie    = p.reqCodes.data.trie,
    text           = p.reqFieldData.data.text,
    tags           = p.reqFieldData.data.tags,
    imps           = p.reqFieldData.data.implications.srcToTgt)

  def parseCode(s: String): ReqCode = {
    val ns = s.split('.').reverse.map(ReqCode.Node.apply)
    ReqCode(NonEmptyVector(ns.head, ns.tail.toVector))
  }

  
  case class GReq(desc   : Text.GenericReqDesc.OptionalText                            = Vector.empty,
                  id     : Option[GenericReq.Id]                                       = None,
                  reqType: Option[ReqType.Id]                                          = None,
                  alive  : Alive                                                       = Alive,
                  codes  : Set[String]                                                 = Set.empty,
                  tags   : Set[ApplicableTag.Id]                                       = Set.empty,
                  impSrcs: Set[Req.Id]                                                 = Set.empty,
                  impTgts: Set[Req.Id]                                                 = Set.empty,
                  cftexts: Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText] = Map.empty) {

    def code   (rcs: String*)                                                 = copy(codes   = this.codes   ++ rcs)
    def tag    (ids: ApplicableTag.Id*)                                       = copy(tags    = this.tags    ++ ids)
    def impSrc (ids: Req.Id*)                                                 = copy(impSrcs = this.impSrcs ++ ids)
    def impTgt (ids: Req.Id*)                                                 = copy(impTgts = this.impTgts ++ ids)
    def cftext (k: CustomField.Text.Id, v: Text.CustomTextField.NonEmptyText) = copy(cftexts = this.cftexts.updated(k,v))
    def cftextS(k: CustomField.Text.Id, s: String)                            = if (s.isEmpty) this else cftext(k, s)

    def times(n: Int): Composite =
      Stream.fill(n - 1)(this).foldLeft(autoCompositeGReq(this))(_ + _)

    def state: Mod[GenericReq] =
      State[S, GenericReq]{ p =>
        val id          = this.id getOrElse GenericReq.Id(p.nextId)
        val reqTypeId   = this.reqType.getOrElse(p.defaultReqType.reqTypeId)
        val (pr, pubid) = Pubid.alloc(id, reqTypeId, p.pubids)
        val req         = GenericReq(id, pubid, desc, alive)
        val text        = cftexts.mapValues(t => Map.empty[Req.Id, Text.CustomTextField.NonEmptyText].updated(id, t))
        val tags        = p.tags.addvs(id, this.tags)
        val imps        = p.imps.addks(impSrcs, id).addvs(id, impTgts)
        val codeTrie    = codes.map(parseCode).foldLeft(p.reqCodeTrie)(ReqCode.Trie.put(_, _)(id))
        val p2          = p.copy(nextId      = this.id.fold(id.value + 1)(_ => p.nextId),
                                 pubids      = pr,
                                 reqs        = p.reqs + req,
                                 reqCodeTrie = codeTrie,
                                 text        = p.text |+| text,
                                 tags        = tags,
                                 imps        = imps)
        (p2, req)
      }
  }

  case class Composite(ss: NonEmptyVector[Mod[_]], defaultReqType: Option[ReqType]) {

    def +(b: GReq): Composite =
      copy(ss = b.state +: ss)

    def state: Mod[Unit] = {
      var s = ss.whole.reduce((a, b) => b >> a).map(_ => ())
      for (rt <- defaultReqType)
        s = State.modify[S](_.copy(defaultReqType = rt)) >> s
      s
    }

    def shuffle: Composite = {
      val x = scala.util.Random.shuffle(ss.whole)
      copy(ss = NonEmptyVector(x.head, x.tail))
    }

    def setDefaultReqType(rt: ReqType): Composite =
      copy(defaultReqType = Some(rt))

    def !(p: Project): Project =
      state.exec(projectState(p)).done

    def !!(p: Project): Project =
      shuffle.!(p)
  }

  implicit def autoCompositeGReq(g: GReq) = Composite(NonEmptyVector(g.state), None)

  implicit def parseCTF(i: String): Text.CustomTextField.NonEmptyText = {
    if (i.isEmpty) sys.error("Text.CustomTextField can't be empty.") else NonEmptyVector(Text.CustomTextField.Literal(i))
  }

  implicit def parseGRD(i: String): Text.GenericReqDesc.OptionalText =
    if (i.isEmpty) Vector.empty else Vector1(Text.GenericReqDesc.Literal(i))
}
