package shipreq.webapp.base.test

import scalaz._
import scalaz.syntax.bind._
import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import ReqFieldData.{Implications, ImplicationsU}

object ProjectDSL {

  type S = ProjectState

  type Mod[A] = State[S, A]

  type TextInput = String
  val defaultTextInput: TextInput = ""

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

  def modTextData(d: ReqFieldData.Text, k: CustomField.Text.Id, f: EndoFn[Map[Req.Id, Text.CustomTextField.OptionalText]]) =
    d.updated(k, f(d.getOrElse(k, Map.empty)))
  
  def addTextData(d: ReqFieldData.Text, r: Req.Id, n: Map[CustomField.Text.Id, TextInput]) =
    n.foldLeft(d) {
      case (d2, (k, v)) => modTextData(d2, k, _.updated(r, parseCTF(v)))
    }

  def parseCTF(i: TextInput): Text.CustomTextField.OptionalText =
    if (i.isEmpty) Nil else Text.CustomTextField.Literal(i) :: Nil

  def parseGRD(i: TextInput): Text.GenericReqDesc.OptionalText =
    if (i.isEmpty) Nil else Text.GenericReqDesc.Literal(i) :: Nil

  def parseCode(s: String): ReqCode = {
    val ns = s.split('.').reverse.map(ReqCode.Node.apply)
    ReqCode(NonEmptyList.nel(ns.head, ns.tail.toList))
  }

  
  case class GReq(desc   : TextInput                           = defaultTextInput,
                  id     : Option[GenericReq.Id]               = None,
                  reqType: Option[ReqType.Id]                  = None,
                  alive  : Alive                               = Alive,
                  codes  : Set[String]                         = Set.empty,
                  tags   : Set[ApplicableTag.Id]               = Set.empty,
                  impSrcs: Set[Req.Id]                         = Set.empty,
                  impTgts: Set[Req.Id]                         = Set.empty,
                  cftexts: Map[CustomField.Text.Id, TextInput] = Map.empty) {

    def code  (rcs: String*)                           = copy(codes   = this.codes   ++ rcs)
    def tag   (ids: ApplicableTag.Id*)                 = copy(tags    = this.tags    ++ ids)
    def impSrc(ids: Req.Id*)                           = copy(impSrcs = this.impSrcs ++ ids)
    def impTgt(ids: Req.Id*)                           = copy(impTgts = this.impTgts ++ ids)
    def cftext(kvs: (CustomField.Text.Id, TextInput)*) = copy(cftexts = this.cftexts ++ kvs)

    def times(n: Int): Composite =
      Stream.fill(n - 1)(this).foldLeft(autoCompositeGReq(this))(_ + _)

    def state: Mod[GenericReq] =
      State[S, GenericReq]{ p =>
        val id          = this.id getOrElse GenericReq.Id(p.nextId)
        val reqTypeId   = this.reqType.getOrElse(p.defaultReqType.reqTypeId)
        val (pr, pubid) = Pubid.alloc(id, reqTypeId, p.pubids)
        val desc        = parseGRD(this.desc)
        val req         = GenericReq(id, pubid, desc, alive)
        val text        = addTextData(p.text, id, cftexts)
        val tags        = p.tags.addvs(id, this.tags)
        val imps        = p.imps.addks(impSrcs, id).addvs(id, impTgts)
        val codeTrie    = codes.map(parseCode).foldLeft(p.reqCodeTrie)(ReqCode.Trie.put(_, _)(id))
        val p2          = p.copy(nextId      = this.id.fold(id.value + 1)(_ => p.nextId),
                                 pubids      = pr,
                                 reqs        = p.reqs + req,
                                 reqCodeTrie = codeTrie,
                                 text        = text,
                                 tags        = tags,
                                 imps        = imps)
        (p2, req)
      }
  }

  case class Composite(ss: NonEmptyList[Mod[_]], defaultReqType: Option[ReqType]) {

    def +(b: GReq): Composite =
      copy(ss = b.state <:: ss)

    def state: Mod[Unit] = {
      var s = ss.list.reduce((a, b) => b >> a).map(_ => ())
      for (rt <- defaultReqType)
        s = State.modify[S](_.copy(defaultReqType = rt)) >> s
      s
    }

    def shuffle: Composite = {
      val x = scala.util.Random.shuffle(ss.list)
      copy(ss = NonEmptyList.nel(x.head, x.tail))
    }

    def setDefaultReqType(rt: ReqType): Composite =
      copy(defaultReqType = Some(rt))

    def !(p: Project): Project =
      state.exec(projectState(p)).done

    def !!(p: Project): Project =
      shuffle.!(p)
  }

  implicit def autoCompositeGReq(g: GReq) = Composite(NonEmptyList(g.state), None)
}
