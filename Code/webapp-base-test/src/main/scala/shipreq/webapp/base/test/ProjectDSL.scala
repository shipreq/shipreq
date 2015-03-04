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

  case class ProjectState(p: Project, nextId: Long, defaultReqType: ReqType,
                         reqs: IMap[Req.Id, Req], pubids: Pubid.Register,
                         text: ReqFieldData.Text, tags: ReqFieldData.Tags, imps: ImplicationsU) {
    def done: Project =
      p.copy(
        reqs         = succ(p.reqs,         Requirements(reqs, pubids)),
        reqFieldData = succ(p.reqFieldData, ReqFieldData(text, tags, Implications(imps))))
  }

  def projectState(p: Project) = ProjectState(p,
    nextId         = p.reqs.data.reqs.keySet.ifelse(_.isEmpty, _ => 1, _.max.value),
    defaultReqType = p.customReqTypes.data.values.headOption.getOrElse(StaticReqType.values.head),
    reqs           = p.reqs.data.reqs,
    pubids         = p.reqs.data.pubids,
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
  
  case class GReq(desc   : TextInput                           = defaultTextInput,
                  id     : Option[GenericReq.Id]               = None,
                  reqType: Option[ReqType.Id]                  = None,
                  alive  : Alive                               = Alive,
                  tags   : Set[ApplicableTag.Id]               = Set.empty,
                  impSrcs: Set[Req.Id]                         = Set.empty,
                  impTgts: Set[Req.Id]                         = Set.empty,
                  cftexts: Map[CustomField.Text.Id, TextInput] = Map.empty) {

    // TODO missing req codes

    def tag   (ids: ApplicableTag.Id*)                 = copy(tags    = this.tags    ++ ids)
    def impSrc(ids: Req.Id*)                           = copy(impSrcs = this.impSrcs ++ ids)
    def impTgt(ids: Req.Id*)                           = copy(impTgts = this.impTgts ++ ids)
    def cftext(kvs: (CustomField.Text.Id, TextInput)*) = copy(cftexts = this.cftexts ++ kvs)

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
        val p2          = p.copy(nextId = this.id.fold(id.value + 1)(_ => p.nextId),
                                 pubids = pr,
                                 reqs   = p.reqs + req,
                                 text   = text,
                                 tags   = tags,
                                 imps   = imps)
        (p2, req)
      }
  }

  private def succ[A](r: RevAnd[A], a: A): RevAnd[A] =
    RevAnd(r.rev.succ, a)

  case class Composite(ss: NonEmptyList[Mod[_]]) {

    def +(b: GReq): Composite =
      Composite(b.state <:: ss)

    def state: Mod[Unit] =
      ss.list.reduce((a,b) => b >> a).map(_ => ())

    def shuffle: Composite = {
      val x = scala.util.Random.shuffle(ss.list)
      Composite(NonEmptyList.nel(x.head, x.tail))
    }

    def !(p: Project): Project =
      state.exec(projectState(p)).done

    def !!(p: Project): Project =
      shuffle.!(p)
  }

  implicit def autoCompositeGReq(g: GReq) = Composite(NonEmptyList(g.state))
}
