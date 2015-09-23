package shipreq.webapp.base.test

import scalaz.{IMap => _, _}
import scalaz.std.AllInstances._
import scalaz.syntax.bind._
import scalaz.syntax.semigroup._
import shipreq.base.util._, MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Text}

object ProjectDslInternals {
  type Mod[A] = State[ProjectState, A]

  trait ToState {
    def state: Mod[_]
  }

  case class ProjectState(p             : Project,
                          nextId        : Int,
                          defaultReqType: Option[CustomReqTypeId],
                          reqs          : GenericReqIMap,
                          pubids        : PubidRegister,
                          reqCodeTrie   : ReqCode.Trie,
                          maxReqCodeId  : Int,
                          text          : ReqData.Text,
                          tags          : ReqData.Tags,
                          imps          : Implications.Uni) {

    private var _newMaxReqCodeId = maxReqCodeId

    def newMaxReqCodeId = _newMaxReqCodeId

    def nextReqCodeId() = {
      _newMaxReqCodeId += 1
      ReqCodeId(newMaxReqCodeId)
    }

    def done: Project =
      IdCeilings.supply(ids =>
        p.copy(
          reqs         = succ(p.reqs,         Requirements(reqs, pubids)),
          reqCodes     = succ(p.reqCodes,     ReqCodes(reqCodeTrie)),
          reqText      = succ(p.reqText,      text),
          reqTags      = succ(p.reqTags,      tags),
          implications = succ(p.implications, Implications(imps)),
          idCeilings   = ids))
  }

  private def succ[A](old: A, n: A) = n // obsolete. Used to increse Rev

  def projectState(p: Project) = ProjectState(p,
    nextId         = p.reqs.reqs.keySet.ifelse(_.isEmpty, _ => 1, _.max.value),
    defaultReqType = p.config.customReqTypes.values.headOption.map(_.id),
    reqs           = p.reqs.genericReqs,
    pubids         = p.reqs.pubids,
    reqCodeTrie    = p.reqCodes.trie,
    maxReqCodeId   = p.reqCodes.cataA(0)((q,_,d) => q max d.id.value),
    text           = p.reqText,
    tags           = p.reqTags,
    imps           = p.implications.srcToTgt)

  type CFTextId     = CustomField.Text.Id
  type CFTextValue  = Text.CustomTextField.NonEmptyText
  type CFTextValueO = Text.CustomTextField.OptionalText

  val emptyReqCodeData = ReqCode.Data(None, UnivEq.emptySet, UnivEq.emptySetMultimap)

  case class Composite(ss: NonEmptyVector[Mod[_]], defaultReqType: Option[CustomReqTypeId]) extends ToState {

    def +(n: ToState): Composite =
      copy(ss = n.state +: ss)

    def state: Mod[Unit] = {
      var s = ss.whole.reduce((a, b) => b >> a).map(_ => ())
      for (rt <- defaultReqType) {
        val s1 = s
        s = State.modify[ProjectState](_.copy(defaultReqType = Some(rt))) >> s1
      }
      s
    }

    def shuffle: Composite = {
      val x = scala.util.Random.shuffle(ss.whole)
      copy(ss = NonEmptyVector(x.head, x.tail))
    }

    def defaultReqType(rt: CustomReqTypeId): Composite =
      copy(defaultReqType = Some(rt))

    def !(p: Project): Project =
      state.exec(projectState(p)).done

    def !!(p: Project): Project =
      shuffle.!(p)
  }

  class MrTagRef[T <: Atom.TagRef](val t: T) extends AnyVal {
    def apply(ids: Seq[ApplicableTagId])                  : t.OptionalText = ids.toVector map t.TagRef
    def apply(id1: ApplicableTagId, ids: ApplicableTagId*): t.NonEmptyText = apply(NonEmptyVector(id1, ids.toVector))
    def apply(ids: NonEmptyVector[ApplicableTagId])       : t.NonEmptyText = ids map t.TagRef
  }
}

// =====================================================================================================================
object ProjectDsl {
  import ProjectDslInternals._

  implicit def projectDsl_autoComposite(s: ToState): Composite =
    Composite(NonEmptyVector.one(s.state), None)

  case class GReq(title  : Text.GenericReqTitle.OptionalText = Vector.empty,
                  id     : Option[GenericReqId]              = None,
                  reqType: Option[CustomReqTypeId]           = None,
                  live   : Live                              = Live,
                  codes  : Set[ReqCode.Value]                = Set.empty,
                  tags   : Set[ApplicableTagId]              = Set.empty,
                  impSrcs: Set[ReqId]                        = Set.empty,
                  impTgts: Set[ReqId]                        = Set.empty,
                  cftexts: Map[CFTextId, CFTextValue]        = Map.empty) extends ToState {

    def code   (rcs: ReqCode.Value*)         : GReq = copy(codes   = this.codes   ++ rcs)
    def tag    (ids: ApplicableTagId*)       : GReq = copy(tags    = this.tags    ++ ids)
    def impSrc (ids: ReqId*)                 : GReq = copy(impSrcs = this.impSrcs ++ ids)
    def impTgt (ids: ReqId*)                 : GReq = copy(impTgts = this.impTgts ++ ids)
    def cftext (k: CFTextId, v: CFTextValue) : GReq = copy(cftexts = this.cftexts.updated(k,v))
    def cftextO(k: CFTextId, v: CFTextValueO): GReq = NonEmptyVector.maybe(v, this)(cftext(k, _))
    def cftextS(k: CFTextId, s: String)      : GReq = if (s.isEmpty) this else cftext(k, s)

    def times(n: Int): Composite =
      Stream.fill(n - 1)(this).foldLeft(this: Composite)(_ + _)

    def state: Mod[GenericReq] =
      State[ProjectState, GenericReq]{ p =>
        val id = this.id getOrElse GenericReqId(p.nextId)

        def reqCodeData() =
          ReqCode.Data(Some(ReqCode.ActiveData(p.nextReqCodeId(), id)), UnivEq.emptySet, UnivEq.emptySetMultimap)

        val reqTypeId   = this.reqType.getOrElse(p.defaultReqType.get)
        val (pr, pubid) = p.pubids.allocC(reqTypeId)(id)
        val req         = GenericReq(id, pubid, title, live)
        val text        = cftexts.mapValuesNow(t => Map.empty[ReqId, CFTextValue].updated(id, t))
        val tags        = p.tags.addvs(id, this.tags)
        val imps        = p.imps.addks(impSrcs, id).addvs(id, impTgts)
        val codeTrie    = codes.foldLeft(p.reqCodeTrie)((t, c) => t.put(c, reqCodeData()))
        val p2          = p.copy(nextId       = this.id.fold(id.value + 1)(_ => p.nextId),
                                 pubids       = pr,
                                 reqs         = p.reqs + req,
                                 reqCodeTrie  = codeTrie,
                                 maxReqCodeId = p.newMaxReqCodeId,
                                 text         = p.text |+| text,
                                 tags         = tags,
                                 imps         = imps)
        (p2, req)
      }
  }

  case class RCGroup(code : ReqCode.Value,
                     title: Text.ReqCodeGroupTitle.OptionalText = Vector.empty) extends ToState {
    def state: Mod[ReqCodeGroup] =
      State[ProjectState, ReqCodeGroup]{ p =>
        val g  = ReqCodeGroup(title)
        val ad = ReqCode.ActiveData(p.nextReqCodeId(), g)
        val t  = p.reqCodeTrie.modify(code)(_.getOrElse(emptyReqCodeData).copy(active = Some(ad)))
        val p2 = p.copy(reqCodeTrie = t, maxReqCodeId = p.newMaxReqCodeId)
        (p2, g)
      }
  }

  case class DeadReqCode(code : ReqCode.Value,
                         id: Option[ReqCodeId] = None,
                         target: Option[ReqId] = None) extends ToState {
    def state: Mod[ReqCodeId] =
      State[ProjectState, ReqCodeId]{ p =>
        val id = this.id getOrElse p.nextReqCodeId()
        val t = p.reqCodeTrie.modify(code) { o =>
          val d = o.getOrElse(emptyReqCodeData) //.copy(active = Some(ReqCode.ActiveData(p.nextReqCodeId(), ReqCodeGroup(Vector.empty))))
          target match {
            case None      => d.copy(refsToGroup = d.refsToGroup + id)
            case Some(tgt) => d.copy(reqInactive = d.reqInactive.add(tgt, id))
          }
        }
        val p2 = p.copy(reqCodeTrie = t, maxReqCodeId = p.newMaxReqCodeId)
        (p2, id)
      }
  }

  implicit def projectDsl_parseCTF(i: String): Text.CustomTextField.NonEmptyText = {
    if (i.isEmpty) sys.error("Text.CustomTextField can't be empty.") else NonEmptyVector(Text.CustomTextField.Literal(i))
  }

  implicit def projectDsl_parseGRD(i: String): Text.GenericReqTitle.OptionalText =
    if (i.isEmpty) Vector.empty else Vector1(Text.GenericReqTitle.Literal(i))

  def reqTitleTagRefs   = new MrTagRef(Text.GenericReqTitle)
  def customTextTagRefs = new MrTagRef(Text.CustomTextField)
}
