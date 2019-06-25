package shipreq.webapp.base.test

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.prop._
import scalaz.{IMap => _, _}
import scalaz.std.AllInstances._
import scalaz.syntax.bind._
import scalaz.syntax.semigroup._
import shipreq.base.util._
import MTrie.Ops
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
                          imps          : Implications.UniDir) {

    private var _newMaxReqCodeId = maxReqCodeId

    def newMaxReqCodeId = _newMaxReqCodeId

    def nextReqCodeGroupId() = {
      _newMaxReqCodeId += 1
      ReqCodeGroupId(newMaxReqCodeId)
    }

    def nextApReqCodeId() = {
      _newMaxReqCodeId += 1
      ApReqCodeId(newMaxReqCodeId)
    }

    def newActiveGroup(id: Option[ReqCodeGroupId], t: Text.CodeGroupTitle.OptionalText) =
      ReqCode.ActiveGroup(
        LiveCodeGroup(id getOrElse nextReqCodeGroupId(), t),
        ReqCode.emptyReqInactive)

    def assignReqCodeToReq(t: ReqCode.Trie, c: ReqCode.Value, id: Option[ApReqCodeId], reqId: ReqId, reqLive: Live): ReqCode.Trie = {
      import ReqCode._
      def rcid() = id getOrElse nextApReqCodeId()
      reqLive match {
        case Live => t.modify(c)(e => ActiveReq(rcid(), reqId, e.flatMap(_.deadGroup), e.fold(emptyReqInactive)(_.reqInactive)))
        case Dead => t.modify(c)(_.getOrElse(Data.empty).modReqInactive(_.add(reqId, rcid())))
      }
    }

    def done: Project =
      IdCeilings.supply { ids =>
        var f = Project.idCeilings set ids
        f = f compose Project.reqs        .modify(r => succ(r, Requirements(reqs, r.useCases, pubids)))
        f = f compose Project.reqCodes    .modify(succ(_, ReqCodes(reqCodeTrie)))
        f = f compose Project.reqText     .modify(succ(_, text))
        f = f compose Project.reqTags     .modify(succ(_, tags))
        f = f compose Project.implications.modify(succ(_, Implications.BiDir(imps)))
        f(p)
      }
  }

  private def succ[A](old: A, n: A) = n // obsolete. Used to increse Rev

  def projectState(p: Project) = ProjectState(p,
    nextId         = p.content.reqs.idIterator.ifelse(_.isEmpty, _ => 1, _.max.value),
    defaultReqType = p.config.reqTypes.custom.values.headOption.map(_.id),
    reqs           = p.content.reqs.genericReqs,
    pubids         = p.content.reqs.pubids,
    reqCodeTrie    = p.content.reqCodes.trie,
    maxReqCodeId   = p.content.reqCodes.idList match {case Nil => 0; case l => l.iterator.map(_.value).max},
    text           = p.content.reqText,
    tags           = p.content.reqTags,
    imps           = p.content.implications.forwards)

  type CFTextId     = CustomField.Text.Id
  type CFTextValue  = Text.CustomTextField.NonEmptyText
  type CFTextValueO = Text.CustomTextField.OptionalText

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

    def !(p: Project): Project = {
      val p2 = state.exec(projectState(p)).done
      DataProp.project.allIncludingConfig assert p2
      p2
    }

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
    def cftextS(k: CFTextId, s: String)      : GReq = if (s.isEmpty) this else {import UnsafeTypes._ ;cftext(k, s)}

    def times(n: Int): Composite =
      Stream.fill(n - 1)(this).foldLeft(this: Composite)(_ + _)

    def state: Mod[GenericReq] =
      State[ProjectState, GenericReq]{ p =>
        val id = this.id getOrElse GenericReqId(p.nextId)

        val reqTypeId   = this.reqType.getOrElse(p.defaultReqType.get)
        val (pr, pubid) = p.pubids.allocC(reqTypeId)(id)
        val req         = GenericReq(id, pubid, title, live)
        val text        = cftexts.mapValuesNow(t => Map.empty[ReqId, CFTextValue].updated(id, t))
        val tags        = p.tags.addvs(id, this.tags)
        val imps        = p.imps.addks(impSrcs, id).addvs(id, impTgts)
        val codeTrie    = codes.foldLeft(p.reqCodeTrie)((t, c) => p.assignReqCodeToReq(t, c, None, id, live))
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
                     id   : Option[ReqCodeGroupId] = None,
                     title: Text.CodeGroupTitle.OptionalText = Vector.empty) extends ToState {
    def state: Mod[LiveCodeGroup] =
      State[ProjectState, LiveCodeGroup]{ p =>
        val ad = p.newActiveGroup(id, title)
        val t  = p.reqCodeTrie.modify(code)(_.fold(ad)(old => ad.copy(reqInactive = old.reqInactive)))
        val p2 = p.copy(reqCodeTrie = t, maxReqCodeId = p.newMaxReqCodeId)
        (p2, ad.group)
      }
  }

  /**
   * Adds a dead ReqCode.
   *
   * If `oldReqId` is defined, the ReqCode used to belong to a requirement, else a dead group will be added.
   */
  case class DeadReqCode(code    : ReqCode.Value,
                         id      : Option[ReqCodeId] = None,
                         oldReqId: Option[ReqId] = None,
                         title   : String = "dead group") extends ToState {
    def state: Mod[Unit] =
      State[ProjectState, Unit]{ p =>
        import UnsafeTypes._
        val t = p.reqCodeTrie.modify(code) { o =>
          val d = o.getOrElse(ReqCode.Data.empty)
          oldReqId match {
            case None =>
              val id = this.id.fold(p.nextReqCodeGroupId())(_.asInstanceOf[ReqCodeGroupId])
              TestOptics.reqCodeDataDeadGroup.set(Some(DeadCodeGroup(id, title)))(d)
            case Some(r) =>
              val id = this.id.fold(p.nextApReqCodeId())(_.asInstanceOf[ApReqCodeId])
              TestOptics.reqCodeDataReqInactive.modify(_.add(r, id))(d)
          }
        }
        val p2 = p.copy(reqCodeTrie = t, maxReqCodeId = p.newMaxReqCodeId)
        (p2, ())
      }
  }

  def reqTitleTagRefs   = new MrTagRef(Text.GenericReqTitle)
  def customTextTagRefs = new MrTagRef(Text.CustomTextField)
}
