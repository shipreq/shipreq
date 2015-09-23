package shipreq.webapp.base.event

import japgolly.nyaya.util.Multimap
import scala.annotation.tailrec
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.text.{Grammar, Text}
import ApplyEventLib._, SE.SE
import DataImplicits._
import MTrie.Ops

trait ApplyContentEvent extends ApplyConfigEvent {

  object ReqEvents {

    private val imap = IMapStore(Project.genericReqs)

    private val grLiveExplicitly = LiveAccessor(GenericReq.liveExplicitly)(_.id.toString)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.req)

    def ensureLiveReqId(reqId: ReqId): SE[Unit] =
      whenUntrusted(
        needReq(reqId) >>= ensureLiveReq)

    def ensureLiveReq(req: Req): SE[Unit] =
      whenUntrusted(
        SE.test(p => req.live(p.config.customReqTypes) :: Live, s"${show(req)} is dead."))

    def ensureLiveTextFieldId(id: CustomField.Text.Id): SE[Unit] =
      whenUntrusted(
        SE.getE(_.config.customFieldAttempt(id)) >>= ensureLiveTextField)

    def ensureLiveTextField(cf: CustomField.Text): SE[Unit] =
      ensureLive(cf.live)(show(cf))

    def ensureLiveCustomReqType(rt: CustomReqType): SE[Unit] =
      ensureLive(rt.live)(show(rt))

    def ensureLiveCustomReqTypeId(id: CustomReqTypeId): SE[Unit] =
      whenUntrusted(needCustomReqType(id) >>= ensureLiveCustomReqType)

    def needReq(reqId: ReqId): SE[Req] =
      reqId match {
        case id: GenericReqId => imap.need(id)
      }

    def needCustomReqType(id: CustomReqTypeId): SE[CustomReqType] =
      CustomReqTypeEvents.imap need id

    def needLiveCustomReqType(id: CustomReqTypeId): SE[CustomReqType] =
      needCustomReqType(id).flatTap(rt => ensureLive(rt.live)(rt.mnemonic.value))

    val createGeneric: CreateGenericReq => SE[Unit] = {
      val ^ = CreateGenericReqGD

      def foreachValue(id: GenericReqId, vs: ^.Values): SE[Unit] =
        SE.foldMapRun(vs.values) {
          case ^.ValueForTitle   (v) => SE.nop // Handled below
          case ^.ValueForTags    (v) => Project.reqTags.modify(_.addvs(id, v.whole))
          case ^.ValueForImpTgts (v) => Project.implicationsSrcToTgt.modify(_.addvs(id, v.whole))
          case ^.ValueForImpSrcs (v) => Project.implicationsSrcToTgt.modify(_.addks(v.whole, id))
          case ^.ValueForReqCodes(v) => ReqCodeLogic.addAll_IVs(v.whole, id, true)
      }

      @inline def emptyTitle: Text.GenericReqTitle.OptionalText =
        Vector.empty

      e => for {
        rt      ← needLiveCustomReqType(e.rt)
        reqData ← SE.get(_.reqs)
        id      = e.id
        title   = ^.Title.get(e.vs).fold(emptyTitle)(_.value.whole)
        pp      = reqData.pubids.allocC(rt.id)(id)
        req     = GenericReq(id, pp._2, title, Live)
        reqs    ← imap.create(req)
        _       ← Project.pubidRegister set pp._1
        _       ← foreachValue(id, e.vs)
        _       ← updateIdCeiling(id)
      } yield ()
    }

    def applyDelete(e: DeleteReq): SE[Unit] =
      e.id match {
        case id: GenericReqId => e.da match {
          case Delete  => deleteGenericReq(id)
          case Restore => restoreGenericReq(id)
        }
      }

    def deleteGenericReq(id: GenericReqId): SE[Unit] =
      imap.update(id, grLiveExplicitly.makeDead) >>
      ReqCodeLogic.removeBelongingToReq(id)

    def restoreGenericReq(id: GenericReqId): SE[Unit] =
      imap.update(id, grLiveExplicitly.makeLive) >>
      ReqCodeLogic.restoreBelongingToReq(id)

    def validateTags(tagIds: => Iterable[ApplicableTagId]): SE[Unit] =
      whenUntrusted(
        SE.testO(p =>
        tagIds.toStream.map(p.config.atagValidate).find(_.isDefined) match {
          case Some(None) | None => None
          case Some(Some(err))   => Some(err)
        }
      ))

    def applyPatchTags(e: PatchReqTags): SE[Unit] =
      ensureLiveReqId(e.id) >>
      validateTags(e.patch.value.allValues) >>
      Project.reqTags.modify(_.mod(e.id, e.patch.value.apply))

    def applyPatchImplicationTgt(e: PatchImplicationTgt): SE[Unit] =
      ensureLiveReqId(e.id) >>
      Project.implicationsSrcToTgt.modify(_.mod(e.id, e.patch.value.apply))

    def applyPatchImplicationSrc(e: PatchImplicationSrc): SE[Unit] = {
      val t = e.id
      val d = e.patch.value
      ensureLiveReqId(e.id) >> Project.implicationsSrcToTgt.modify { mm0 =>
        var mm = mm0
        d.removed.foreach(id => mm = mm.del(id, t))
        d.added  .foreach(id => mm = mm.add(id, t))
        mm
      }
    }

    def applySetGenericReqType(e: SetGenericReqType): SE[Unit] =
      for {
        r <- imap.need(e.id)
        _ <- ensureLiveReq(r)
        _ <- ensureLiveCustomReqTypeId(e.value)
        _ <- Project.reqs.modify { reqs =>
          val pp = reqs.pubids.allocC(e.value)(e.id)
          val r2 = r.copy(pubid = pp._2)
          Requirements(reqs.genericReqs + r2, pp._1)
        }
      } yield ()

    def applySetGenericReqTitle(e: SetGenericReqTitle): SE[Unit] =
      ensureLiveReqId(e.id) >>
        imap.updateF(e.id, _.copy(title = e.value))

    def applySetCustomTextField(e: SetCustomTextField): SE[Unit] =
      ensureLiveReqId(e.id) >>
      ensureLiveTextFieldId(e.fid) >>
      (Project.reqText ^|-> ReqData.textAt(e.fid, e.id)).set(e.value)
  }

  // ===================================================================================================================
  /**
   * Why the hell is all this req-code changing logic so complicated?
   *
   * Because req codes aren't true, unique identifiers to requirements. They act as such when they're active, but
   * they often change, get reassigned, and become inactive.
   *
   * And then there are code references that can exist in text.
   * Basically, a reference to req via a code always maintains its link to said req, and tries to keep and follow the
   * code around as it changes, without disrupting other workflows.
   *
   * Thus, the complicated logic is in place to achieve the following properties:
   *
   * - A ref to req via code *never* loses the association to the original req.
   * - Refer via code to a req/group, rename code, refs appear to be updated.
   * - Refer via code to a req/group, del code, restore code, ref shows original code.
   * - Refer via code to a req, aggregate req codes, refs appear to be updated.
   * - Refer via code to a req, del code, ref displayed using other code or pubid.
   * - Refer via code to a group, del code, ref displayed as an error.
   * - Delete a req, another req can use its req code.
   * - Delete a req, restore it, it retains its req codes (unless they've been usurped meaning other they're active
   *   and assigned to other targets).
   */
  object ReqCodeLogic {
    import ReqCode._

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.reqCode)

    val validateCode = validateWith(V.reqCode.valueAndNodesU)

    val getTrie = SE get Project.reqCodeTrie.get

    def ensureInactiveData(d: Data, v: Value): SE[Unit] =
      ensureNone(d.active)(_ => s"${show(v)} shouldn't be in use.")

    def ensureActiveDataTargetIs(t: Target): ActiveData => SE[Unit] =
      whenUntrusted(ad => SE.test(ad.target ≟ t, s"Expected ReqCode target to be $t, found: ${ad.target}."))

    def ensureRefToReqExists(v: Value, d: Data, rc: ReqCodeId)(reqId: ReqId): SE[Unit] =
      whenUntrusted(
        SE.test(d.refsToReqs(reqId) contains rc, s"Ref to ${show(reqId)} not found in ${show(v)}."))

    def ensureReqCodeGroup(t: Target): SE[Unit] =
      whenUntrusted(needReqCodeGroup(t).void)

    def needReqCodeGroup(t: Target): SE[ReqCodeGroup] =
      t match {
        case g: ReqCodeGroup => SE ret g
        case x               => SE fail s"Expected a ReqCodeGroup, found: $x"
      }

    def needData(t: Trie, v: Value): SE[Data] =
      t.valueAtPath[SE[Data]](v, SE fail s"${show(v)} not found.")(SE.ret)

    def needActiveData(d: Data, v: Value): SE[ActiveData] =
      optionGet(d.active, s"${show(v)} should be in use.")

    def needValue(id: ReqCodeId): SE[Value] =
      SE.get(_.reqCodes.reqCodesById) >>= (m =>
        optionGet(m get id, s"${show(id)} not found."))

    def needValues[A](ids: Iterable[ReqCodeId], f: (ReqCodeId, Value) => A): SE[Vector[A]] = {
      SE { p =>
        val m = p.reqCodes.reqCodesById
        val found = Vector.newBuilder[A]
        var missing = Vector.empty[ReqCodeId]
        for (id <- ids)
          m.get(id) match {
            case Some(v) => found += f(id, v)
            case None    => missing :+= id
          }
        if (missing.nonEmpty)
          SE Failure s"Codes not found: ${missing.map(_.value).sorted.map("#" + _).mkString(", ")}"
        else
          SE.Ok(p, found.result())
      }
    }

    /**
     * Note: Doesn't call [[updateIdCeiling]]. Surround with [[doAdd]].
     *
     * @param v The validated req code to add.
     * @param addToActive If true the new ReqCode will be active, else it will be added to the dormant ref collections.
     */
    private def _addValidated(t: Trie, id: ReqCodeId, v: Value, target: Target, addToActive: Boolean): SE[Trie] = {
      type R = SE[Trie]

      def createNode: R =
        if (addToActive) {
          val ad = ActiveData(id, target)
          val d = Data(Some(ad), UnivEq.emptySet, UnivEq.emptySetMultimap)
          SE ret t.put(v, d)
        } else
          SE fail s"${show(v)} not found."

      def modifyNode(d: Data): R =
        if (addToActive)
          ensureInactiveData(d, v) |>> {
            val ad = ActiveData(id, target)
            var rg = d.refsToGroup
            var rr = d.refsToReqs
            target match {
              case r: ReqId        => rr = rr.del(r, id)
              case g: ReqCodeGroup => rg = rg - id
            }
            t.put(v, Data(Some(ad), rg, rr))
          }
        else
          needActiveData(d, v) |>> {
            var rr = d.refsToReqs
            target match {
              case reqId: ReqId => rr = rr.add(reqId, id)
              case g: ReqCodeGroup =>
                // This should never happen
                sys.error(s"addReqCode → mod → (grp ∧ ¬addToActive) - $id $v $target ⇏ $g")
            }
            t.put(v, d.copy(refsToReqs = rr))
          }

      t.valueAtPath(v, createNode)(modifyNode)
    }

    /**
     * Note: Doesn't call [[updateIdCeiling]]. Surround with [[doAdd]].
     *
     * @param addToActive If true the new ReqCode will be active, else it will be added to the dormant ref collections.
     */
    def _addUnvalidated(t: Trie, id: ReqCodeId, v: Value, target: Target, addToActive: Boolean): SE[Trie] =
      validateCode(v) >>= (_addValidated(t, id, _, target, addToActive))

    def addOne(id: ReqCodeId, v: Value, target: Target, addToActive: Boolean): SE[Unit] =
      doAdd(_addUnvalidated(_, id, v, target, addToActive), id.value)

    private def _addAll[A](t: Trie, as: Iterable[A], target: Target, addToActive: Boolean)(getId: A => ReqCodeId, getV: A => Value): SE[Trie] =
      foldMapBind(t, as)(a => _addUnvalidated(_, getId(a), getV(a), target, addToActive))

    def addAll_IVs(vs: Iterable[IdAndValue], target: Target, addToActive: Boolean): SE[Unit] =
      doAdd(
        _addAll(_, vs, target, addToActive)(_.id, _.value),
        IdCeilings.maxOfF(vs)(_.id.value))

    private def doAdd(f: Trie => SE[Trie], maxId: Int): SE[Unit] =
      (getTrie >>= f >>= Project.reqCodeTrie.set) >> updateIdCeiling(maxId)

    /**
     * Remove a single code.
     *
     * @param keepRef Determine whether a reference should be kept of the current id and target.
     *                If `false`, the data is gone completely.
     */
    def remove(trie: Trie, v: Value, d: Data, a: ActiveData, keepRef: ReqCodeId => Boolean): Trie = {
      var refsToGroup = d.refsToGroup
      var refsToReqs  = d.refsToReqs
      val id = a.id
      if (keepRef(id))
        a.target match {
          case t: ReqId        => refsToReqs = refsToReqs.add(t, id)
          case _: ReqCodeGroup => refsToGroup += id
        }
      if (refsToGroup.nonEmpty || refsToReqs.nonEmpty)
        trie.put(v, Data(None, refsToGroup, refsToReqs))
      else
        trie.remove(v)
    }

    /**
     * @param keepRef Determine whether a reference should be kept of the current id and target.
     *                If `false`, the data is gone completely.
     */
    def removeValues(vs: Iterable[Value], keepRef: ReqCodeId => Boolean, validateTarget: ActiveData => SE[Unit]): SE[Unit] =
      getTrie.foldMapBind(vs)(v => t =>
        for {
          d <- needData(t, v)
          a <- needActiveData(d, v)
          _ <- validateTarget(a)
        } yield remove(t, v, d, a, keepRef)
      ) >>= Project.reqCodeTrie.set

    def removeValue(v: Value, keepRef: ReqCodeId => Boolean, validateTarget: ActiveData => SE[Unit]): SE[Unit] =
      removeValues(set1(v), keepRef, validateTarget)

    def removeId(id: ReqCodeId, keepRef: ReqCodeId => Boolean, validateTarget: ActiveData => SE[Unit]): SE[Unit] =
      removeIds(set1(id), keepRef, validateTarget)

    def removeIds(ids: Set[ReqCodeId], keepRef: ReqCodeId => Boolean, validateTarget: ActiveData => SE[Unit]): SE[Unit] =
      needValues(ids, (_, v) => v) >>= (vs =>
        removeValues(vs, keepRef, validateTarget))

    def removeBelongingToReq(reqId: ReqId): SE[Unit] =
      SE.get >>= { p =>
        val refd = p.atomScan.codeRefs
        val vs   = p.reqCodes.activeReqCodesByTarget(reqId)
        removeValues(vs, refd.contains, ensureActiveDataTargetIs(reqId))
      }

    /**
     * Restore a requirement's inactive ReqCode back to active status.
     *
     * If the ReqCode is already active with another ID, then it has been usurped while it was inactive.
     * Usurped ReqCodes are renamed to avoid conflict before being restored.
     */
    def restoreReqCode(trie: Trie, reqId: ReqId, id: ReqCodeId, v: Value): SE[Trie] =
      for {
        d <- needData(trie, v)
        _ <- ensureRefToReqExists(v, d, id)(reqId)
      } yield
        if (d.active.isEmpty) {
          // ReqCode is available. Restore simply.
          val ad = ActiveData(id, reqId)
          val rr = d.refsToReqs.del(reqId, id)
          trie.put(v, Data(Some(ad), d.refsToGroup, rr))
        } else {
          // ReqCode has been usurped. Rename before restoration.
          val v2  = renameReqCodeToAvoidConflict(v, trie)
          val ad2 = ActiveData(id, reqId)
          val rr2 = UnivEq.emptySetMultimap[ReqId, ReqCodeId].setvs(reqId, d.refsToReqs(reqId) - id)
          val d2  = Data(Some(ad2), UnivEq.emptySet,  rr2)
          trie
            .put(v, d.copy(refsToReqs = d.refsToReqs.delk(reqId)))
            .put(v2, d2)
        }

    private val maxNodeLen = Grammar.reqCode.nodeLength.total.max

    /**
     * Rename a ReqCode so that the resulting code is available/free.
     *
     * To minimise disruption to other reqs and avoid further potential collisions, the resulting code will not exist
     * in the ReqCode trie at all (as opposed to selecting an existing node with inactive data).
     */
    def renameReqCodeToAvoidConflict(conflicted: Value, trie: Trie): Value = {
      val init     = conflicted.init
      val t        = NonEmptyVector.maybe(init, trie)(trie.dropPath)

      @tailrec
      def go(root: String, i: Int): Node = {
        val s = root + "_" + i
        if (s.length > maxNodeLen)
          go(root.init, 2)
        else {
          val n = Node(s)
          if (t hasValueK n)
            go(root, i + 1)
          else
            n
        }
      }

      val n = go(conflicted.last.value, 2)
      NonEmptyVector.end(init, n)
    }

    /**
     * Restore a requirement's inactive ReqCodes back to active status.
     *
     * If more than one id refers to the same ReqCode, then only the id with the smallest value is activated.
     */
    def restoreReqCodesById(reqId: ReqId, ids: Iterable[ReqCodeId]): SE[Unit] = {
      // Sort IDs here because only the first ID/reqcode is restored and we want determinism
      val idsSorted = ids.toVector.sorted

      needValues(idsSorted, IdAndValue) >>= { ivs =>
        var valuesSeen = Set.empty[Value]
        ivs.foldLeft(getTrie)((seTrie, iv) =>
          if (valuesSeen contains iv.value)
            seTrie
          else {
            valuesSeen += iv.value
            seTrie >>= (restoreReqCode(_, reqId, iv.id, iv.value))
          }
        ) >>= Project.reqCodeTrie.set
      }
    }

    def restoreBelongingToReq(reqId: ReqId): SE[Unit] =
      SE.get(_.reqCodes inactiveIdsByReqId reqId) >>=
        (restoreReqCodesById(reqId, _))

    def addCodesToTarget(target: Target, mm: Multimap[ReqCode.Value, Set, ReqCodeId]): SE[Unit] =
      if (mm.isEmpty)
        SE.nop
      else {
        def modTrie: Trie => SE[Trie] = t0 =>
          foldMapBind(t0, mm.m){ x => t =>
            val v    = x._1
            val ids1 = x._2
            if (ids1.size == 1)
              _addUnvalidated(t, ids1.head, v, target, true)
            else {
              // Sort IDs here because only the first ID/reqcode becomes the ActiveData and we want determinism
              val ids2 = ids1.toVector.sorted
              _addUnvalidated(t, ids2.head, v, target, true) >>=
                (_addAll(_, ids2.tail, target, false)(identity, _ => v))
            }
          }
        val maxId = IdCeilings.maxOfF(mm.values)(IdCeilings maxOf _)
        doAdd(modTrie, maxId)
      }

    def applyPatchReqCodes(e: PatchReqCodes): SE[Unit] =
      for {
        referenced ← SE.get(_.atomScan.codeRefs)
        keepRefIds = e.add.values.foldLeft(referenced)(_ &~_)
        _          ← removeIds(e.remove, keepRefIds.contains, ensureActiveDataTargetIs(e.id))
        _          ← restoreReqCodesById(e.id, e.restore)
        _          ← addCodesToTarget(e.id, e.add)
      } yield ()
  }

  // ===================================================================================================================
  object ReqCodeGroupEvents {
    import ReqCodeLogic._

    val ^ = ReqCodeGroupGD
    val GD = GenericDataApp[ReqCodes](^)

    def applyCreate(e: CreateReqCodeGroup): SE[Unit] = {
      implicit val vs = e.vs
      for {
        c ← GD.need(^.Code)
        t = GD.want(^.Title)(Vector.empty)
        g = if (t.isEmpty) ReqCodeGroup.empty else ReqCodeGroup(t)
        _ ← addOne(e.id, c, g, true)
      } yield ()
    }

    private def updateGroupCode(id: ReqCodeId, newCode: ReqCode.Value): SE[Unit] =
      for {
        t  ← getTrie
        v  ← needValue(id)
        d  ← needData(t, v)
        a  ← needActiveData(d, v)
        g  ← needReqCodeGroup(a.target)
        t2 = remove(t, v, d, a, _ => false)
        t3 ← _addUnvalidated(t2, id, newCode, g, addToActive = true)
        _  ← Project.reqCodeTrie set t3
      } yield ()

    private def modifyGroup(id: ReqCodeId, f: ReqCodeGroup => ReqCodeGroup): SE[Unit] =
      for {
        t  ← getTrie
        v  ← needValue(id)
        d  ← needData(t, v)
        a  ← needActiveData(d, v)
        g  ← needReqCodeGroup(a.target)
        g2 = f(g)
        a2 = a.copy(target = g2)
        d2 = d.copy(active = Some(a2))
        t2 = t.put(v, d2)
        _  ← Project.reqCodeTrie set t2
      } yield ()

    def applyUpdate(e: UpdateReqCodeGroup): SE[Unit] =
      SE.foldMapRun(e.vs.values) {
        case ^.ValueForTitle(t) => modifyGroup(e.id, _.copy(title = t))
        case ^.ValueForCode (v) => updateGroupCode(e.id, v)
      }

    def applyDelete(e: DeleteReqCodeGroup): SE[Unit] =
      for {
        refd ← SE.get(_.atomScan.codeRefs)
        _    ← removeId(e.id, refd.contains, ad => ensureReqCodeGroup(ad.target))
      } yield ()
  }
}
