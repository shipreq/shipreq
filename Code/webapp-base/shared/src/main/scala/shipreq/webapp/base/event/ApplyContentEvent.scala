package shipreq.webapp.base.event

import scala.collection.GenTraversable
import shipreq.base.util._
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.text.Text
import ApplyEventLib._
import DataImplicits._
import MTrie.Ops

trait ApplyContentEvent extends ApplyConfigEvent {

  object ReqEvents {
    val R  = Project.reqs
    val GR = R ^|-> Requirements.genericReqs
    val TX = Project.reqText
    val T  = Project.reqTags
    val I  = Project.implications ^|-> Implications.srcToTgt
    val C  = Project.reqCodes
    val CT = C ^|-> ReqCodes.trie

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.req)

    val grIMap = IMapApp.data(GenericReq)

    val grLiveExplicitly = LiveApp(GenericReq.liveExplicitly)

    val ensureLive =
      ensureLiveFnP((p, reqId: ReqId) => reqId match {
        case id: GenericReqId => p.reqs.genericReqs.get(id)
      })(_ live _.config.customReqTypes)

    val ensureLiveTextField =
      ensureLiveFn((p, id: CustomField.Text.Id) => p.config.fields.customFields.get(id))(_.live)

    val ensureLiveCustomReqTypeId =
      ensureLiveFn((p, id: CustomReqTypeId) => p.config.customReqTypes get id)(_.live)

    def needCustomReqType(id: CustomReqTypeId): App[Project, CustomReqType] =
      App(p => CustomReqTypeEvents.imap.need(id)(p.config.customReqTypes))

    def createGeneric(e: CreateGenericReq): AP =
      App[Project, Project] { p =>
        import CreateGenericReqGD._
        val id      = e.id
        val reqData = p.reqs

        var result =
          for {
            rt    ← (needCustomReqType(e.rt) >=> ensureLiveBy(_.live))(p)
            title = Title.get(e.vs).fold(Vector.empty: Text.GenericReqTitle.OptionalText)(_.value.whole)
            pp    = reqData.pubids.allocC(rt.id)(id)
            req   = GenericReq(id, pp._2, title, Live)
            reqs  ← grIMap.add(req)(reqData.genericReqs)
          } yield R.set(Requirements(reqs, pp._1))(p)

        e.vs.values.foreach {
          case ValueForTitle   (v) => () // Already used
          case ValueForTags    (v) => result = result.map(T.modify(_.addvs(id, v.whole)))
          case ValueForImpTgts (v) => result = result.map(I.modify(_.addvs(id, v.whole)))
          case ValueForImpSrcs (v) => result = result.map(I.modify(_.addks(v.whole, id)))
          case ValueForReqCodes(v) => result = result ?=> ReqCodeLogic.addAll_IVs(v.whole, id, true)
        }

        result ?=> updateIdCeiling(e.id)
      }

    def applyDelete(e: DeleteReq): AP =
      e.id match {
        case id: GenericReqId => e.da match {
          case Delete  => deleteGenericReq(id)
          case Restore => restoreGenericReq(id)
        }
      }

    def deleteGenericReq(id: GenericReqId): AP = {
      val a = grIMap.update(id, grLiveExplicitly.makeDead)
      val b = ReqCodeLogic.removeBelongingToReq(id)
      (GR @=> a) >=> b
    }

    def restoreGenericReq(id: GenericReqId): AP = {
      val a = grIMap.update(id, grLiveExplicitly.makeLive)
      val b = ReqCodeLogic.restoreBelongingToReq(id)
      (GR @=> a) >=> (C @=> b)
    }

    val validateTags = whenUntrusted[Set[ApplicableTagId] => App[Project, Any]](
      tags => App(p =>
        tags.toStream.map(p.config.atagValidate).find(_.isDefined) match {
          case Some(None) | None => okUnit
          case Some(Some(err))   => fail(err)
        }
      ))

    def applyPatchTags(e: PatchReqTags): AP = {
      val d = e.patch.value
      val a = validateTags(d.allValues) >-> ensureLive(e.id)
      val b = App.ok(T.modify(_.mod(e.id, d.apply)))
      a >-> b
    }

    def applyPatchImplicationTgt(e: PatchImplicationTgt): AP = {
      val s = e.id
      val d = e.patch.value
      val a = ensureLive(e.id)
      val b = App.ok(I.modify(_.mod(s, d.apply)))
      a >-> b
    }

    def applyPatchImplicationSrc(e: PatchImplicationSrc): AP = {
      val t = e.id
      val d = e.patch.value
      val a = ensureLive(e.id)
      val b = App.ok(I.modify { mm0 =>
        var mm = mm0
        d.removed.foreach(id => mm = mm.del(id, t))
        d.added  .foreach(id => mm = mm.add(id, t))
        mm
      })
      a >-> b
    }

    def applySetGenericReqType(e: SetGenericReqType): AP = {
      val f: AE[Requirements] = App { r =>
        val t = r.pubids.allocC(e.value)(e.id)
        for (m <- grIMap.update(e.id, App.ok(_.copy(pubid = t._2)))(r.genericReqs))
          yield Requirements(m, t._1)
      }
      ensureLive(e.id) >-> ensureLiveCustomReqTypeId(e.value) >-> (R @=> f)
    }

    def applySetGenericReqTitle(e: SetGenericReqTitle): AP = {
      val f = grIMap.update(e.id, App.ok(_.copy(title = e.value)))
      ensureLive(e.id) >-> (GR @=> f)
    }

    def applySetCustomTextField(e: SetCustomTextField): AP = {
      val modText: AE[ReqData.Text] = App.ok(ReqData.textAt(e.fid, e.id).set(e.value))
      ensureLive(e.id) >-> ensureLiveTextField(e.fid) >-> (TX @=> modText)
    }
  }

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

    val C = Project.reqCodes
    val CT = C ^|-> ReqCodes.trie

    type ARC = AE[ReqCodes]
    type AT = AE[Trie]

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.reqCode)

    val validateCode = validateWith(V.reqCode.valueAndNodesU)

    val ensureInactive: AE[Data] = {
      val f = ensureNone[ActiveData](a => s"ReqCode should be inactive: $a.")
      App(d => f(d.active).map(_ => d))
    }

    val ensureActive: App[Data, ActiveData] = {
      val f = ensureSome[ActiveData]("ReqCode should be active.")
      App(d => f(d.active))
    }

    val ensureReqCodeGroup: App[Target, ReqCodeGroup] =
      App {
        case g: ReqCodeGroup => ok(g)
        case x => fail(s"Expect a ReqCodeGroup, found: $x")
      }

    private def needData(t: Trie, v: Value): Result[Data] =
      t.valueAtPath[Result[Data]](v, fail(s"Trie data found for $v"))(ok)

    private def needValue(rc: ReqCodes): App[ReqCodeId, Value] = {
      val m = rc.reqCodesById
      App(id => m.get(id) ensureSome s"ReqCode not found: $id")
    }

    private def needValues(rc: ReqCodes, ids: GenTraversable[ReqCodeId]): Result[Vector[IdAndValue]] = {
      val nv = needValue(rc)
      apFoldLeft(ids)(id => App((q: Vector[IdAndValue]) =>
        nv(id).map(v => q :+ IdAndValue(id, v))
      ))(Vector.empty)
    }

    def modifyTrieRC(f: ReqCodes => AT): ARC =
      App(rc => f(rc)(rc.trie) map ReqCodes.apply)

    def modifyTrieP(f: (Project, ReqCodes) => AT): AP =
      App(p => (CT @=> f(p, C get p))(p))

    /**
     * Add a ReqCode.
     *
     * @param addToActive If true the new ReqCode will be active, else it will be added to the dormant ref collections.
     */
    def add(id: ReqCodeId, value: Value, target: Target, addToActive: Boolean): AT =
      validateCode(value) ?-> App { t =>
        type R = Result[Trie]

        def createNode: R =
          if (addToActive) {
            val ad = ActiveData(id, target)
            val d = Data(Some(ad), UnivEq.emptySet, UnivEq.emptySetMultimap)
            ok(t.put(value, d))
          } else
            fail(s"ReqCode not found: $value")

        def modifyNode(d: Data): R =
          if (addToActive)
            ensureInactive(d).map { _ =>
              val ad = ActiveData(id, target)
              var rg = d.refsToGroup
              var rr = d.refsToReqs
              target match {
                case r: ReqId        => rr = rr.del(r, id)
                case g: ReqCodeGroup => rg = rg - id
              }
              t.put(value, Data(Some(ad), rg, rr))
            }
          else
            ensureActive(d).map { _ =>
              var rr = d.refsToReqs
              target match {
                case reqId: ReqId => rr = rr.add(reqId, id)
                case g: ReqCodeGroup =>
                  // This should never happen
                  sys.error(s"addReqCode → mod → (grp ∧ ¬addToActive) - $id $value $target ⇏ $g")
              }
              t.put(value, d.copy(refsToReqs = rr))
            }

        t.valueAtPath(value, createNode)(modifyNode)
      }

    def addAll_IVs(vs: GenTraversable[IdAndValue], target: Target, addToActive: Boolean): AP =
      CT @=> apFoldLeft(vs)(iv => add(iv.id, iv.value, target, addToActive)) >=>
        updateIdCeiling(IdCeilings.maxOfF(vs)(_.id))

//    def addAll_VIs(vs: GenTraversable[(Value, ReqCodeId)], target: Target, addToActive: Boolean): AT =
//      apFoldLeft(vs)(vi => add(vi._2, vi._1, target, addToActive))

    def addAll_V_Is(v: Value, ids: GenTraversable[ReqCodeId], target: Target, addToActive: Boolean): AT =
      apFoldLeft(ids)(id => add(id, v, target, addToActive))


    /**
     * Remove a single code, then perform an addition action with the `ActiveData` found.
     *
     * @param checkTarget Validate the existing `ActiveData` target before making a change.
     * @param keepRef Determine whether a reference should be kept of the current id and target. If false, the data is
     *                gone completely.
     * @param and Perform an additional action at the end, using the `ActiveData` found before removal.
     */
    def remove1And(v: Value, checkTarget: AE[Target], keepRef: ReqCodeId => Boolean, and: ActiveData => AT): AT =
      App { trie =>

        def remove(d: Data, a: ActiveData): Trie = {
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

        for {
          d  ← needData(trie, v)
          a  ← ensureActive(d)
          _  ← checkTarget(a.target)
          t2 = remove(d, a)
          t3 ← and(a)(t2)
        } yield t3
      }

    def removeValue(v: Value, checkTarget: AE[Target], keepRef: ReqCodeId => Boolean): AT =
      remove1And(v, checkTarget, keepRef, _ => nop)

    def removeValues(vs: Set[Value], checkTarget: AE[Target], keepRef: ReqCodeId => Boolean): AT =
      apFoldLeft(vs)(removeValue(_, checkTarget, keepRef))

    def removeIds(ids: Set[ReqCodeId], checkTarget: AE[Target], keepRef: ReqCodeId => Boolean): ARC =
      App { rc =>
        for {
          vs <- needValue(rc).traverseSet(ids)
          t  <- apFoldLeft(vs)(removeValue(_, checkTarget, keepRef))(rc.trie)
        } yield ReqCodes(t)
      }

    def removeBelongingToReq(reqId: ReqId): AP =
      modifyTrieP { (p, rc) =>
        val referenced = p.atomScan.codeRefs
        val vs = rc.activeReqCodesByTarget(reqId)
        removeValues(vs, ensureEqual(reqId), referenced.contains)
      }

    /**
     * Restore a requirement's inactive ReqCode back to active status.
     *
     * If the ReqCode is already active with another ID, then it has been usurped while inactive, in which case this
     * function returns without modification or error.
     */
    def restoreReqCode(reqId: ReqId, id: ReqCodeId, v: Value): AT =
      App(trie =>
        for {
          d <- needData(trie, v)
          _ <- untrustedTest(d.refsToReqs(reqId) contains id, s"$reqId not found in $v")
        } yield
        if (d.active.isEmpty) {
          val ad = ActiveData(id, reqId)
          val rr = d.refsToReqs.del(reqId, id)
          trie.put(v, Data(Some(ad), d.refsToGroup, rr))
        } else
        // ReqCode has been usurped while it was inactive - now it will have to stay inactive
          trie
      )

    /**
     * Restore a requirement's inactive ReqCodes back to active status.
     *
     * If more than one id refers to the same ReqCode, then only the id with the smallest value is activated.
     */
    def restoreReqCodesById(reqId: ReqId, ids: GenTraversable[ReqCodeId]): ARC =
      App { rc =>

        var valuesSeen = Set.empty[Value]
        def fold(iv: IdAndValue): AT =
          if (valuesSeen contains iv.value)
            nop
          else {
            valuesSeen += iv.value
            restoreReqCode(reqId, iv.id, iv.value)
          }

        // Sort IDs here because only the first ID/reqcode is restored and we want determinism
        val ids2 = ids.toVector.sorted

        for {
          ivList <- needValues(rc, ids2)
          t      <- apFoldLeft(ivList)(fold)(rc.trie)
        } yield ReqCodes(t)
      }

    def restoreBelongingToReq(reqId: ReqId): ARC =
      App { rc =>
        val ids = rc.inactiveIdsByReqId(reqId)
        restoreReqCodesById(reqId, ids)(rc)
      }

    def applyPatchReqCodes(e: PatchReqCodes): AP = {
      val addIds = e.add.values.foldLeft(Set.empty[ReqCodeId])(_ ++ _)
      val target = e.id

      val addCodes: ARC =
        if (e.add.isEmpty)
          nop
        else
          ReqCodes.trie @=> apFoldLeft(e.add.m) { x =>
            val v    = x._1
            val ids1 = x._2
            if (ids1.size == 1)
              add(ids1.head, v, target, true)
            else {
              // Sort IDs here because only the first ID/reqcode becomes the ActiveData and we want determinism
              val ids2 = ids1.toVector.sorted
              add(ids2.head, v, target, true) >=> addAll_V_Is(v, ids2.tail, target, false)
            }
          }

      val restore = restoreReqCodesById(target, e.restore)

      val restoreAndAdd = restore >=> addCodes

      val maxId = IdCeilings.maxOfF(e.add.values)(IdCeilings maxOf _)

      App { (p: Project) =>
        val referenced = p.atomScan.codeRefs
        val keepRefIds = referenced -- addIds
        val remove = removeIds(e.remove, ensureEqual(target), keepRefIds.contains)

        val app = C @=> (remove >=> restoreAndAdd) >=> updateIdCeiling(maxId)
        app(p)
      }
    }

    def updateGroupCode(id: ReqCodeId, newCode: Value): ARC =
      modifyTrieRC { rc =>
        def relocate(a: ActiveData): AT =
          ensureReqCodeGroup(a.target) ?=>> (add(id, newCode, _, true))

        def update(curCode: Value): AT =
          remove1And(curCode, nop, _ => false, relocate)

        needValue(rc)(id) ?=>> update
      }

    def modifyGroup(id: ReqCodeId, f: ReqCodeGroup => ReqCodeGroup): ARC =
      modifyTrieRC { rc =>
        def update(v: Value): AT =
          App(t =>
            for {
              d <- needData(t, v)
              a <- ensureActive(d)
              g <- ensureReqCodeGroup(a.target)
            } yield {
              val g2 = f(g)
              val a2 = a.copy(target = g2)
              val d2 = d.copy(active = Some(a2))
              t.put(v, d2)
            }
          )

        needValue(rc)(id) ?=>> update
      }
  }

  object ReqCodeGroupEvents extends GenericDataApp {
    import ReqCodeLogic._

    override val ^ = ReqCodeGroupGD
    override type Data = ReqCodes

    val readCode  = need(^.Code)
    val readTitle = want(^.Title)(Vector.empty)

    def applyCreate(e: CreateReqCodeGroup): AP = {
      implicit val vs = e.vs
      val app =
        for {
          c <- readCode
          t <- readTitle
        } yield {
          val g = if (t.isEmpty) ReqCodeGroup.empty else ReqCodeGroup(t)
          CT @=> add(e.id, c, g, true) >=> updateIdCeiling(e.id)
        }
      app.joinE
    }

    def applyUpdate(e: UpdateReqCodeGroup): AP = {
      val id = e.id

      val updateValues = updateEachValue {
        case ^.ValueForTitle(t) => modifyGroup(id, _.copy(title = t))
        case ^.ValueForCode (v) => updateGroupCode(id, v)
      }

      C @=> updateValues(e.vs)
    }

    def applyDelete(e: DeleteReqCodeGroup): AP =
      App { (p: Project) =>
        val referenced = p.atomScan.codeRefs
        val rc = removeIds(Set(e.id), ensureReqCodeGroup, referenced.contains)
        (C @=> rc)(p)
      }
  }
}
