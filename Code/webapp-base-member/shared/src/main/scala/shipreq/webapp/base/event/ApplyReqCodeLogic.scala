package shipreq.webapp.base.event

import nyaya.util.Multimap
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data.ReqCode._
import shipreq.webapp.base.data.{DataValidators => V, _}
import shipreq.webapp.base.event.ApplyEventLib._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.validation.Implicits._

trait ApplyReqCodeLogic {
  this: ApplyEvent =>

  import ApplyReqCodeLogic._

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

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.reqCode)

    val validateCode = validateA(V.reqCode.valueAndNodes named SpecialBuiltInField.Code.name)

    val getTrie = Eval.gets(Project.reqCodeTrie.get)

    implicit object ReqAdder extends Adder[AddReq] {
      override def reqCodeId(a: AddReq) = a.id

      override def apply(t: Trie, cmd: AddReq): Eval[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = Eval[ActiveReq]
          import cmd.{addToActive, id, reqId}

          def createNode: MakeNewData =
            if (addToActive)
              Eval.pure(ActiveReq(id, reqId, None, emptyReqInactive))
            else
              Eval.fail(s"${show(v)} not found; can't add inactive ${show(id)} .")

          def modifyNode(d: Data): MakeNewData =
            if (addToActive)
              ensureInactive(d, v)
                .andReturn(ActiveReq(id, reqId, d.deadGroup, d.reqInactive.del(reqId, id)))
            else
              needActiveReq(d, v)
                .map(ar => ar.copy(reqInactive = ar.reqInactive.add(reqId, id)))

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    implicit object GroupAdder extends Adder[AddGroup] {
      override def reqCodeId(a: AddGroup) = a.liveGroup.id

      override def apply(t: Trie, cmd: AddGroup): Eval[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = Eval[ActiveGroup]
          import cmd.liveGroup

          def createNode: MakeNewData =
            Eval.pure(ActiveGroup(liveGroup, emptyReqInactive))

          def modifyNode(d: Data): MakeNewData =
            ensureInactive(d, v).andReturn(ActiveGroup(liveGroup, d.reqInactive))

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    def ensureInactive(d: Data, v: Value): Eval[Unit] =
      whenUntrusted(d match {
        case _: Inactive    => Eval.unit
        case _: ActiveReq
           | _: ActiveGroup => Eval.fail(s"${show(v)} shouldn't be in use.")
      })

    def ensureActiveReqIs(reqId: ReqId): ActiveReq => Eval[Unit] =
      whenUntrusted(a => Eval.test(a.reqId ==* reqId, s"Expected ReqCode target to be $reqId, found: ${a.reqId}."))

    def ensureRefToReqExists(v: Value, d: Data, rc: ApReqCodeId)(reqId: ReqId): Eval[Unit] =
      whenUntrusted(
        Eval.test(d.reqInactive(reqId) contains rc, s"Ref to ${show(reqId)} not found in ${show(v)}."))

    def needData(t: Trie, v: Value): Eval[Data] =
      t.valueAtPath[Eval[Data]](v, Eval.fail(s"${show(v)} not found."))(Eval.pure)

    def needActiveReq(d: Data, v: Value): Eval[ActiveReq] =
      narrowCC[Data, ActiveReq](d, s"${show(v)} is not an ActiveReq.")

    def needActiveGroup(d: Data, v: Value): Eval[ActiveGroup] =
      narrowCC[Data, ActiveGroup](d, s"${show(v)} is not an ActiveGroup.")

    def needDeadGroup(d: Data, v: Value): Eval[DeadCodeGroup] =
      Eval.some(d.deadGroup, s"Expected to find dead group at ${show(v)}.")

    def needCode(id: ReqCodeId): Eval[Value] =
      Eval.getFlatMap(p => Eval.some(p.content.reqCodes.getReqCode(id), s"${show(id)} not found."))

    def needApCodes[A](ids: Iterable[ApReqCodeId], f: (ApReqCodeId, Value) => A): Eval[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.apReqCodesById.get, f)

    def needCodeGroups[A](ids: Iterable[ReqCodeGroupId], f: (ReqCodeGroupId, Value) => A): Eval[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.reqCodeGroupsById.get, f)

    def needCodes[I <: ReqCodeId, A](ids: Iterable[I])
                                    (getFn: Project => I => Option[Value], f: (I, Value) => A): Eval[Vector[A]] = {
      Eval.getFlatMap { p =>
        val get = getFn(p)
        val found = Vector.newBuilder[A]
        var missing = Vector.empty[I]
        for (id <- ids)
          get(id) match {
            case Some(v) => found += f(id, v)
            case None    => missing :+= id
          }
        if (missing.nonEmpty)
          Eval.fail(s"Codes not found: ${missing.map(_.value).sorted.map("#" + _).mkString(", ")}")
        else
          Eval.pure(found.result())
      }
    }

    private def awakenGroup(g: DeadCodeGroup) = LiveCodeGroup(g.id, g.title)
    private def killGroup  (g: LiveCodeGroup) = DeadCodeGroup(g.id, g.title)

    def addOne[A](a: A)(implicit adder: Adder[A]): Eval[Unit] =
      getTrie.flatMap(addOneT(_, a)).flatMap(Project.reqCodeTrie.set)

    def addAll[A](as: Iterable[A])(implicit adder: Adder[A]): Eval[Unit] =
      getTrie.flatMap(addAllT(_, as)).flatMap(Project.reqCodeTrie.set)

    def addOneT[A](t: Trie, a: A)(implicit adder: Adder[A]): Eval[Trie] =
      updateIdCeiling(adder.reqCodeId(a).value) >>
        adder(t, a)

    def addAllT[A](t: Trie, as: Iterable[A])(implicit adder: Adder[A]): Eval[Trie] =
      updateIdCeiling(IdCeilings.maxOfF(as)(adder.reqCodeId(_).value)) >>
        foldMapBind(t, as)(a => adder(_, a))

    def addAllToReq(vs: Iterable[ApReqCodeId.AndValue], reqId: ReqId, addToActive: Boolean): Eval[Unit] =
      addAll(vs map (iv =>
        AddReq(iv.value, Unvalidated, iv.id, reqId, addToActive)))

    private def putInactive(trie: Trie, code: Value, data: Inactive): Trie =
      if (data.nonEmpty)
        trie.put(code, data)
      else
        trie.remove(code)

    /**
     * @param remember Determine whether a reference should be kept of the current id and target.
     *                 If `false`, the data is gone completely.
     */
    def inactivateReq(trie: Trie, code: Value, a: ActiveReq, remember: Boolean): Trie = {
      val ri: ReqInactive =
        if (remember)
          a.reqInactive.add(a.reqId, a.id)
        else
          a.reqInactive
      val d2 = Inactive(a.deadGroup, ri)
      putInactive(trie, code, d2)
    }

    def inactivateReqsByCodeT(t: Trie, codes: Iterable[Value], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => Eval[Unit]): Eval[Trie] =
      foldMapBind(t, codes)(v => t =>
        for {
          d <- needData(t, v)
          a <- needActiveReq(d, v)
          _ <- validateTarget(a)
        } yield inactivateReq(t, v, a, remember(a.id))
      )

    def inactivateReqsByIdT(t: Trie, ids: Iterable[ApReqCodeId], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => Eval[Unit]): Eval[Trie] =
      needApCodes(ids, (_, v) => v).flatMap(vs =>
        inactivateReqsByCodeT(t, vs, remember, validateTarget))

    def inactivateBelongingToReqs(reqIds: Set[ReqId]): Eval[Unit] =
      getTrie.flatMap(inactivateBelongingToReqsT(_, reqIds)).flatMap(Project.reqCodeTrie.set)

    def inactivateBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): Eval[Trie] =
      Eval.gets(_.content.reqCodes.activeReqCodesByReqId).flatMap(m =>
        foldMapBind(trie, reqIds)(reqId => inactivateReqsByCodeT(_, m(reqId), _ => true, ensureActiveReqIs(reqId))))

    /**
     * @param remember Determine whether the group should be moved into the ReqCode's dead group slot.
     *                 A reason for `false` here is when the group is being moved and will be active elsewhere.
     */
    def inactivateGroup(trie: Trie, code: Value, a: ActiveGroup, remember: Boolean): Trie = {
      val dg: DeadGroup =
        if (remember) // Adding "… && a.group.nonEmpty" means "remember" must also check if refs this code exist
          Some(killGroup(a.group))
        else
          None
      val d2 = Inactive(dg, a.reqInactive)
      putInactive(trie, code, d2)
    }

    def inactivateGroupsByIdT(t: Trie, ids: Iterable[ReqCodeGroupId], remember: Boolean): Eval[Trie] =
      needCodeGroups(ids, (_, v) => v).flatMap(vs =>
        inactivateGroupsByCodeT(t, vs, remember))

    def inactivateGroupsByCodeT(t: Trie, codes: Iterable[Value], remember: Boolean): Eval[Trie] =
      foldMapBind(t, codes)(code => t =>
        for {
          d <- needData(t, code)
          a <- needActiveGroup(d, code)
        } yield inactivateGroup(t, code, a, remember)
      )

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
     * Restore a requirement's inactive ReqCode back to active status.
     *
     * If the ReqCode is already active with another ID, then it has been usurped while it was inactive.
     * Usurped ReqCodes are renamed to avoid conflict before being restored.
     */
    def restoreCodeToReqT(trie: Trie, reqId: ReqId, id: ApReqCodeId, code: Value): Eval[Trie] =
      for {
        d <- needData(trie, code)
        _ <- ensureRefToReqExists(code, d, id)(reqId)
      } yield d match {

        case d: Inactive =>
          // ReqCode is available. Restore simply.
          val a = ActiveReq(id, reqId, d.deadGroup, d.reqInactive.del(reqId, id))
          trie.put(code, a)

        case _: ActiveGroup | _: ActiveReq =>
          // ReqCode has been usurped. Rename before restoration.
          val v2  = renameReqCodeToAvoidConflict(code, trie)
          val ri2 = emptyReqInactive.setvs(reqId, d.reqInactive(reqId) - id)
          val d2  = ActiveReq(id, reqId, None, ri2)
          trie
            .put(code, d.modReqInactive(_ delk reqId))
            .put(v2, d2)
      }

    /**
     * Restore a requirement's inactive ReqCodes back to active status.
     *
     * If more than one id refers to the same ReqCode, then only the id with the smallest value is activated.
     */
    def restoreToReqByIdsT(t0: Trie, reqId: ReqId, ids: Iterable[ApReqCodeId]): Eval[Trie] = {
      // Sort IDs here because only the first ID/reqcode is restored and we want determinism
      var idsSorted = ids.toVector
      if (idsSorted.length > 1)
        idsSorted = idsSorted.sorted

      needApCodes(idsSorted, ApReqCodeId.AndValue).flatMap { ivs =>
        var valuesSeen = Set.empty[Value]
        foldMapBind(t0, ivs)(iv => t =>
          if (valuesSeen contains iv.value)
            Eval.pure(t)
          else {
            valuesSeen += iv.value
            restoreCodeToReqT(t, reqId, iv.id, iv.value)
          }
        )
      }
    }

    def restoreBelongingToReqT(trie: Trie, reqId: ReqId): Eval[Trie] =
      restoreBelongingToReqsT(trie, set1(reqId))

    def restoreBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): Eval[Trie] =
      Eval.gets(_.content.reqCodes.inactiveIdsByReqId).flatMap(m =>
        foldMapBind(trie, reqIds)(reqId => restoreToReqByIdsT(_, reqId, m(reqId))))

    def restoreBelongingToReqs(reqIds: Set[ReqId]): Eval[Unit] =
      getTrie.flatMap(restoreBelongingToReqsT(_, reqIds)).flatMap(Project.reqCodeTrie.set)

    def restoreGroupAtCodeT(trie: Trie, code: Value): Eval[Trie] =
      needData(trie, code).flatMap {
        case d: Inactive =>
          // ReqCode is available. Restore simply.
          needDeadGroup(d, code).map { g =>
            val a = ActiveGroup(awakenGroup(g), d.reqInactive)
            trie.put(code, a)
          }

        case d: ActiveReq =>
          // ReqCode has been usurped. Rename before restoration.
          needDeadGroup(d, code).map { g =>
            val v2 = renameReqCodeToAvoidConflict(code, trie)
            val d2 = ActiveGroup(awakenGroup(g), emptyReqInactive)
            trie
              .put(code, d.copy(deadGroup = None))
              .put(v2, d2)
          }

        case _: ActiveGroup =>
          Eval.fail(s"Group at ${show(code)} is already live.")
      }

    def restoreGroupsByIdT(t0: Trie, ids: Iterable[ReqCodeGroupId]): Eval[Trie] =
      needCodeGroups(ids, (_, _)).flatMap(ivs =>
        foldMapBind(t0, ivs)(iv => t =>
          restoreGroupAtCodeT(t, iv._2)))

    private def addCodesToReq(target: ReqId, mm: Multimap[ReqCode.Value, Set, ApReqCodeId]): Vector[AddReq] = {
      // Result order is important here
      val r = Vector.newBuilder[AddReq]
      mm.m.foreach { x =>
        val v    = x._1
        val ids1 = x._2
        if (ids1.sizeIs == 1)
          r += AddReq(v, Unvalidated, ids1.head, target, true)
        else {
          // Sort IDs here because only the first ID becomes the ActiveReq.id and we want determinism
          val ids2 = ids1.toArray
          java.util.Arrays.sort(ids2, implicitly[Ordering[ApReqCodeId]])
          var first = true
          for (id <- ids2) {
            r += AddReq(v, Unvalidated, id, target, first)
            first = false
          }
        }
      }
      r.result()
    }

    def applyReqCodesPatch(e: ReqCodesPatch): Eval[Unit] =
      for {
        p    <- Eval.get
        refd = p.content.codeRefs
        keep = e.add.values.foldLeft(refd)(_ -- _)
        t0   = p.content.reqCodes.trie
        t1   <- inactivateReqsByIdT(t0, e.remove, keep.contains, ensureActiveReqIs(e.id))
        t2   <- restoreToReqByIdsT(t1, e.id, e.restore)
        t3   <- addAllT(t2, addCodesToReq(e.id, e.add))
        _    <- Project.reqCodeTrie set t3
      } yield ()
  }

}

// =====================================================================================================================

object ApplyReqCodeLogic {

  sealed trait Adder[A] {
    def reqCodeId(a: A): ReqCodeId
    def apply(t: Trie, a: A): Eval[Trie]
  }

  /** Command to add a ReqCode to a requirement. */
  final case class AddReq(code         : Value,
                          codeValidated: Validated,
                          id           : ApReqCodeId,
                          reqId        : ReqId,
                          addToActive  : Boolean)

  /** Command to add an active CodeGroup. */
  final case class AddGroup(code         : Value,
                            codeValidated: Validated,
                            liveGroup    : LiveCodeGroup)

}

