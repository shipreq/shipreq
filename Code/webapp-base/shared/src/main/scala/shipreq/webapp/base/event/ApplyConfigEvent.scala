package shipreq.webapp.base.event

import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.util.GenericData
import ApplyEventLib._, SE.SE
import DataImplicits._

trait ApplyConfigEvent {
  this: ApplyEvent =>

  // ===================================================================================================================
  object CustomIssueTypeEvents {
    val ^    = CustomIssueTypeGD
    val GD   = GenericDataApp[CustomIssueType](^)
    val imap = IMapStoreL(Project.customIssueTypes)(CustomIssueType.live)

    val validateKey  = validateI(V.customIssueType.keyU, FieldNames.hashRefKey)(_.value)
    val validateDesc = validateO(V.customIssueType.descU, FieldNames.desc)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customIssueType)

    def applyCreate(e: CreateCustomIssueType): SE[Unit] =
      for {
        k <- GD.need(^.Key )(e.vs) >>= validateKey
        d <- GD.need(^.Desc)(e.vs) >>= validateDesc
        _ <- imap create CustomIssueType(e.id, k, d, Live)
        _ <- updateIdCeiling(e.id)
      } yield ()

    val updateKey  = validateKey  >>=@ CustomIssueType.key
    val updateDesc = validateDesc >>=@ CustomIssueType.desc

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForKey  => updateKey (v.value)
      case v: ^.ValueForDesc => updateDesc(v.value)
    }

    def applyUpdate(e: UpdateCustomIssueType): SE[Unit] =
      imap.updateLive(e.id, updateValues(e.vs))

    def applyDelete(e: DeleteCustomIssueType): SE[Unit] =
      imap.deleteOrRestore(e.id, e.da)
  }

  // ===================================================================================================================
  object CustomReqTypeEvents {
    val ^    = CustomReqTypeGD
    val GD   = GenericDataApp[CustomReqType](^)
    val imap = IMapStoreL(Project.customReqTypes)(CustomReqType.live)

    val validateName     = validateA(V.reqType.nameU, FieldNames.name)
    val validateMnemonic = validateI(V.reqType.mnemonicU, FieldNames.mnemonic)(_.value)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customReqType)

    def applyCreate(e: CreateCustomReqType): SE[Unit] =
      for {
        n <- GD.need(^.Name)    (e.vs) >>= validateName
        m <- GD.need(^.Mnemonic)(e.vs) >>= validateMnemonic
        i <- GD.need(^.Imp)     (e.vs)
        _ <- imap create CustomReqType(e.id, m, Set.empty, n, i, Live)
        _ <- updateIdCeiling(e.id)
      } yield ()

    val updateName     = validateName >>=@ CustomReqType.name
    val updateMnemonic = validateMnemonic thenUpdate ((_: CustomReqType) setMnemonic _) // TODO fix lens
    val updateImp      = fieldUpdateFn(CustomReqType.imp)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForName     => updateName    (v.value)
      case v: ^.ValueForImp      => updateImp     (v.value)
      case v: ^.ValueForMnemonic => updateMnemonic(v.value)
    }

    def applyUpdate(e: UpdateCustomReqType): SE[Unit] =
      imap.updateLive(e.id, updateValues(e.vs))

    def applyDelete(e: DeleteCustomReqType): SE[Unit] = {
      val cascade: Set[ReqId] => SE[Unit] =
        e.da match {
          case Delete  => ReqCodeLogic.inactivateBelongingToReqs
          case Restore => ReqCodeLogic.restoreBelongingToReqs
        }
      imap.deleteOrRestore(e.id, e.da) >> reqsToCascadeReqTypeLiveChange(e.id) >>= cascade
    }

    private def reqsToCascadeReqTypeLiveChange(id: CustomReqTypeId): SE[Set[ReqId]] =
      SE.get(_.reqs.genericReqs
        .values.toStream
        .filter(r => r.reqTypeId ==* id && r.liveExplicitly :: Live)
        .map(_.id: ReqId)
        .toSet)
  }

  // ===================================================================================================================
  object TagEvents {
    type Children = TagInTree.Children
    type Parents  = TagInTree.Parents

    val imap = IMapStore(Project.tags)

    val validateName = validateA(V.tag.nameU, FieldNames.name)
    val validateDesc = validateO(V.tag.descU, FieldNames.desc)
    val validateKey  = validateI(V.tag.keyU , FieldNames.hashRefKey)(_.value)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.tag)

    def debugPrintTagTree(name: String): SE[Unit] =
      SE.get(_.config.tags).map(tt =>
        println(s"\n$name\n${TagTree prettyPrint tt}\n"))

    def ensureParentsValid(id: TagId, p: Parents, tt: TagTree): SE[Unit] =
      whenUntrusted(SE.test(
        p.keys.forall(k => k !=* id && tt.containsK(k)),
        s"Invalid parent(s) for ${show(id)}: ${p.keySet -- (tt - id).keySet}"))

    def updateParents(tt: TagTree, id: TagId, p: Parents): SE[TagTree] =
        ensureParentsValid(id, p, tt) |>>
          MMTree.ApplyParents.trustedApply1(tt, id, p)

    def create(tit: TagInTree, parents: Option[Parents]): SE[Unit] =
      imap.create(tit) >>
      parents.fold(SE.nop)(p => lensMod(Project.tags)(updateParents(_, tit.id, p))) >>
      updateIdCeiling(tit.id)

    def applyDelete(e: DeleteTag): SE[Unit] =
      setLife(e.id, e.da.targetState)

    private def setLife(rootId: TagId, newLife: Live): SE[Unit] = {
      def modifySubject(id: TagId, tt: TagTree): TagTree =
        tt.mod(id, TagInTree.live set newLife)

      def childNeedsModification(child: TagInTree, parentId: TagId, parentLive: Live, tt: TagTree): Boolean =
        (child.tag.live ==* parentLive) && {
          val cid = child.tag.id
          !tt.values.exists(x =>
            x.id !=* parentId &&
            x.tag.live ==* parentLive &&
            x.children.exists(_ ==* cid)
          )
        }

      def modifyChildren(children: Children, parentId: TagId, parentLive: Live, tt0: TagTree): SE[TagTree] =
        foldMapBind(tt0, children)(childId => tt =>
          imapNeed(tt)(childId) >>= (child =>
            if (childNeedsModification(child, parentId, parentLive, tt))
              go(childId, tt)
            else
              SE.ret(tt)
            )
        )

      def go(id: TagId, tt: TagTree): SE[TagTree] =
        imapNeed(tt)(id) >>= { subj =>
          val subjLive = subj.tag.live
          val tt2 = modifySubject(id, tt)
          modifyChildren(subj.children, id, subjLive, tt2)
        }

      lensMod(Project.tags)(go(rootId, _))
    }
  }

  abstract class TagEvents[Data <: Tag : ClassTag, GD <: GenericData](final val ^ : GD) {
    final val GD = GenericDataApp[Data](^)
    import TagEvents._

    protected final class UpdateVars(id: TagId, tagTree: TagTree, tit: TagInTree) {
      private var sed: SE[Data] =
        for {
          d <- narrowCC[Tag, Data](tit.tag)
          _ <- ensureLive(d.live)(show(id))
        } yield d

      private var children = tit.children
      private var parentsSE: Option[TagTree => SE[TagTree]] = None

      def apply(f: Data => SE[Data]): Unit =
        sed = sed >>= f

      def setChildren(v: Children): Unit =
        children = v

      def setParents(p: Parents): Unit =
        parentsSE = Some(TagEvents.updateParents(_, id, p))

      def result(): SE[TagTree] = {
        val r = sed.map(tag => tagTree + TagInTree(tag, children))
        parentsSE.fold(r)(r >>= _)
      }
    }

    protected final def update(id: TagId, f: UpdateVars => Unit): SE[Unit] =
      lensMod(Project.tags)(tt =>
        imapNeed(tt)(id) >>= { tit =>
          val vars = new UpdateVars(id, tt, tit)
          f(vars)
          vars.result()
        })
  }

  // -----------------------------------------------------------------------------------------------
  object ApplicableTagEvents extends TagEvents[ApplicableTag, ApplicableTagGD.type](ApplicableTagGD) {
    import TagEvents._

    def applyCreate(e: CreateApplicableTag): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n   ← GD.need(^.Name) >>= validateName
        d   ← GD.need(^.Desc) >>= validateDesc
        k   ← GD.need(^.Key)  >>= validateKey
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = ApplicableTag(e.id, n, d, k, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   ← create(tit, op)
      } yield ()
    }

    val updateName = validateName >>=@ ApplicableTag.name
    val updateDesc = validateDesc >>=@ ApplicableTag.desc
    val updateKey  = validateKey  >>=@ ApplicableTag.key

    def applyUpdate(e: UpdateApplicableTag): SE[Unit] =
      update(e.id, vars =>
        e.vs.values foreach {
          case v: ^.ValueForName     => vars apply updateName(v.value)
          case v: ^.ValueForDesc     => vars apply updateDesc(v.value)
          case v: ^.ValueForKey      => vars apply updateKey (v.value)
          case v: ^.ValueForChildren => vars setChildren v.value
          case v: ^.ValueForParents  => vars setParents v.value
        }
      )
  }

  // -----------------------------------------------------------------------------------------------
  object TagGroupEvents extends TagEvents[TagGroup, TagGroupGD.type](TagGroupGD) {
    import TagEvents._

    def applyCreate(e: CreateTagGroup): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n   ← GD.need(^.Name) >>= validateName
        d   ← GD.need(^.Desc) >>= validateDesc
        mc  ← GD.need(^.MutexChildren)
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = TagGroup(e.id, n, d, mc, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   ← create(tit, op)
      } yield ()
    }

    val updateName          = validateName >>=@ TagGroup.name
    val updateDesc          = validateDesc >>=@ TagGroup.desc
    val updateMutexChildren = fieldUpdateFn(TagGroup.mutexChildren)

    def applyUpdate(e: UpdateTagGroup): SE[Unit] =
      update(e.id, vars =>
        e.vs.values foreach {
          case v: ^.ValueForName          => vars apply updateName(v.value)
          case v: ^.ValueForDesc          => vars apply updateDesc(v.value)
          case v: ^.ValueForMutexChildren => vars apply updateMutexChildren(v.value)
          case v: ^.ValueForChildren      => vars setChildren v.value
          case v: ^.ValueForParents       => vars setParents v.value
        }
      )
  }

  // ===================================================================================================================
  object FieldEvents {
    private val customFieldsL = Project.fields ^|-> FieldSet.customFields
    private val fieldOrderL   = Project.fields ^|-> FieldSet.order

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customField)

    val validateName = validateA(V.field.nameU, FieldNames.name)
    val validateKey  = validateI(V.field.keyU, FieldNames.fieldRefKey)(_.value)

    def create(cf: CustomField): SE[Unit] =
      for {
        fs  ← SE get Project.fields.get
        cfs ← imapCreate(fs.customFields)(cf)
        fs2 = FieldSet(cfs, fs.order :+ cf.id)
        _   ← Project.fields set fs2
        _   ← updateIdCeiling(cf.id)
      } yield ()

    def update[CF <: CustomField : ClassTag](id: CustomFieldId, mod: CF => SE[CF]): SE[Unit] =
      for {
        p  ← SE.get
        m  = customFieldsL get p
        f1 ← imapNeed(m)(id)
        f2 ← narrowCC[CustomField, CF](f1)
        _  ← ensureLive(f2 live p.config)(show(id))
        f3 ← mod(f2)
        _  ← customFieldsL set (m + f3)
      } yield ()

    private val repositionField = repositionFn[FieldId]

    def applyReposition(e: RepositionField): SE[Unit] =
      lensMod(fieldOrderL)(repositionField(e.id, e.newPos))

    private val removeFromOrder = removeFromVector[FieldId]

    private val addSF = appendNewToVector[FieldId]

    def applyAddStaticField(e: AddStaticField): SE[Unit] =
      lensMod(fieldOrderL)(addSF(e.f))

    def ensureDeletableSF(sf: StaticField): SE[Unit] =
      SE.test(
        sf.deletable :: Deletable,
        s"Static field $sf cannot be deleted.")

    def applyDeleteSF(e: DeleteStaticField): SE[Unit] =
      ensureDeletableSF(e.f) >>
        lensMod(fieldOrderL)(removeFromOrder(e.f))

    def applyDeleteCF(e: DeleteCustomField): SE[Unit] =
      for {
        p  ← SE.get
        m  = customFieldsL get p
        f1 ← imapNeed(m)(e.id)
        f2 ← toggleLiveCheckBeforeAfter(f1, e.da.targetState)(_ live p.config, CustomField.liveExplicitly.set, show(f1))
        _  ← customFieldsL set (m + f2)
      } yield ()
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTextFieldEvents {
    import FieldEvents.{validateName, validateKey, create, update}

    val ^ = CustomTextFieldGD
    val GD = GenericDataApp[CustomField.Text](^)

    def applyCreate(e: CreateCustomTextField): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n <- GD.need(^.Name) >>= validateName
        k <- GD.need(^.Key)  >>= validateKey
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ReqTypes)
        _ <- create(CustomField.Text(e.id, n, k, m, r, Live))
      } yield ()
    }

    val updateName      = validateName >>=@ CustomField.Text.name
    val updateKey       = validateKey  >>=@ CustomField.Text.key
    val updateMandatory = fieldUpdateFn(CustomField.Text.mandatory)
    val updateReqTypes  = fieldUpdateFn(CustomField.Text.reqTypes)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForName      => updateName     (v.value)
      case v: ^.ValueForKey       => updateKey      (v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyUpdate(e: UpdateCustomTextField): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomTagFieldGD
    val GD = GenericDataApp[CustomField.Tag](^)

    def applyCreate(e: CreateCustomTagField): SE[Unit] = {
      implicit val vs = e.vs
      for {
        t <- GD.need(^.TagId)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ReqTypes)
        _ <- ensureTagIsLive(t)
        _ <- create(CustomField.Tag(e.id, t, m, r, Live))
      } yield ()
    }

    val updateTagId     = fieldUpdateFn(CustomField.Tag.tagId)
    val updateMandatory = fieldUpdateFn(CustomField.Tag.mandatory)
    val updateReqTypes  = fieldUpdateFn(CustomField.Tag.reqTypes)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForTagId     => updateTagId    (v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyUpdate(e: UpdateCustomTagField): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomImpFieldGD
    val GD = GenericDataApp[CustomField.Implication](^)

    def applyCreate(e: CreateCustomImpField): SE[Unit] = {
      implicit val vs = e.vs
      for {
        t <- GD.need(^.ReqTypeId)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ReqTypes)
        _ <- ensureReqTypeIsLive(t)
        _ <- create(CustomField.Implication(e.id, t, m, r, Live))
      } yield ()
    }

    val updateReqTypeId = fieldUpdateFn(CustomField.Implication.reqTypeId)
    val updateMandatory = fieldUpdateFn(CustomField.Implication.mandatory)
    val updateReqTypes  = fieldUpdateFn(CustomField.Implication.reqTypes)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForReqTypeId => updateReqTypeId(v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyUpdate(e: UpdateCustomImpField): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }
}
