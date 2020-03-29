package shipreq.webapp.base.event

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import monocle.Traversal
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{DataValidators => V, _}
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.util.GenericData
import ApplyEventLib._, SE.SE
import DataImplicits._
import Event._

trait ApplyConfigEvent {
  this: ApplyEvent =>

  // ===================================================================================================================
  object CustomIssueTypeEvents {
    val ^    = CustomIssueTypeGD
    val GD   = GenericDataApp[CustomIssueType](^)
    val imap = IMapStoreL(Project.customIssueTypes)(CustomIssueType.live)

    val validateKey  = validateI(V.customIssueType.key.stateless)(_.value)
    val validateDesc = validateO(V.customIssueType.desc.stateless)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customIssueType)

    def applyCreate(e: CustomIssueTypeCreate): SE[Unit] =
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

    def applyUpdate(e: CustomIssueTypeUpdate): SE[Unit] =
      imap.updateLive(e.id, updateValues(e.vs))

    def applyDelete(e: CustomIssueTypeDelete): SE[Unit] =
      imap.setLive(e.id, Dead)

    def applyRestore(e: CustomIssueTypeRestore): SE[Unit] =
      imap.setLive(e.id, Live)
  }

  // ===================================================================================================================
  object CustomReqTypeEvents {
    private val ^    = CustomReqTypeGD
    private val GD   = GenericDataApp[CustomReqType](^)
            val imap = IMapStoreL(Project.reqTypes ^|-> ReqTypes.custom)(CustomReqType.live)

    private val validateName     = validateA(V.reqType.name.stateless)
    private val validateMnemonic = validateI(V.reqType.mnemonic.stateless)(_.value)
    private val updateIdCeiling  = updateIdCeilingFn(IdCeilings.customReqType)

    def applyCreate(e: CustomReqTypeCreate): SE[Unit] =
      for {
        n <- GD.need(^.Name)       (e.vs) >>= validateName
        m <- GD.need(^.Mnemonic)   (e.vs) >>= validateMnemonic
        i <- GD.need(^.Implication)(e.vs)
        _ <- imap create CustomReqType(e.id, m, Set.empty, n, i, Live)
        _ <- updateIdCeiling(e.id)
      } yield ()

    private val updateName        = validateName >>=@ CustomReqType.name
    private val updateMnemonic    = Memo.bool(validateMnemonic >>=@ CustomReqType.mnemonic(_))
    private val updateImplication = fieldUpdateFn(CustomReqType.imp)

    private val updateValues = Memo.bool(retainMnemonic =>
      GD.updateEachValue {
        case v: ^.ValueForName        => updateName       (v.value)
        case v: ^.ValueForImplication => updateImplication(v.value)
        case v: ^.ValueForMnemonic    => updateMnemonic   (retainMnemonic)(v.value)
      })

    def applyUpdate(e: CustomReqTypeUpdate): SE[Unit] =
      isInUse(e.id).flatMap(inUse =>
        imap.updateLive(e.id, updateValues(inUse)(e.vs)))

    /**
      * If there is no content associated with a req type, then it is hard-deleted and references to it are hard-deleted
      * from config.
      *
      * This is a slight deviation from UX consistency. Normally users can delete anything and then restore it back to
      * it's previous state, "state" including related entities so that change/error is cheap from the user's PoV.
      *
      * In this instance, deleting a req type with that's never been associated with any content deletes:
      *   - the req type itself
      *   - custom implication fields based on the req type
      *   - references to the req type in fields' ReqTypeApplicability
      *
      * The tradeoff is that it allows users to mess around with req type config without mnemonics being permanently
      * consumed within the project. It also allows us to create nice ProjectTemplates whilst giving users the freedom
      * to properly remove what they don't want.
      */
    def applyDelete(e: CustomReqTypeDelete): SE[Unit] =
      ifInUse(e.id,
        notInUse  = hardDelete(e.id),
        whenInUse = softDelete(e.id))

    def applyHardDelete(e: CustomReqTypeDeleteHard): SE[Unit] =
      hardDelete(e.id)

    def applySoftDelete(e: CustomReqTypeDeleteSoft): SE[Unit] =
      softDelete(e.id)

    def applyRestore(e: CustomReqTypeRestore): SE[Unit] =
      deleteOrRestore(e.id, Live, ReqCodeLogic.restoreBelongingToReqs)

    private def isInUse(id: CustomReqTypeId): SE[Boolean] =
      SE.get { p =>

        def hasReqs =
          p.content.reqs.pubids.value(id).nonEmpty

        lazy val customFields =
          p.config.fields.customFields.valuesIterator
            .filter(CustomField.referencesCustomReqType(id))
            .map(f => reqtable.Column.CustomField(f.id))
            .toList

        def inSavedViews =
          p.reqtableViewIterator.exists(sv =>
            sv.view.referencesReqType(id) ||
              customFields.exists(sv.view.referencesColumn))

        hasReqs || inSavedViews
      }

    private def ifInUse[A](id       : CustomReqTypeId,
                           notInUse : => SE[A],
                           whenInUse: => SE[A]): SE[A] =
      isInUse(id).flatMap(inUse => if (inUse) whenInUse else notInUse)

    private def deleteOrRestore(id: CustomReqTypeId, newState: Live, cascade: Set[ReqId] => SE[Unit]): SE[Unit] =
      imap.setLive(id, newState) >> reqsToCascadeReqTypeLiveChange(id) >>= cascade

    private def reqsToCascadeReqTypeLiveChange(id: CustomReqTypeId): SE[Set[ReqId]] =
      SE.get(_.content.reqs.genericReqs
        .valuesIterator
        .filter(r => r.reqTypeId ==* id && r.liveExplicitly ==* Live)
        .map(_.id: ReqId)
        .toSet)

    private def softDelete(id: CustomReqTypeId): SE[Unit] =
      deleteOrRestore(id, Dead, ReqCodeLogic.inactivateBelongingToReqs)

    private val fieldReqTypeRules1: Traversal[Project, FieldReqTypeRules[Any]] =
      Project.customFields ^|->> FieldSet.customFieldsTraversal ^|-> CustomField.fieldReqTypeRulesHack

    private val reqTypeApplicability1: Traversal[Project, ApplicableReqTypes] =
      Project.applicableTags ^|-> ApplicableTag.applicableReqTypes

    private def hardDelete(id: CustomReqTypeId): SE[Unit] = {
      def deleteImpFields: SE[Unit] =
        SE.get(_.config.fields.customImpFields.filter(_.reqTypeId ==* id))
          .flatMap(SE.foldMapRun(_)(f => FieldEvents.hardDelete(f.id)))

      def removeFromReqTypeApplicability: SE[Unit] = {
        val f: EndoFn[ApplicableReqTypes]     = _.hardDelete(id)
        val g: EndoFn[FieldReqTypeRules[Any]] = _.hardDelete(id)
        reqTypeApplicability1.modify(f) compose fieldReqTypeRules1.modify(g)
      }

      def deleteReqType: SE[Unit] =
        imap.hardDelete(id)

      deleteImpFields >> removeFromReqTypeApplicability >> deleteReqType
    }
  }

  // ===================================================================================================================
  object TagEvents {
    type Children = TagInTree.Children
    type Parents  = TagInTree.Parents

    val imap = IMapStore(Project.tagTree)

    val validateColour = validateI(V.colour)(_.fold("")(_.value))
    val validateName   = validateA(V.tag.name  .stateless)
    val validateDesc   = validateO(V.tag.desc  .stateless)
    val validateKey    = validateI(V.tag.key   .stateless)(_.value)

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.tag)

    def debugPrintTagTree(name: String): SE[Unit] =
      SE.get(_.config.tags.tree).map(tt =>
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
      parents.fold(SE.nop)(p => lensMod(Project.tagTree)(updateParents(_, tit.id, p))) >>
      updateIdCeiling(tit.id)

    def applyDelete(e: TagDelete): SE[Unit] =
      setLife(e.id, Dead)

    def applyRestore(e: TagRestore): SE[Unit] =
      setLife(e.id, Live)

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

      lensMod(Project.tagTree)(go(rootId, _))
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
      lensMod(Project.tagTree)(tt =>
        imapNeed(tt)(id) >>= { tit =>
          val vars = new UpdateVars(id, tt, tit)
          f(vars)
          vars.result()
        })
  }

  // -----------------------------------------------------------------------------------------------
  object ApplicableTagEventsV1 extends TagEvents[ApplicableTag, ApplicableTagGDv1.type](ApplicableTagGDv1) {
    import TagEvents._

    def applyCreate(e: ApplicableTagCreateV1): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n   ← GD.need(^.Name) >>= validateName
        d   ← GD.need(^.Desc) >>= validateDesc
        k   ← GD.need(^.Key)  >>= validateKey
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = ApplicableTag.v1(e.id, n, d, k, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   ← create(tit, op)
      } yield ()
    }

    val updateDesc = validateDesc >>=@ ApplicableTag.desc
    val updateKey  = validateKey  >>=@ ApplicableTag.key

    def applyUpdate(e: ApplicableTagUpdateV1): SE[Unit] =
      update(e.id, vars =>
        e.vs.values foreach {
          case _: ^.ValueForName     => ()
          case v: ^.ValueForDesc     => vars apply updateDesc(v.value)
          case v: ^.ValueForKey      => vars apply updateKey (v.value)
          case v: ^.ValueForChildren => vars setChildren v.value
          case v: ^.ValueForParents  => vars setParents v.value
        }
      )
  }

  // -----------------------------------------------------------------------------------------------
  object ApplicableTagEvents extends TagEvents[ApplicableTag, ApplicableTagGD.type](ApplicableTagGD) {
    import TagEvents._

    def applyCreate(e: ApplicableTagCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        a   ← GD.need(^.ApplicableReqTypes)
        c   ← GD.need(^.Colour) >>= validateColour
        d   ← GD.need(^.Desc) >>= validateDesc
        k   ← GD.need(^.Key)  >>= validateKey
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = ApplicableTag(e.id, k, d, c, a, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   ← create(tit, op)
      } yield ()
    }

    val updateApplicableReqTypes = fieldUpdateFn(ApplicableTag.applicableReqTypes)
    val updateColour             = validateColour >>=@ ApplicableTag.colour
    val updateDesc               = validateDesc   >>=@ ApplicableTag.desc
    val updateKey                = validateKey    >>=@ ApplicableTag.key

    def applyUpdate(e: ApplicableTagUpdate): SE[Unit] =
      update(e.id, vars =>
        e.vs.values foreach {
          case v: ^.ValueForApplicableReqTypes => vars apply updateApplicableReqTypes(v.value)
          case v: ^.ValueForColour             => vars apply updateColour            (v.value)
          case v: ^.ValueForDesc               => vars apply updateDesc              (v.value)
          case v: ^.ValueForKey                => vars apply updateKey               (v.value)
          case v: ^.ValueForChildren           => vars setChildren v.value
          case v: ^.ValueForParents            => vars setParents v.value
        }
      )
  }

  // -----------------------------------------------------------------------------------------------
  object TagGroupEvents extends TagEvents[TagGroup, TagGroupGD.type](TagGroupGD) {
    import TagEvents._

    def applyCreate(e: TagGroupCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n   ← GD.need(^.Name) >>= validateName
        d   ← GD.need(^.Desc) >>= validateDesc
        ex  ← GD.need(^.Exclusivity)
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = TagGroup(e.id, n, d, ex, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   ← create(tit, op)
      } yield ()
    }

    val updateName        = validateName >>=@ TagGroup.name
    val updateDesc        = validateDesc >>=@ TagGroup.desc
    val updateExclusivity = fieldUpdateFn(TagGroup.exclusivity)

    def applyUpdate(e: TagGroupUpdate): SE[Unit] =
      update(e.id, vars =>
        e.vs.values foreach {
          case v: ^.ValueForName        => vars apply updateName(v.value)
          case v: ^.ValueForDesc        => vars apply updateDesc(v.value)
          case v: ^.ValueForExclusivity => vars apply updateExclusivity(v.value)
          case v: ^.ValueForChildren    => vars setChildren v.value
          case v: ^.ValueForParents     => vars setParents v.value
        }
      )
  }

  // ===================================================================================================================
  object FieldEvents {
    private val fieldOrderL = Project.fields ^|-> FieldSet.order

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customField)

    val validateName = validateA(V.field.name.stateless)

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
        m  = Project.customFields get p
        f1 ← imapNeed(m)(id)
        f2 ← narrowCC[CustomField, CF](f1)
        _  ← ensureLive(f2 live p.config)(show(id))
        f3 ← mod(f2)
        _  ← Project.customFields set (m + f3)
      } yield ()

    private val repositionField = repositionFn[FieldId]

    def applyReposition(e: FieldReposition): SE[Unit] =
      lensMod(fieldOrderL)(repositionField(e.id, e.newPos))

    private val removeFromOrder = removeFromVector[FieldId]

    private val addSF = appendNewToVector[FieldId]

    def applyStaticAdd(e: FieldStaticAdd): SE[Unit] =
      lensMod(fieldOrderL)(addSF(e.f))

    def applyStaticRemove(e: FieldStaticRemove): SE[Unit] =
      lensMod(fieldOrderL)(removeFromOrder(e.f))

    def applyCustomDelete(e: FieldCustomDelete): SE[Unit] =
      deleteOrRestoreCF(e.id, Dead)

    def applyCustomRestore(e: FieldCustomRestore): SE[Unit] =
      deleteOrRestoreCF(e.id, Live)

    private def deleteOrRestoreCF(id: CustomFieldId, targetState: Live): SE[Unit] =
      for {
        p  ← SE.get
        m  = Project.customFields get p
        f1 ← imapNeed(m)(id)
        f2 ← toggleLiveCheckBeforeAfter(f1, targetState)(_ live p.config, CustomField.liveExplicitly.set, show(f1))
        _  ← Project.customFields set (m + f2)
      } yield ()

    def hardDelete(id: CustomFieldId): SE[Unit] =
      Project.fields.modify(f =>
        FieldSet(
          f.customFields - id,
          f.order.filter(_.foldId(_ => true, _ !=* id))))

    def updateCustomFieldV1[D](rules: FieldReqTypeRules[D])
                              (mo: Option[Mandatory], ao: Option[ApplicableReqTypes]): FieldReqTypeRules[D] =
      (mo, ao) match {
        case (None, None) =>
          rules

        case (Some(m), Some(a)) =>
          FieldReqTypeRules.v1(m, a)

        case (Some(m), None   ) =>
          FieldReqTypeRules.resolutionTraversal[D].modify({
            case FieldReqTypeRules.Resolution.Optional
               | FieldReqTypeRules.Resolution.Mandatory => FieldReqTypeRules.Resolution.v1(m)
            case r                                      => r
          })(rules)

        case (None, Some(a)) =>
          val m =
            rules.resolutionIterator().flatMap {
              case FieldReqTypeRules.Resolution.Optional  => Optional :: Nil
              case FieldReqTypeRules.Resolution.Mandatory => Mandatory :: Nil
              case _                                      => Nil
            }.nextOption().getOrElse(Optional)

          FieldReqTypeRules.v1(m, a)
      }
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTextFieldEventsV1 {
    import FieldEvents.{validateName, create, update, updateCustomFieldV1}

    val ^ = CustomTextFieldGDv1
    val GD = GenericDataApp[CustomField.Text](^)

    def applyCreate(e: FieldCustomTextCreateV1): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n <- GD.need(^.Name) >>= validateName
        k <- GD.need(^.Key)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ApplicableReqTypes)
        _ <- create(CustomField.Text.v1(e.id, n, k, m, r, Live))
      } yield ()
    }

    val updateName = validateName >>=@ CustomField.Text.name

    private def updateValues(f: CustomField.Text, vs: ^.NonEmptyValues): SE[CustomField.Text] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForName               => updateName(v.value)
          case _: ^.ValueForKey                => SE.ret
          case v: ^.ValueForMandatory          => f => SE.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => SE.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomTextUpdateV1): SE[Unit] =
      update[CustomField.Text](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEventsV1 {
    import FieldEvents.{create, update, updateCustomFieldV1}

    val ^ = CustomTagFieldGDv1
    val GD = GenericDataApp[CustomField.Tag](^)

    def applyCreate(e: FieldCustomTagCreateV1): SE[Unit] = {
      implicit val vs = e.vs
      for {
        t <- GD.need(^.TagId)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ApplicableReqTypes)
        _ <- ensureTagIsLive(t)
        _ <- create(CustomField.Tag.v1(e.id, t, m, r, Live))
      } yield ()
    }

    val updateTagId = fieldUpdateFn(CustomField.Tag.tagIdv1)

    private def updateValues(f: CustomField.Tag, vs: ^.NonEmptyValues): SE[CustomField.Tag] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForTagId              => updateTagId    (v.value)
          case v: ^.ValueForMandatory          => f => SE.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => SE.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomTagUpdateV1): SE[Unit] =
      update[CustomField.Tag](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEventsV1 {
    import FieldEvents.{create, update, updateCustomFieldV1}

    val ^ = CustomImpFieldGDv1
    val GD = GenericDataApp[CustomField.Implication](^)

    def applyCreate(e: FieldCustomImpCreateV1): SE[Unit] = {
      implicit val vs = e.vs
      for {
        t <- GD.need(^.ReqTypeId)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ApplicableReqTypes)
        _ <- ensureReqTypeIsLive(t)
        _ <- create(CustomField.Implication.v1(e.id, t, m, r, Live))
      } yield ()
    }

    val updateReqTypeId = fieldUpdateFn(CustomField.Implication.reqTypeId)

    private def updateValues(f: CustomField.Implication, vs: ^.NonEmptyValues): SE[CustomField.Implication] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForReqTypeId          => updateReqTypeId(v.value)
          case v: ^.ValueForMandatory          => f => SE.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => SE.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomImpUpdateV1): SE[Unit] =
      update[CustomField.Implication](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTextFieldEvents {
    import FieldEvents.{validateName, create, update}

    val ^ = CustomTextFieldGD
    val GD = GenericDataApp[CustomField.Text](^)

    def applyCreate(e: FieldCustomTextCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        n <- GD.need(^.Name) >>= validateName
        r <- GD.need(^.FieldReqTypeRules)
        _ <- create(CustomField.Text(e.id, n, r, Live))
      } yield ()
    }

    val updateName              = validateName >>=@ CustomField.Text.name
    val updateFieldReqTypeRules = fieldUpdateFn(CustomField.Text.fieldReqTypeRules)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForName              => updateName             (v.value)
      case v: ^.ValueForFieldReqTypeRules => updateFieldReqTypeRules(v.value)
    }

    def applyUpdate(e: FieldCustomTextUpdate): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomTagFieldGD
    val GD = GenericDataApp[CustomField.Tag](^)

    def applyCreate(e: FieldCustomTagCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        r <- GD.need(^.FieldReqTypeRules)
        _ <- ensureTagIsLive(e.tagId)
        _ <- create(CustomField.Tag(e.id, e.tagId, r, Live))
      } yield ()
    }

    val updateTagId             = fieldUpdateFn(CustomField.Tag.tagId)
    val updateFieldReqTypeRules = fieldUpdateFn(CustomField.Tag.fieldReqTypeRules)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForFieldReqTypeRules => updateFieldReqTypeRules(v.value)
    }

    def applyUpdate(e: FieldCustomTagUpdate): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomImpFieldGD
    val GD = GenericDataApp[CustomField.Implication](^)

    def applyCreate(e: FieldCustomImpCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        r <- GD.need(^.FieldReqTypeRules)
        _ <- ensureReqTypeIsLive(e.reqTypeId)
        _ <- create(CustomField.Implication(e.id, e.reqTypeId, r, Live))
      } yield ()
    }

    val updateReqTypeId         = fieldUpdateFn(CustomField.Implication.reqTypeId)
    val updateFieldReqTypeRules = fieldUpdateFn(CustomField.Implication.fieldReqTypeRules)

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForFieldReqTypeRules => updateFieldReqTypeRules(v.value)
    }

    def applyUpdate(e: FieldCustomImpUpdate): SE[Unit] =
      update(e.id, updateValues(e.vs))
  }
}
