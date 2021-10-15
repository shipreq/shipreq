package shipreq.webapp.member.project.event

import japgolly.microlibs.utils.Memo
import monocle.Traversal
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data.{DataValidators => V, _}
import shipreq.webapp.member.project.event.ApplyEventLib._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.RetiredGenericData._
import shipreq.webapp.member.project.filter._
import shipreq.webapp.member.project.util.GenericData

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

    def applyCreate(e: CustomIssueTypeCreate): Eval[Unit] =
      for {
        k <- GD.need(^.Key )(e.vs).flatMap(validateKey)
        d <- GD.need(^.Desc)(e.vs).flatMap(validateDesc)
        _ <- imap create CustomIssueType(e.id, k, d, Live)
        _ <- updateIdCeiling(e.id)
      } yield ()

    val updateKey  = validateKey  >>=@ CustomIssueType.key
    val updateDesc = validateDesc >>=@ CustomIssueType.desc

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForKey  => updateKey (v.value)
      case v: ^.ValueForDesc => updateDesc(v.value)
    }

    def applyUpdate(e: CustomIssueTypeUpdate): Eval[Unit] =
      imap.updateLive(e.id, updateValues(e.vs))

    def applyDelete(e: CustomIssueTypeDelete): Eval[Unit] =
      imap.setLive(e.id, Dead)

    def applyRestore(e: CustomIssueTypeRestore): Eval[Unit] =
      imap.setLive(e.id, Live)
  }

  // ===================================================================================================================
  object CustomReqTypeEventsV1 {
    private val ^    = CustomReqTypeGDv1
    private val GD   = GenericDataApp[CustomReqType](^)
            val imap = IMapStoreL(Project.reqTypes andThen ReqTypes.custom)(CustomReqType.live)

    private val validateName     = validateA(V.reqType.name.stateless)
    private val validateMnemonic = validateI(V.reqType.mnemonic.stateless)(_.value)
    private val updateIdCeiling  = updateIdCeilingFn(IdCeilings.customReqType)

    def applyCreate(e: CustomReqTypeCreateV1): Eval[Unit] =
      for {
        n <- GD.need(^.Name)       (e.vs).flatMap(validateName)
        m <- GD.need(^.Mnemonic)   (e.vs).flatMap(validateMnemonic)
        i <- GD.need(^.Implication)(e.vs)
        _ <- imap create CustomReqType.v1(e.id, m, Set.empty, n, i, Live)
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

    def applyUpdate(e: CustomReqTypeUpdateV1): Eval[Unit] =
      isCustomReqTypeInUse(e.id).flatMap(inUse =>
        imap.updateLive(e.id, updateValues(inUse)(e.vs)))

    private def isCustomReqTypeInUse(id: CustomReqTypeId): Eval[Boolean] =
      Eval.gets { p =>

        def hasReqs =
          p.content.reqs.pubids.value(id).nonEmpty

        lazy val customFields =
          p.config.fields.customFields.valuesIterator
            .filter(CustomField.referencesCustomReqType(id))
            .map(f => savedview.Column.CustomField(f.id))
            .toList

        def inSavedViews =
          p.savedViewIterator.exists(sv =>
            sv.view.referencesReqType(id) ||
              customFields.exists(sv.view.referencesColumn))

        hasReqs || inSavedViews
      }

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
    def applyDelete(e: CustomReqTypeDelete): Eval[Unit] =
      ifInUse(e.id,
        notInUse  = CustomReqTypeEvents.hardDelete(e.id),
        whenInUse = CustomReqTypeEvents.softDelete(e.id))

    private def ifInUse[A](id       : CustomReqTypeId,
                           notInUse : => Eval[A],
                           whenInUse: => Eval[A]): Eval[A] =
      isCustomReqTypeInUse(id).flatMap(inUse => if (inUse) whenInUse else notInUse)
  }

  // ===================================================================================================================
  object CustomReqTypeEvents {
    private val ^    = CustomReqTypeGD
    private val GD   = GenericDataApp[CustomReqType](^)
            val imap = IMapStoreL(Project.reqTypes andThen ReqTypes.custom)(CustomReqType.live)

    private val validateName     = validateA(V.reqType.name.stateless)
    private val validateMnemonic = validateI(V.reqType.mnemonic.stateless)(_.value)
    private val updateIdCeiling  = updateIdCeilingFn(IdCeilings.customReqType)

    def applyCreate(e: CustomReqTypeCreate): Eval[Unit] =
      for {
        n <- GD.need(^.Name)       (e.vs).flatMap(validateName)
        m <- GD.need(^.Mnemonic)   (e.vs).flatMap(validateMnemonic)
        d <- GD.need(^.Description)(e.vs)
        i <- GD.need(^.Implication)(e.vs)
        _ <- imap create CustomReqType(e.id, m, Set.empty, n, d, i, Live)
        _ <- updateIdCeiling(e.id)
      } yield ()

    private val updateName        = validateName >>=@ CustomReqType.name
    private val updateMnemonic    = Memo.bool(validateMnemonic >>=@ CustomReqType.mnemonic(_))
    private val updateDesc        = fieldUpdateFn(CustomReqType.desc)
    private val updateImplication = fieldUpdateFn(CustomReqType.imp)

    private val updateValues = Memo.bool(retainMnemonic =>
      GD.updateEachValue {
        case v: ^.ValueForName        => updateName       (v.value)
        case v: ^.ValueForMnemonic    => updateMnemonic   (retainMnemonic)(v.value)
        case v: ^.ValueForDescription => updateDesc       (v.value)
        case v: ^.ValueForImplication => updateImplication(v.value)
      })

    def applyUpdate(e: CustomReqTypeUpdate): Eval[Unit] =
      isCustomReqTypeInUse(e.id).flatMap(inUse =>
        imap.updateLive(e.id, updateValues(inUse)(e.vs)))

    def applyHardDelete(e: CustomReqTypeDeleteHard): Eval[Unit] =
      hardDelete(e.id)

    def applySoftDelete(e: CustomReqTypeDeleteSoft): Eval[Unit] =
      softDelete(e.id)

    def applyRestore(e: CustomReqTypeRestore): Eval[Unit] =
      deleteOrRestore(e.id, Live, ReqCodeLogic.restoreBelongingToReqs)

    private def isCustomReqTypeInUse(id: CustomReqTypeId): Eval[Boolean] =
      Eval.gets(_.isReqTypeInUse(id))

    private def deleteOrRestore(id: CustomReqTypeId, newState: Live, cascade: Set[ReqId] => Eval[Unit]): Eval[Unit] =
      imap.setLive(id, newState) >> reqsToCascadeReqTypeLiveChange(id).flatMap(cascade)

    private def reqsToCascadeReqTypeLiveChange(id: CustomReqTypeId): Eval[Set[ReqId]] =
      Eval.gets(_.content.reqs.genericReqs.imap
        .valuesIterator
        .filter(r => r.reqTypeId ==* id && r.liveExplicitly ==* Live)
        .map(_.id: ReqId)
        .toSet)

    def softDelete(id: CustomReqTypeId): Eval[Unit] =
      deleteOrRestore(id, Dead, ReqCodeLogic.inactivateBelongingToReqs)

    private val fieldReqTypeRules1: Traversal[Project, FieldReqTypeRules[Any]] =
      Project.customFields andThen FieldSet.customFieldsTraversal andThen CustomField.fieldReqTypeRulesHack

    private val reqTypeApplicability1: Traversal[Project, ApplicableReqTypes] =
      Project.applicableTags andThen ApplicableTag.applicableReqTypes

    def hardDelete(id: CustomReqTypeId): Eval[Unit] = {

      def getImpFieldsToDelete: Eval[List[CustomField.Implication]] =
        Eval.gets(_.config.fields.customImpFields.filter(_.reqTypeId ==* id))

      def deleteImpFields(fields: List[CustomField.Implication]): Eval[Unit] =
        Eval.foldMapRun(fields)(f => FieldEvents.hardDelete(f.id))

      def removeFromSavedViews(fields: Set[FieldId]): Eval[Unit] = {
        import shipreq.webapp.member.project.data.savedview._
        val remove = Filter.Valid.remove(fields = fields, reqTypes = Set(id))
        Eval.mod { p =>
          Project.savedViewTraversal.modify { view =>
            view
              .filterColumns(p.config) {
                case Column.CustomField(f) if fields.contains(f) => false
                case _                                           => true
              }
              .withFilter(view.filter.flatMap(remove(_).toOption))
          }(p)
        }
      }

      def removeFromReqTypeApplicability: Eval[Unit] = {
        val f: EndoFn[ApplicableReqTypes]     = _.hardDelete(id)
        val g: EndoFn[FieldReqTypeRules[Any]] = _.hardDelete(id)
        reqTypeApplicability1.modify(f) compose fieldReqTypeRules1.modify(g)
      }

      def deleteReqType: Eval[Unit] =
        imap.hardDelete(id)

      for {
        badFields <- getImpFieldsToDelete
        _         <- deleteImpFields(badFields)
        _         <- removeFromSavedViews(badFields.iterator.map(_.fieldId).toSet)
        _         <- removeFromReqTypeApplicability
        _         <- deleteReqType
      } yield ()
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

    def debugPrintTagTree(name: String): Eval[Unit] =
      Eval.gets(_.config.tags.tree).map(tt =>
        println(s"\n$name\n${TagTree prettyPrint tt}\n"))

    def ensureParentsValid(id: TagId, p: Parents, tt: TagTree): Eval[Unit] =
      whenUntrusted(Eval.test(
        p.keys.forall(k => k !=* id && tt.containsK(k)),
        s"Invalid parent(s) for ${show(id)}: ${p.keySet -- (tt - id).keySet}"))

    def updateParents(tt: TagTree, id: TagId, p: Parents): Eval[TagTree] =
        ensureParentsValid(id, p, tt).andReturn(MMTree.ApplyParents.trustedApply1(tt, id, p))

    def create(tit: TagInTree, parents: Option[Parents]): Eval[Unit] =
      imap.create(tit) >>
      parents.fold(Eval.unit)(p => lensMod(Project.tagTree)(updateParents(_, tit.id, p))) >>
      updateIdCeiling(tit.id)

    def applyDelete(e: TagDelete): Eval[Unit] =
      setLife(e.id, Dead)

    def applyRestore(e: TagRestore): Eval[Unit] =
      setLife(e.id, Live)

    private def setLife(rootId: TagId, newLife: Live): Eval[Unit] = {
      def modifySubject(id: TagId, tt: TagTree): TagTree =
        tt.mod(id, TagInTree.live replace newLife)

      def childNeedsModification(child: TagInTree, parentId: TagId, parentLive: Live, tt: TagTree): Boolean =
        (child.tag.live ==* parentLive) && {
          val cid = child.tag.id
          !tt.values.exists(x =>
            x.id !=* parentId &&
            x.tag.live ==* parentLive &&
            x.children.exists(_ ==* cid)
          )
        }

      def modifyChildren(children: Children, parentId: TagId, parentLive: Live, tt0: TagTree): Eval[TagTree] =
        foldMapBind(tt0, children)(childId => tt =>
          imapNeed(tt)(childId).flatMap(child =>
            if (childNeedsModification(child, parentId, parentLive, tt))
              go(childId, tt)
            else
              Eval.pure(tt)
            )
        )

      def go(id: TagId, tt: TagTree): Eval[TagTree] =
        imapNeed(tt)(id).flatMap { subj =>
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
      private var sed: Eval[Data] =
        for {
          d <- narrowCC[Tag, Data](tit.tag)
          _ <- ensureLive(d.live)(show(id))
        } yield d

      private var children = tit.children
      private var parentsEval: Option[TagTree => Eval[TagTree]] = None

      def apply(f: Data => Eval[Data]): Unit =
        sed = sed flatMap f

      def setChildren(v: Children): Unit =
        children = v

      def setParents(p: Parents): Unit =
        parentsEval = Some(TagEvents.updateParents(_, id, p))

      def result(): Eval[TagTree] = {
        val r = sed.map(tag => tagTree + TagInTree(tag, children))
        parentsEval.fold(r)(r flatMap _)
      }
    }

    protected final def update(id: TagId, f: UpdateVars => Unit): Eval[Unit] =
      lensMod(Project.tagTree)(tt =>
        imapNeed(tt)(id).flatMap { tit =>
          val vars = new UpdateVars(id, tt, tit)
          f(vars)
          vars.result()
        })
  }

  // -----------------------------------------------------------------------------------------------
  object ApplicableTagEventsV1 extends TagEvents[ApplicableTag, ApplicableTagGDv1.type](ApplicableTagGDv1) {
    import TagEvents._

    def applyCreate(e: ApplicableTagCreateV1): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        n   <- GD.need(^.Name) .flatMap(validateName)
        d   <- GD.need(^.Desc) .flatMap(validateDesc)
        k   <- GD.need(^.Key)  .flatMap(validateKey)
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = ApplicableTag.v1(e.id, n, d, k, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   <- create(tit, op)
      } yield ()
    }

    val updateDesc = validateDesc >>=@ ApplicableTag.desc
    val updateKey  = validateKey  >>=@ ApplicableTag.key

    def applyUpdate(e: ApplicableTagUpdateV1): Eval[Unit] =
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

    def applyCreate(e: ApplicableTagCreate): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        a   <- GD.need(^.ApplicableReqTypes)
        c   <- GD.need(^.Colour).flatMap(validateColour)
        d   <- GD.need(^.Desc).flatMap(validateDesc)
        k   <- GD.need(^.Key).flatMap(validateKey)
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = ApplicableTag(e.id, k, d, c, a, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   <- create(tit, op)
      } yield ()
    }

    val updateApplicableReqTypes = fieldUpdateFn(ApplicableTag.applicableReqTypes)
    val updateColour             = validateColour >>=@ ApplicableTag.colour
    val updateDesc               = validateDesc   >>=@ ApplicableTag.desc
    val updateKey                = validateKey    >>=@ ApplicableTag.key

    def applyUpdate(e: ApplicableTagUpdate): Eval[Unit] =
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

    def applyCreate(e: TagGroupCreate): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        n   <- GD.need(^.Name).flatMap(validateName)
        d   <- GD.need(^.Desc).flatMap(validateDesc)
        ex  <- GD.need(^.Exclusivity)
        oc  = GD.read(^.Children)
        op  = GD.read(^.Parents)
        tag = TagGroup(e.id, n, d, ex, Live)
        tit = TagInTree(tag, oc getOrElse Vector.empty)
        _   <- create(tit, op)
      } yield ()
    }

    val updateName        = validateName >>=@ TagGroup.name
    val updateDesc        = validateDesc >>=@ TagGroup.desc
    val updateExclusivity = fieldUpdateFn(TagGroup.exclusivity)

    def applyUpdate(e: TagGroupUpdate): Eval[Unit] =
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
    private val fieldOrderL = Project.fields andThen FieldSet.order

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.customField)

    val validateName = validateA(V.field.name.stateless)

    def create(cf: CustomField): Eval[Unit] =
      for {
        fs  <- Eval.gets(Project.fields.get)
        cfs <- imapCreate(fs.customFields)(cf)
        fs2 = FieldSet(cfs, fs.order :+ cf.id)
        _   <- Project.fields replace fs2
        _   <- updateIdCeiling(cf.id)
      } yield ()

    def update[CF <: CustomField : ClassTag](id: CustomFieldId, mod: CF => Eval[CF]): Eval[Unit] =
      for {
        p  <- Eval.get
        m  = Project.customFields get p
        f1 <- imapNeed(m)(id)
        f2 <- narrowCC[CustomField, CF](f1)
        _  <- ensureLive(f2 live p.config)(show(id))
        f3 <- mod(f2)
        _  <- Project.customFields replace (m + f3)
      } yield ()

    private val repositionField = repositionFn[FieldId]

    def applyReposition(e: FieldReposition): Eval[Unit] =
      lensMod(fieldOrderL)(repositionField(e.id, e.newPos))

    private val removeFromOrder = removeFromVector[FieldId]

    private val addSF = appendNewToVector[FieldId]

    def applyStaticAdd(e: FieldStaticAdd): Eval[Unit] =
      lensMod(fieldOrderL)(addSF(e.f))

    def applyStaticRemove(e: FieldStaticRemove): Eval[Unit] =
      lensMod(fieldOrderL)(removeFromOrder(e.f))

    def applyCustomDelete(e: FieldCustomDelete): Eval[Unit] =
      deleteOrRestoreCF(e.id, Dead)

    def applyCustomRestore(e: FieldCustomRestore): Eval[Unit] =
      deleteOrRestoreCF(e.id, Live)

    private def deleteOrRestoreCF(id: CustomFieldId, targetState: Live): Eval[Unit] =
      for {
        p  <- Eval.get
        m  = Project.customFields get p
        f1 <- imapNeed(m)(id)
        f2 <- toggleLiveCheckBeforeAfter(f1, targetState)(_ live p.config, CustomField.liveExplicitly.replace, show(f1))
        _  <- Project.customFields replace (m + f2)
      } yield ()

    def hardDelete(id: CustomFieldId): Eval[Unit] =
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

    def applyCreate(e: FieldCustomTextCreateV1): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        n <- GD.need(^.Name).flatMap(validateName)
        k <- GD.need(^.Key)
        m <- GD.need(^.Mandatory)
        r <- GD.need(^.ApplicableReqTypes)
        _ <- create(CustomField.Text.v1(e.id, n, k, m, r, Live))
      } yield ()
    }

    val updateName = validateName >>=@ CustomField.Text.name

    private def updateValues(f: CustomField.Text, vs: ^.NonEmptyValues): Eval[CustomField.Text] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForName               => updateName(v.value)
          case _: ^.ValueForKey                => Eval.pure
          case v: ^.ValueForMandatory          => f => Eval.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => Eval.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomTextUpdateV1): Eval[Unit] =
      update[CustomField.Text](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEventsV1 {
    import FieldEvents.{create, update, updateCustomFieldV1}

    val ^ = CustomTagFieldGDv1
    val GD = GenericDataApp[CustomField.Tag](^)

    def applyCreate(e: FieldCustomTagCreateV1): Eval[Unit] = {
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

    private def updateValues(f: CustomField.Tag, vs: ^.NonEmptyValues): Eval[CustomField.Tag] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForTagId              => updateTagId    (v.value)
          case v: ^.ValueForMandatory          => f => Eval.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => Eval.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomTagUpdateV1): Eval[Unit] =
      update[CustomField.Tag](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEventsV1 {
    import FieldEvents.{create, update, updateCustomFieldV1}

    val ^ = CustomImpFieldGDv1
    val GD = GenericDataApp[CustomField.Implication](^)

    def applyCreate(e: FieldCustomImpCreateV1): Eval[Unit] = {
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

    private def updateValues(f: CustomField.Implication, vs: ^.NonEmptyValues): Eval[CustomField.Implication] = {
      var man = Option.empty[Mandatory]
      var art = Option.empty[ApplicableReqTypes]

      val u =
        GD.updateEachValue {
          case v: ^.ValueForReqTypeId          => updateReqTypeId(v.value)
          case v: ^.ValueForMandatory          => f => Eval.point {man = Some(v.value); f}
          case v: ^.ValueForApplicableReqTypes => f => Eval.point {art = Some(v.value); f}
        }

      u(vs)(f).map { f2 =>
        val newRules = updateCustomFieldV1(f2.fieldReqTypeRules)(man, art)
        f2.copy(fieldReqTypeRules = newRules)
      }
    }

    def applyUpdate(e: FieldCustomImpUpdateV1): Eval[Unit] =
      update[CustomField.Implication](e.id, updateValues(_, e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTextFieldEvents {
    import FieldEvents.{validateName, create, update}

    val ^ = CustomTextFieldGD
    val GD = GenericDataApp[CustomField.Text](^)

    def applyCreate(e: FieldCustomTextCreate): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        n <- GD.need(^.Name).flatMap(validateName)
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

    def applyUpdate(e: FieldCustomTextUpdate): Eval[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomTagFieldGD
    val GD = GenericDataApp[CustomField.Tag](^)

    val validateDerivativeTags: DerivativeTags => Eval[DerivativeTags] =
      d => ensureTagsExist(d.tagIdIterator().toSet).andReturn(d)

    def applyCreate(e: FieldCustomTagCreate): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        _ <- ensureTagIsLive(e.tagId)
        r <- GD.need(^.FieldReqTypeRules)
        d <- validateDerivativeTags(GD.want(^.DerivativeTags)(DerivativeTags.emptyDisabled))
        _ <- create(CustomField.Tag(e.id, e.tagId, r, d, Live))
      } yield ()
    }

    val updateTagId             = fieldUpdateFn(CustomField.Tag.tagId)
    val updateFieldReqTypeRules = fieldUpdateFn(CustomField.Tag.fieldReqTypeRules)
    val updateDerivativeTags    = validateDerivativeTags >>=@ CustomField.Tag.derivativeTags

    val updateValues = GD.updateEachValue {
      case v: ^.ValueForFieldReqTypeRules => updateFieldReqTypeRules(v.value)
      case v: ^.ValueForDerivativeTags    => updateDerivativeTags   (v.value)
    }

    def applyUpdate(e: FieldCustomTagUpdate): Eval[Unit] =
      update(e.id, updateValues(e.vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEvents {
    import FieldEvents.{create, update}

    val ^ = CustomImpFieldGD
    val GD = GenericDataApp[CustomField.Implication](^)

    def applyCreate(e: FieldCustomImpCreate): Eval[Unit] = {
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

    def applyUpdate(e: FieldCustomImpUpdate): Eval[Unit] =
      update(e.id, updateValues(e.vs))
  }
}
