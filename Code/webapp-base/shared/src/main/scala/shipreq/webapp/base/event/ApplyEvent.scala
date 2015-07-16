package shipreq.webapp.base.event

import japgolly.nyaya.LogicPropExt
import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scalaz.{-\/, \/-}
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.text.Text
import DataImplicits._
import DeletionAction._
import ApplyEventLib._
import MTrie.Ops

class ApplyEvent(implicit val trust: Trust) {

  def apply(events: GenTraversable[Event]): AP =
    apFoldLeft(events)(_apply1) >=> validateDataProps

  def apply1(event: Event): AP =
    _apply1(event) >=> validateDataProps

  private val validateDataProps: AP = {
    val prop = DataProp.project.allIncludingConfig
    whenUntrusted(App { p =>
      val e = prop(p)
      if (e.success)
        ok(p)
      else
        fail(e.report)
    })
  }

  private def _apply1(event: Event): AP =
    event match {
      case e: CreateCustomIssueType => CustomIssueTypeEvents applyCreate e
      case e: UpdateCustomIssueType => CustomIssueTypeEvents applyUpdate e
      case e: DeleteCustomIssueType => CustomIssueTypeEvents applyDelete e

      case e: CreateCustomReqType => CustomReqTypeEvents applyCreate e
      case e: UpdateCustomReqType => CustomReqTypeEvents applyUpdate e
      case e: DeleteCustomReqType => CustomReqTypeEvents applyDelete e

      case e: CreateApplicableTag => ApplicableTagEvents applyCreate e
      case e: UpdateApplicableTag => ApplicableTagEvents applyUpdate e
      case e: CreateTagGroup      => TagGroupEvents      applyCreate e
      case e: UpdateTagGroup      => TagGroupEvents      applyUpdate e
      case e: DeleteTag           => TagEvents           applyDelete e

      case e: CreateCustomTextField => CustomTextFieldEvents applyCreate e
      case e: UpdateCustomTextField => CustomTextFieldEvents applyUpdate e
      case e: CreateCustomTagField  => CustomTagFieldEvents  applyCreate e
      case e: UpdateCustomTagField  => CustomTagFieldEvents  applyUpdate e
      case e: CreateCustomImpField  => CustomImpFieldEvents  applyCreate e
      case e: UpdateCustomImpField  => CustomImpFieldEvents  applyUpdate e
      case e: DeleteCustomField     => FieldEvents           applyDeleteC e
      case e: DeleteStaticField     => FieldEvents           applyDeleteS e
      case e: RepositionField       => FieldEvents           applyReposition e

      case e: CreateGenericReq => ReqEvents    createGeneric      e
      case e: PatchReqCodes    => ReqCodeLogic applyPatchReqCodes e
      case e: DeleteReq        => ReqEvents    applyDelete        e

      case e: CreateReqCodeGroup => ReqCodeGroupEvents applyCreate e
      case e: UpdateReqCodeGroup => ReqCodeGroupEvents applyUpdate e
      case e: DeleteReqCodeGroup => ReqCodeGroupEvents applyDelete e
    }

  trait TellTrust extends AskTrust {
    override final protected implicit def trust = ApplyEvent.this.trust
  }

  // ===================================================================================================================
  object CustomIssueTypeEvents extends GenericDataApp with IMapStore with TellTrust {
    override val ^        = CustomIssueTypeGD
    override type Id      = CustomIssueTypeId
    override type Data    = CustomIssueType
    override val L        = Project.customIssueTypes ^|-> RevAnd.data
    override def liveLens = CustomIssueType.live

    val validateKey  = validateWithF(V.customIssueType.keyU)(_.value)
    val validateDesc = validateWithF(V.customIssueType.descU)(_ getOrElse "")

    val readKey  = need(^.Key)  >=> validateKey
    val readDesc = need(^.Desc) >=> validateDesc

    val updateKey  = updateL(CustomIssueType.key)  <<=< validateKey
    val updateDesc = updateL(CustomIssueType.desc) <<=< validateDesc

    val updateValues = updateEachValue {
      case v: ^.ValueForKey  => updateKey (v.value)
      case v: ^.ValueForDesc => updateDesc(v.value)
    }

    def applyCreate(e: CreateCustomIssueType): AP = {
      implicit val vs = e.vs
      create(
        for {
          k <- readKey
          d <- readDesc
        } yield CustomIssueType(e.id, k, d, Live)
      )
    }

    def applyUpdate(e: UpdateCustomIssueType): AP =
      update(e.id, updateValues(e.vs))

    def applyDelete(e: DeleteCustomIssueType): AP =
      delete(e.id, e.da)
  }

  // ===================================================================================================================
  object CustomReqTypeEvents extends GenericDataApp with IMapStore {
    override protected implicit def trust = ApplyEvent.this.trust
    override val ^        = CustomReqTypeGD
    override type Id      = CustomReqTypeId
    override type Data    = CustomReqType
    override val L        = Project.customReqTypes ^|-> RevAnd.data
    override def liveLens = CustomReqType.live

    val validateName     = validateWith (V.reqType.nameU)
    val validateMnemonic = validateWithF(V.reqType.mnemonicU)(_.value)

    val readName     = need(^.Name)     >=> validateName
    val readMnemonic = need(^.Mnemonic) >=> validateMnemonic
    val readImp      = need(^.Imp)

    val updateName     = updateL(CustomReqType.name)              <<=< validateName
    val updateMnemonic = updateF[Data, Mnemonic](_ setMnemonic _) <<=< validateMnemonic
    val updateImp      = updateL(CustomReqType.imp)

    val updateValues = updateEachValue {
      case v: ^.ValueForName     => updateName    (v.value)
      case v: ^.ValueForImp      => updateImp     (v.value)
      case v: ^.ValueForMnemonic => updateMnemonic(v.value)
    }

    def applyCreate(e: CreateCustomReqType): AP = {
      implicit val vs = e.vs
      create(
        for {
          n <- readName
          m <- readMnemonic
          i <- readImp
        } yield CustomReqType(e.id, m, Set.empty, n, i, Live)
      )
    }

    def applyUpdate(e: UpdateCustomReqType): AP =
      update(e.id, updateValues(e.vs))

    def applyDelete(e: DeleteCustomReqType): AP =
      delete(e.id, e.da)
  }

  // ===================================================================================================================
  object TagEvents {
    type Children = TagInTree.Children
    type Parents  = TagInTree.Parents

    val L = Project.tags ^|-> RevAnd.data
    val imap = IMapApp.like(TagTree.empty)

    val validateName = validateWith(V.tag.nameU)
    val validateDesc = validateWithF(V.tag.descU)(_ getOrElse "")
    val validateKey  = validateWithF(V.tag.keyU)(_.value)

    def ensureParentsValid(id: TagId, p: Parents): AE[TagTree] =
      whenUntrusted(App.test(
        tt => Valid <~ p.keys.forall(k => (k ≠ id) && tt.containsK(k)),
        tt => s"Invalid parent(s) for $id: ${p.keySet -- (tt - id).keySet}"))

    def setParents(id: TagId, p: Parents): AE[TagTree] =
      ensureParentsValid(id, p) >=> App.ok(MMTree.ApplyParents.trustedApply1(_, id, p))

    def create(newTag: Result[TagInTree], parents: Result[Option[Parents]]): AP = {
      def applyParents(id: TagId): AE[TagTree] =
        parents ?=>> {
          case None    => nop
          case Some(p) => setParents(id, p)
        }
      L @=> (newTag ?=>> (t => imap.add(t) >=> applyParents(t.id)))
    }

    def applyDelete(e: DeleteTag): AP =
      L @=> (e.da match {
        case Restore => restore(e.id)
        case SoftDel => softDel(e.id)
        case HardDel => hardDel(e.id)
      })

    private def setLife(ol: Option[Live]): TagId => AE[TagTree] = {

      val modifySubject: TagId => AE[TagTree] =
        ol match {
          case Some(a) =>
            id => App.ok(_.mod(id, TagInTree.live set a))
          case None =>
            withUntrustedCheck[TagTree, TagId](
              (tt, id) => if (tt containsK id) None else Some(s"$id not found."))(
              (tt, id) => tt.mapUnderlying(_.mapValuesNow(_ removeChild id) - id))
        }

      def go(id: TagId): AE[TagTree] =
        imap.needM(id)(t0 => App { subj =>

          val subjLive = subj.tag.live

          def childNeedsModification(child: TagInTree, t: TagTree): Boolean =
            (child.tag.live ≟ subjLive) && {
              val cid = child.tag.id
              !t.values.exists(x =>
                (x.id ≠ id) &&
                (x.tag.live ≟ subjLive) &&
                x.children.exists(_ ≟ cid)
              )
            }

          val modifyChildren =
            apFoldLeft[TagId, TagTree](subj.children)(childId =>
              imap.needM(childId)(t => App { child =>
                if (childNeedsModification(child, t))
                  go(childId)(t)
                else
                  ok(t)
              }))

          (modifyChildren >=> modifySubject(id))(t0)
        })

      go
    }

    val hardDel = setLife(None)
    val softDel = setLife(Some(Dead))
    val restore = setLife(Some(Live))
  }

  abstract class TagEvents[D <: Tag : ClassTag] extends GenericDataApp {
    import TagEvents._
    override final type Data = D

    final val updatableTag = narrowCC[Tag, Data] >=> ensureLiveBy(_.live)

    final class UpdateVars(id: TagId, tagTree: TagTree, tagInTree: TagInTree) {
      private var tag      = updatableTag(tagInTree.tag)
      private var children = tagInTree.children
      private var tt       = tagTree

      def apply(f: AE[D]): Unit =
        tag = tag ?=> f

      def setChildren(v: Children): Unit =
        children = v

      def setParents(p: Parents): Unit =
        TagEvents.setParents(id, p)(tt) match {
          case \/-(tt2)  => tt = tt2
          case e@ -\/(_) => tag = e
        }

      def result =
        tag.map(tt + TagInTree(_, children))
    }

    protected def update(id: TagId, f: UpdateVars => Unit): AP = {
      val update: AE[TagTree] =
        imap.needM(id)(tagTree => App { tit =>
          val vars = new UpdateVars(id, tagTree, tit)
          f(vars)
          vars.result
        })
      L @=> update
    }
  }

  // -----------------------------------------------------------------------------------------------
  object ApplicableTagEvents extends TagEvents[ApplicableTag] {
    import TagEvents._
    override val ^ = ApplicableTagGD

    val readName     = need(^.Name) >=> validateName
    val readDesc     = need(^.Desc) >=> validateDesc
    val readKey      = need(^.Key)  >=> validateKey
    val readParents  = read(^.Parents)
    val readChildren = read(^.Children)

    val updateName = updateL(ApplicableTag.name) <<=< validateName
    val updateDesc = updateL(ApplicableTag.desc) <<=< validateDesc
    val updateKey  = updateL(ApplicableTag.key)  <<=< validateKey

    def applyCreate(e: CreateApplicableTag): AP = {
      implicit val vs = e.vs
      val newTag =
        for {
          n <- readName
          d <- readDesc
          k <- readKey
          c <- readChildren
        } yield {
          val tag = ApplicableTag(e.id, n, d, k, Live)
          TagInTree(tag, c getOrElse Vector.empty)
        }
      create(newTag, readParents)
    }

    def applyUpdate(e: UpdateApplicableTag): AP =
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
  object TagGroupEvents extends TagEvents[TagGroup] {
    import TagEvents._
    override val ^ = TagGroupGD

    val readName          = need(^.Name) >=> validateName
    val readDesc          = need(^.Desc) >=> validateDesc
    val readMutexChildren = need(^.MutexChildren)
    val readParents       = read(^.Parents)
    val readChildren      = read(^.Children)

    val updateName          = updateL(TagGroup.name) <<=< validateName
    val updateDesc          = updateL(TagGroup.desc) <<=< validateDesc
    val updateMutexChildren = updateL(TagGroup.mutexChildren)

    def applyCreate(e: CreateTagGroup): AP = {
      implicit val vs = e.vs
      val newTag =
        for {
          n <- readName
          d <- readDesc
          m <- readMutexChildren
          c <- readChildren
        } yield {
          val tag = TagGroup(e.id, n, d, m, Live)
          TagInTree(tag, c getOrElse Vector.empty)
        }
      create(newTag, readParents)
    }

    def applyUpdate(e: UpdateTagGroup): AP =
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
    val L = Project.fields ^|-> RevAnd.data
    val M = L ^|-> FieldSet.customFields
    val O = L ^|-> FieldSet.order
    val imap = IMapApp[CustomFieldId, CustomField]

    val validateName = validateWith(V.field.nameU)
    val validateKey  = validateWithF(V.field.keyU)(_.value)

    val ensureLive = ensureLiveBy[CustomField](_.live)
    val updateLive = updateL(CustomField.live)

    def create(cf: CustomField): AP =
      L @=> App(fs =>
        for (cfs <- imap.add(cf)(fs.customFields))
          yield FieldSet(cfs, fs.order :+ cf.id))

    val repositionField = reposition[FieldId]

    def applyReposition(e: RepositionField): AP =
      O @=> repositionField(e.id, e.newPos)

    val ensureDeletableSF =
      App.test[StaticField](
        sf => Valid <~ (sf.deletable :: Deletable),
        sf => s"Static field $sf cannot be deleted.")

    val removeFromOrder = removeFromVector[FieldId]

    val deleteSF = ensureDeletableSF >=>> removeFromOrder

    def hardDeleteCustomField(id: CustomFieldId): AE[FieldSet] =
      App(fs =>
        for {
          m <- imap.remove(id)(fs.customFields)
          o <- removeFromOrder(id)(fs.order)
        } yield FieldSet(m, o)
      )

    def applyDeleteC(e: DeleteCustomField): AP =
      e.da match {
        case HardDel => L @=> hardDeleteCustomField(e.id)
        case SoftDel => M @=> setLive(e.id, Dead)
        case Restore => M @=> setLive(e.id, Live)
      }

    def applyDeleteS(e: DeleteStaticField): AP =
      O @=> deleteSF(e.f)

    def setLive(id: CustomFieldId, newValue: Live): AE[imap.M] =
      imap.update(id, updateLive(newValue))
  }

  trait CustomFieldApp extends GenericDataApp {
    import FieldEvents._

    type Data <: CustomField
    val updateValues: ^.NonEmptyValues => AD

    final def update(id: CustomFieldId, vs: ^.NonEmptyValues)(implicit cc: ClassTag[Data]): AP =
      M @=> imap.update(id, ensureLive >=> narrowCC[CustomField, Data] >=> updateValues(vs))
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTextFieldEvents extends CustomFieldApp {
    import FieldEvents._

    override val ^ = CustomTextFieldGD
    override type Data = CustomField.Text

    val readName      = need(^.Name) >=> validateName
    val readKey       = need(^.Key)  >=> validateKey
    val readMandatory = need(^.Mandatory)
    val readReqTypes  = need(^.ReqTypes)

    val updateName      = updateL(CustomField.Text.name) <<=< validateName
    val updateKey       = updateL(CustomField.Text.key)  <<=< validateKey
    val updateMandatory = updateL(CustomField.Text.mandatory)
    val updateReqTypes  = updateL(CustomField.Text.reqTypes)

    val updateValues = updateEachValue {
      case v: ^.ValueForName      => updateName     (v.value)
      case v: ^.ValueForKey       => updateKey      (v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyCreate(e: CreateCustomTextField): AP = {
      implicit val vs = e.vs
      val newField =
        for {
          n <- readName
          k <- readKey
          m <- readMandatory
          r <- readReqTypes
        } yield CustomField.Text(e.id, n, k, m, r, Live)
      newField ?=>> create
    }

    def applyUpdate(e: UpdateCustomTextField): AP =
      update(e.id, e.vs)
  }

  // -----------------------------------------------------------------------------------------------
  object CustomTagFieldEvents extends CustomFieldApp {
    import FieldEvents._

    override val ^ = CustomTagFieldGD
    override type Data = CustomField.Tag

    val readTagId     = need(^.TagId)
    val readMandatory = need(^.Mandatory)
    val readReqTypes  = need(^.ReqTypes)

    val updateTagId     = updateL(CustomField.Tag.tagId)
    val updateMandatory = updateL(CustomField.Tag.mandatory)
    val updateReqTypes  = updateL(CustomField.Tag.reqTypes)

    val updateValues = updateEachValue {
      case v: ^.ValueForTagId     => updateTagId    (v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyCreate(e: CreateCustomTagField): AP = {
      implicit val vs = e.vs
      val newField =
        for {
          t <- readTagId
          m <- readMandatory
          r <- readReqTypes
        } yield CustomField.Tag(e.id, t, m, r, Live)
      newField ?=>> create
    }

    def applyUpdate(e: UpdateCustomTagField): AP =
      update(e.id, e.vs)
  }

  // -----------------------------------------------------------------------------------------------
  object CustomImpFieldEvents extends CustomFieldApp {
    import FieldEvents._

    override val ^ = CustomImpFieldGD
    override type Data = CustomField.Implication

    val readReqTypeId = need(^.ReqTypeId)
    val readMandatory = need(^.Mandatory)
    val readReqTypes  = need(^.ReqTypes)

    val updateReqTypeId = updateL(CustomField.Implication.reqTypeId)
    val updateMandatory = updateL(CustomField.Implication.mandatory)
    val updateReqTypes  = updateL(CustomField.Implication.reqTypes)

    val updateValues = updateEachValue {
      case v: ^.ValueForReqTypeId => updateReqTypeId(v.value)
      case v: ^.ValueForMandatory => updateMandatory(v.value)
      case v: ^.ValueForReqTypes  => updateReqTypes (v.value)
    }

    def applyCreate(e: CreateCustomImpField): AP = {
      implicit val vs = e.vs
      val newField =
        for {
          t <- readReqTypeId
          m <- readMandatory
          r <- readReqTypes
        } yield CustomField.Implication(e.id, t, m, r, Live)
      newField ?=>> create
    }

    def applyUpdate(e: UpdateCustomImpField): AP =
      update(e.id, e.vs)
  }

  // ===================================================================================================================
  object ReqEvents {

    val R = Project.reqs ^|-> RevAnd.data
    val GR = R ^|-> Requirements.genericReqs
    val T = Project.reqTags ^|-> RevAnd.data
    val I = Project.implications ^|-> RevAnd.data ^|-> Implications.srcToTgt
    val C = Project.reqCodes ^|-> RevAnd.data
    val CT = C ^|-> ReqCodes.trie

    val grIMap = IMapApp.data(GenericReq)
    val grLive = LiveApp(GenericReq.live)

    def needCustomReqType(id: CustomReqTypeId): App[Project, CustomReqType] =
      App(p => CustomReqTypeEvents.imap.need(id)(p.config.customReqTypes.data))

    def createGeneric(e: CreateGenericReq): AP =
      App[Project, Project] { p =>
        import CreateGenericReqGD._
        val id = e.id

        val reqData = p.reqs.data
        // TODO Text atoms need to be validated?

        var result =
          for {
            rt    ← needCustomReqType(e.rt)(p)
            title = Title(e.vs).fold(Vector.empty: Text.GenericReqTitle.OptionalText)(_.value.whole)
            pp    = reqData.pubids.allocC(rt.id)(id)
            req   = GenericReq(id, pp._2, title, Live)
            reqs  ← grIMap.add(req)(reqData.genericReqs)
          } yield R.set(Requirements(reqs, pp._1))(p)

        e.vs.values.foreach {
          case ValueForTitle   (v) => () // Already used
          case ValueForTags    (v) => result = result.map(T.modify(_.addvs(id, v.whole)))
          case ValueForImpTgts (v) => result = result.map(I.modify(_.addvs(id, v.whole)))
          case ValueForImpSrcs (v) => result = result.map(I.modify(_.addks(v.whole, id)))
          case ValueForReqCodes(v) => result = result ?=> (CT @=> ReqCodeLogic.addAll_IVs(v.whole, id, true))
        }

        result
      }

    def applyDelete(e: DeleteReq): AP =
      e.id match {
        case id: GenericReqId => e.da match {
          case SoftDel => deleteGenericReq(id)
          case Restore => restoreGenericReq(id)
        }
      }

    def deleteGenericReq(id: GenericReqId): AP = {
      val a = grIMap.update(id, grLive.makeDead)
      val b = ReqCodeLogic.removeBelongingToReq(id)
      (GR @=> a) >=> b
    }

    def restoreGenericReq(id: GenericReqId): AP = {
      val a = grIMap.update(id, grLive.makeLive)
      val b = ReqCodeLogic.restoreBelongingToReq(id)
      (GR @=> a) >=> (C @=> b)
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

    val C = Project.reqCodes ^|-> RevAnd.data
    val CT = C ^|-> ReqCodes.trie

    type ARC = AE[ReqCodes]
    type AT = AE[Trie]

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

    def addAll_IVs(vs: GenTraversable[IdAndValue], target: Target, addToActive: Boolean): AT =
      apFoldLeft(vs)(iv => add(iv.id, iv.value, target, addToActive))

    def addAll_VIs(vs: GenTraversable[(Value, ReqCodeId)], target: Target, addToActive: Boolean): AT =
      apFoldLeft(vs)(vi => add(vi._2, vi._1, target, addToActive))

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

      App { (p: Project) =>
        val referenced = p.atomScan.codeRefs
        val keepRefIds = referenced -- addIds
        val remove = removeIds(e.remove, ensureEqual(target), keepRefIds.contains)

        (C @=> (remove >=> restoreAndAdd))(p)
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
          CT @=> add(e.id, c, g, true)
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
