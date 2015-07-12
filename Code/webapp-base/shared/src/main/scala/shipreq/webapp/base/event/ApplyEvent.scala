package shipreq.webapp.base.event

import japgolly.nyaya.LogicPropExt
import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scalaz.{-\/, \/-}
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{MMTree, Valid}
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.data.ReqType.Mnemonic
import DataImplicits._
import DeletionAction._
import ApplyEventLib._

class ApplyEvent(implicit val trust: Trust) {

  def apply(events: GenTraversable[Event]): AP =
    apFoldLeft(events)(apply1) >=> validateDataProps

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

  private def apply1(event: Event): AP =
    event match {
      case e: CreateCustomIssueType => CustomIssueTypeEvents applyCreate e
      case e: UpdateCustomIssueType => CustomIssueTypeEvents applyUpdate e
      case e: DeleteCustomIssueType => CustomIssueTypeEvents applyDelete e

      case e: CreateCustomReqType => CustomReqTypeEvents applyCreate e
      case e: UpdateCustomReqType => CustomReqTypeEvents applyUpdate e
      case e: DeleteCustomReqType => CustomReqTypeEvents applyDelete e

      case e: CreateApplicableTag => ApplicableTagEvents applyCreate e
      case e: UpdateApplicableTag => ApplicableTagEvents applyUpdate e

      case e: CreateTagGroup => TagGroupEvents applyCreate e
      case e: UpdateTagGroup => TagGroupEvents applyUpdate e

      case e: DeleteTag => TagEvents applyDelete e
    }

  // ===================================================================================================================
  object CustomIssueTypeEvents extends GenericDataApp with IMapStore {
    override protected implicit def trust = ApplyEvent.this.trust
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
                  go(childId) run t
                else
                  ok(t)
              }))

          modifyChildren >=> modifySubject(id) run t0
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
      private var tag      = updatableTag run tagInTree.tag
      private var children = tagInTree.children
      private var tt       = tagTree

      def apply(f: AE[D]): Unit =
        tag = tag ?=> f

      def setChildren(v: Children): Unit =
        children = v

      def setParents(p: Parents): Unit =
        TagEvents.setParents(id, p).run(tt) match {
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
}
