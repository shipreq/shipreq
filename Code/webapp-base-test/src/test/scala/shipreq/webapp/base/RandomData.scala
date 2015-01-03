package shipreq.webapp.base

import monocle.Lens
import monocle.function.{first, second}
import monocle.std.tuple2._
import scala.annotation.tailrec
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._

import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.prop.test.{Distinct, Gen}
import shipreq.webapp.base.data._, ReqType.Mnemonic
import shipreq.webapp.base.delta._
import shipreq.webapp.base.protocol._
import shipreq.base.util.Debug._
import DataImplicits._

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {

  lazy val id =
    Gen.positivelong

  def shortText1 =
    Gen.alphanumericstring1.lim(AppConsts.shortTextMaxLength) // TODO reenable after Jawn bugfix: Gen.string1

  def shortText =
    Gen.alphanumericstring.lim(AppConsts.shortTextMaxLength) // TODO reenable after Jawn bugfix: Gen.string

  lazy val optionalLargeText =
    shortText1.lim(AppConsts.largeTextMaxLength).option

  lazy val rev =
    Gen.positivelong.map(Rev)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

  lazy val alive =
    Gen.oneof[Alive](Alive, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  lazy val refKey =
    for {
      h <- Gen.alphanumeric
      t <- Gen.charof('.', "_=-", 'a' to 'z', 'A' to 'Z', '0' to '9').list.lim(AppConsts.refKeyLength.end)
    } yield RefKey((h :: t).mkString)

  lazy val customIssueTypeId =
    id map CustomIssueType.Id

  lazy val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, refKey, optionalLargeText, alive)

  /** RefKey uniqueness enforced in Project, not here */
  lazy val customIssueTypes =
    revAndIMap(customIssueType)(identity)

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    id map CustomReqType.Id

  def customReqTypeName =
    shortText1

  lazy val customReqType =
    for {
      id <- customReqTypeId
      n  <- customReqTypeName
      mn <- reqTypeMnemonic
      om <- reqTypeMnemonic.set.lim(16)
      ir <- implicationRequired
      a  <- alive
    } yield CustomReqType(id, mn, om - mn, n, ir, a)

  lazy val customReqTypes = {
    def dname = Distinct.str.at(CustomReqTypeL.name)
    def dmnemonic = {
      val distm = Distinct.fstr.xmap(Mnemonic.apply)(_.value).addhs(ReqType.staticMnemonics).distinct
      val cur = distm.at(CustomReqTypeL.mnemonic)
      val old = distm.lift[Set].at(CustomReqTypeL.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(customReqType)(d.run)
  }

  def distinctId[D, I <: TaggedLong](implicit i: DataIdAux[D, I]) =
    Distinct.flong.xmap(i.mkId)(_.value).distinct.contramap[D](i.id, i.setId)

  def revAndIMap[D, I <: TaggedLong](r: Gen[D])(mod: List[D] => List[D])(implicit i: DataIdAux[D, I]): Gen[RevAnd[IMap[I, D]]] = {
    val d = distinctId[D, I].lift[List]
    val f = mod compose d.run
    val g = f andThen (i.emptyIMap ++ _)
    Gen.apply2(RevAnd[IMap[I, D]])(rev, r.list map g)
  }

  lazy val tagId =
    id map Tag.Id

  lazy val mutexChildren =
    Gen.oneof[MutexChildren](MutexChildren, MutexChildren.Not)

  def tagName =
    shortText1

  lazy val tagGroup =
    Gen.apply5(TagGroup.apply)(tagId, tagName, optionalLargeText, mutexChildren, alive)

  lazy val applicableTag =
    Gen.apply5(ApplicableTag.apply)(tagId, tagName, optionalLargeText, refKey, alive)

  lazy val tag =
    Gen.oneofG[Tag](tagGroup.subst, applicableTag.subst)

  /** RefKey uniqueness enforced in Project, not here */
  lazy val tags: Gen[List[Tag]] = {
    val di = distinctId[Tag, Tag.Id]
    val dn = Distinct.str.at(Tag._name)
    val d = (di * dn).lift[List]
    tag.list map d.run
  }

  type TagTreeStructure = Map[Tag.Id, Vector[Tag.Id]]

  @tailrec
  def preventCycles(m: TagTreeStructure, i: Int = 0): TagTreeStructure =
    Tag.CycleDetectors.multimap.findCycle(m) match {
      case None     =>
        // println(s"No cycles after $i attempts @ size ${m.keyCount}→${m.valueCount}")
        m
      case Some((a, b)) =>
//        println(s"Found cycle #$i [$a→$b] in ${m.m}")
//        preventCycles(m.del(a, b).del(b, a), i + 1) // TODO better but slowwwwww
        preventCycles(m - b, i + 1)
    }

  def tagTreeStructure(tags: Set[Tag.Id]): Gen[TagTreeStructure] =
    if (tags.isEmpty)
      Gen.insert(Map.empty)
    else {
      val tagsSeq = tags.toSeq
      val idset = Gen.oneof(tagsSeq.head, tagsSeq.tail: _*).set
      idset.map(_.toStream)
        .flatMap(ks => Gen sequence ks.map(k => idset.map(ids => (k, (ids - k).toVector)).sup))
        .map(s => preventCycles(s.toMap))
    }

  lazy val tagTree: Gen[TagTree] =
    for {
      l ← tags
      m = Tag.IdAccess.mapById(l)
      s ← tagTreeStructure(m.keySet)
    } yield
      m.values.foldLeft(TagTree.empty)((q, t) =>
        q.add(TagInTree(t, s.getOrElse(t.id, Vector.empty))))

  lazy val revAndTagTree: Gen[RevAnd[TagTree]] =
    Gen.apply2(RevAnd.apply[TagTree])(rev, tagTree)

  def setTagKey(tt: Tag, kk: Option[RefKey]): Tag = tt match {
    case t: ApplicableTag => kk.fold(t)(k => t.copy(key = k))
    case t: TagGroup      => t
  }

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  def distinctRefkeys = {
    type A = RevAnd[CustomIssueTypeIMap]
    type B = RevAnd[TagTree]
    type T = (A, B)
    val refkey = Distinct.fstr.xmap(RefKey.apply)(_.value).distinct
    val issues = refkey
      .at(CustomIssueType._key).liftMapValues[CustomIssueType.Id]
      .at(first[T, A] ^|-> RevAnd._data[CustomIssueTypeIMap] ^|-> imapToMapLens)
    val tags = refkey
      .lift[Option].contramap[Tag](_.keyO, setTagKey)
      .at(TagInTree._tag).liftMapValues[Tag.Id]
      .at(second[T, B] ^|-> RevAnd._data[TagTree] ^|-> imapToMapLens)
    issues + tags
  }

  lazy val project =
    for {
      (issues, tags) <- Gen.tuple2(customIssueTypes, revAndTagTree) map distinctRefkeys.run
      reqtypes       <- customReqTypes
    } yield Project(issues, reqtypes, tags)

  // -------------------------------------------------------------------------------------------------------------------
  object remoteDeltaG {
    def forPart: Partition => Gen[RemoteDeltaG] = {
      case Partition.CustomIssueTypes => customIssueTypesDG
      case Partition.CustomReqTypes   => customReqTypesDG
      case Partition.Tags             => tagsDG
    }

    def generic(p: Partition)(ir: Gen[p.Id], dr: Gen[p.Data]): Gen[RemoteDeltaG] = {
      import p.di
      for {
        d        ← dr.list
        i0       ← ir.set
        i        = d.foldLeft(i0)(_ - _.id)
        (r1, r2) ← revPair
      } yield RemoteDeltaG(p, r1, r2)(i, d)
    }

    lazy val customIssueTypesDG =
      generic(Partition.CustomIssueTypes)(customIssueTypeId, customIssueType)

    lazy val customReqTypesDG =
      generic(Partition.CustomReqTypes)(customReqTypeId, customReqType)

    lazy val tagsDG =
      generic(Partition.Tags)(tagId, povTag)

    lazy val povTag =
      for {
        t      ← tag
        (p, c) ← tagId.set.pair
      } yield {
        val children = (c - t.id -- p).toVector
        val parents  = (p - t.id -- c).toStream.map(_ -> none[Tag.Id]).toMap
        TagProtocol.PovTag(t, TagProtocol.PovRelations(parents, children))
      }
  }

  object remoteDelta {
    def forPart: Partition => Gen[RemoteDelta] =
      remoteDeltaG.forPart(_).map(List(_))
  }

  // -------------------------------------------------------------------------------------------------------------------
  object routines {
    import Routine._, Routines._

    lazy val deletionAction =
      Gen.oneofL(DeletionAction.values)

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val forCfgReqType =
      Gen.apply5(ForCfgReqType)(
        remote(ProjectInit),
        remote(CustomIssueTypeCrud),
        remote(CustomReqTypeCrud),
        remote(CustomReqTypeImplicationMod),
        remote(TagCrud))

    class CrudActionGens[I, V](c: Crudable.Aux[I, V])(idG: Gen[I], vG: Gen[V]) {
      import Gen.Covariance._
      lazy val create = vG.map(CrudAction.Create[V])
      lazy val update = Gen.apply2(CrudAction.Update[I, V])(idG, vG)
      lazy val delete = Gen.apply2(CrudAction.Delete[I])(idG, deletionAction)
      lazy val any    = Gen.oneofG[CrudAction[I, V]](create, update, delete)
    }

    lazy val customIssueTypeCrud = new CrudActionGens(CustomIssueTypeCrud)(
      RandomData.customIssueTypeId,
      Gen.tuple2(refKey, optionalLargeText))

    lazy val customReqTypeCrud = new CrudActionGens(CustomReqTypeCrud)(
      RandomData.customReqTypeId,
      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))

    def tagProtocolValues: Tag => TagProtocol.Values = {
      case TagGroup(_, n, d, e, _)      => TagProtocol.TagGroupValues(n, d, e)
      case ApplicableTag(_, n, d, k, _) => TagProtocol.ApplicableTagValues(n, d, k)
    }

    lazy val tagCrudInput =
      remoteDeltaG.povTag.flatMap(t => {
        val a = Gen insert tagProtocolValues(t.tag)
        val b = Gen insert t.rels
        a \&/ b
      })

    lazy val tagCrud =
      new CrudActionGens(TagCrud)(RandomData.tagId, tagCrudInput)
  }
}
