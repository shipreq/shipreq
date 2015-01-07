package shipreq.webapp.base

import monocle.Lens
import monocle.function.{first, second}
import monocle.std.tuple2._
import scala.annotation.tailrec
import scalaz.OneAnd
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._
import scalaz.syntax.equal._

import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.prop.test.{Distinct, Gen}
import shipreq.webapp.base.data._, ReqType.Mnemonic, Field.ApplicableReqTypes
import shipreq.webapp.base.delta._
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

  def revAnd[D](r: Gen[D]): Gen[RevAnd[D]] =
    Gen.apply2(RevAnd[D])(rev, r)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

  lazy val alive =
    Gen.oneof[Alive](Alive, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  lazy val mandatory =
    Gen.oneof[Mandatory](Mandatory, Mandatory.Not)

  lazy val hashRefKey =
    for {
      h <- Gen.alphanumeric
      t <- Gen.charof('.', "_=-", 'a' to 'z', 'A' to 'Z', '0' to '9').list.lim(AppConsts.hashRefKeyLength.end - 1)
    } yield HashRefKey((h :: t).mkString)

  lazy val customIssueTypeId =
    id map CustomIssueType.Id

  lazy val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, hashRefKey, optionalLargeText, alive)

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val customIssueTypes =
    revAndIMap(customIssueType)(identity)

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    id map CustomReqType.Id

  lazy val staticReqType: Gen[StaticReqType] =
    Gen.oneofL(StaticReqType.values)

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
    def dname = Distinct.str.at(CustomReqType._name)
    def dmnemonic = {
      val distm = Distinct.fstr.xmap(Mnemonic.apply)(_.value).addhs(StaticReqType.mnemonics).distinct
      val cur = distm.at(CustomReqType._mnemonic)
      val old = distm.lift[Set].at(CustomReqType._oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(customReqType)(d.run)
  }

  def distinctId[D, I <: TaggedLong](implicit i: DataIdMAux[D, I]) =
    Distinct.flong.xmap(i.mkId)(_.value).distinct.contramap[D](i.id, i.setId)

  def revAndIMap[D, I <: TaggedLong](r: Gen[D])(mod: List[D] => List[D])(implicit i: DataIdMAux[D, I]): Gen[RevAnd[IMap[I, D]]] = {
    val d = distinctId[D, I].lift[List]
    val f = mod compose d.run
    val g = f andThen (i.emptyIMap ++ _)
    revAnd(r.list map g)
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
    Gen.apply5(ApplicableTag.apply)(tagId, tagName, optionalLargeText, hashRefKey, alive)

  lazy val tag =
    Gen.oneofG[Tag](tagGroup.subst, applicableTag.subst)

  /** HashRefKey uniqueness enforced in Project, not here */
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

  def setTagKey(tt: Tag, kk: Option[HashRefKey]): Tag = tt match {
    case t: ApplicableTag => kk.fold(t)(k => t.copy(key = k))
    case t: TagGroup      => t
  }

  lazy val staticField: Gen[StaticField] =
    Gen.oneofL(StaticField.values)

  def isubset[F[_], A](ga: Gen[A], gf: Gen[F[A]]): Gen[ISubset[F, A]] = {
    def h(k: OneAnd[F, A] => ISubset[F, A]) = gf.flatMap(f => ga.map(a => k(OneAnd(a, f))))
    Gen.oneofG(
      Gen.insert(ISubset.All()),
      h(ISubset.Only.apply),
      h(ISubset.Not.apply))
  }

  def applicableReqTypes(r: Set[CustomReqType.Id]): Gen[ApplicableReqTypes] = {
    val all = StaticReqType.values.list.foldLeft(r.map(a => a: ReqType.Id))(_ + _).toList
    val a = Gen.oneof(all.head, all.tail: _*)
    isubset(a, a.set)
  }

  lazy val customFieldId =
    id map CustomField.Id

  lazy val fieldRefKey =
    for {
      h <- Gen.lower
      t <- Gen.charof('_', "", 'a' to 'z', '0' to '9').list.lim(AppConsts.fieldRefKeyLength.end - 1)
    } yield FieldRefKey((h :: t).mkString)

  def customFieldText(art: Gen[ApplicableReqTypes]): Gen[CustomField.Text] =
    Gen.apply6(CustomField.Text.apply)(customFieldId, shortText1, fieldRefKey, mandatory, art, alive)

  def customField(art: Gen[ApplicableReqTypes]): Gen[CustomField] =
    Gen.oneofGC(customFieldText(art))

  def customFields(cf: Gen[CustomField]): Gen[IMap[CustomField.Id, CustomField]] = {
    def id   = distinctId(CustomField.IdAccess)
    def name = Distinct.str.at(CustomField._name)
    def key  = Distinct.fstr.xmap(FieldRefKey.apply)(_.value).distinct.at(CustomField._key)
    val dist = (id * name * key).lift[Stream]
    cf.stream.map(fs => emptyDataMap(CustomField) ++ dist.run(fs))
  }

  def fieldSet(r: Set[CustomReqType.Id]): Gen[FieldSet] =
    for {
      cf           ← customFields(customField(applicableReqTypes(r)))
      mandatoryIds = cf.keySet.map(f => f: Field.Id) ++ StaticField.notDeletable
      optionalIds  ← Gen.oneof(StaticField.deletable.head, StaticField.deletable.tail: _*).set
      order        ← Gen.shuffle((mandatoryIds ++ optionalIds).toVector)
    } yield FieldSet(cf, order)

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  def distinctHashRefKeys = {
    type A = RevAnd[CustomIssueTypeIMap]
    type B = RevAnd[TagTree]
    type T = (A, B)
    val keyDist = Distinct.fstr.xmap(HashRefKey.apply)(_.value).distinct
    val issues = keyDist
      .at(CustomIssueType._key).liftMapValues[CustomIssueType.Id]
      .at(first[T, A] ^|-> RevAnd._data[CustomIssueTypeIMap] ^|-> imapToMapLens)
    val tags = keyDist
      .lift[Option].contramap[Tag](_.keyO, setTagKey)
      .at(TagInTree._tag).liftMapValues[Tag.Id]
      .at(second[T, B] ^|-> RevAnd._data[TagTree] ^|-> imapToMapLens)
    issues + tags
  }

  lazy val project =
    for {
      (issues, tags) <- Gen.tuple2(customIssueTypes, revAndTagTree) map distinctHashRefKeys.run
      reqtypes       <- customReqTypes
      fields         <- revAnd(fieldSet(reqtypes.data.keySet))
    } yield Project(issues, reqtypes, fields, tags)

  // -------------------------------------------------------------------------------------------------------------------
  // Protocol
  object protocol {
    import shipreq.webapp.base.protocol.{FieldProtocol => FP, _}
    import Gen.Covariance._

    lazy val deletionAction =
      Gen.oneofL(DeletionAction.values)

    lazy val reqTypeId: Gen[ReqType.Id] =
      Gen.oneofG(customReqTypeId, staticReqType)

    lazy val fieldId: Gen[Field.Id] =
      Gen.oneofG(customFieldId, staticField)

    lazy val applicableReqTypes: Gen[ApplicableReqTypes] =
      isubset(reqTypeId, reqTypeId.set)

    lazy val fieldPosition: Gen[FP.Position] =
      fieldId.option

    lazy val textFieldValues =
      Gen.apply4(FP.TextFieldValues.apply)(shortText1, fieldRefKey, mandatory, applicableReqTypes)

    lazy val fieldValues: Gen[FP.Values] =
      Gen.oneofG(textFieldValues)

    lazy val fieldDelta: Gen[FP.Delta] =
      Gen.apply2(FP.Delta.apply)(staticField \/ customField(applicableReqTypes), fieldPosition)

    object fieldCfgAction {
      import FP.CfgAction, CfgAction._
      lazy val create      : Gen[Create]       = fieldValues map Create
      lazy val updateValues: Gen[UpdateValues] = Gen.apply2(UpdateValues)(customFieldId, fieldValues)
      lazy val updateOrder : Gen[UpdateOrder]  = Gen.apply2(UpdateOrder)(fieldId, fieldPosition)
      lazy val delete      : Gen[Delete]       = Gen.apply2(Delete)(fieldId, deletionAction)
      lazy val any         : Gen[CfgAction]    = Gen.oneofG(create, updateValues, updateOrder, delete)
    }

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
  }

  // -------------------------------------------------------------------------------------------------------------------
  object remoteDeltaG {
    import shipreq.webapp.base.protocol._
    import RandomData.protocol._

    def forPart: Partition => Gen[RemoteDeltaG] = {
      case Partition.CustomIssueTypes => customIssueTypesDG
      case Partition.CustomReqTypes   => customReqTypesDG
      case Partition.Fields           => fieldsDG
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

    lazy val fieldsDG =
      generic(Partition.Fields)(fieldId, fieldDelta)

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
    import shipreq.webapp.base.protocol._
    import Routine._, Routines._
    import RandomData.protocol._

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val projectSPA =
      Gen.apply6(ProjectSPA)(
        remote(ProjectInit),
        remote(CustomIssueTypeCrud),
        remote(CustomReqTypeCrud),
        remote(CustomReqTypeImplicationMod),
        remote(FieldCrud),
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
      Gen.tuple2(hashRefKey, optionalLargeText))

    lazy val customReqTypeCrud = new CrudActionGens(CustomReqTypeCrud)(
      RandomData.customReqTypeId,
      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))

    lazy val tagCrud =
      new CrudActionGens(TagCrud)(RandomData.tagId, tagCrudInput)
  }
}
