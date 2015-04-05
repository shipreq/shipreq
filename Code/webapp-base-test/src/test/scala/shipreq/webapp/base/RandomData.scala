package shipreq.webapp.base

import com.nicta.rng.Rng
import japgolly.nyaya.util._
import japgolly.nyaya.test.{Distinct, Gen, GenS}
import monocle.Lens
import monocle.function.{first, second, third}
import monocle.std.tuple2._
import monocle.std.tuple3._
import scala.annotation.tailrec
import scala.collection.GenTraversable
import scalaz.{NonEmptyList, OneAnd, StateT, Name, Need}
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._
import scalaz.std.vector._

import shipreq.base.util.{BiMap, IMap}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.base.util.Debug._
import shipreq.webapp.base.data._, ReqType.Mnemonic, Field.ApplicableReqTypes
import shipreq.webapp.base.delta._
import shipreq.webapp.base.text.{Text, Grammar}
import DataImplicits._
import ReqFieldData.{Implications, ImplicationsU}

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {

  type StateG[S, A] = StateT[Gen, S, A]
  implicit def gliftS[S, A](g: Gen[A]): StateG[S, A] = StateT(s => g.map(a => (s,a)))

  def someOfWithDups[A, B](as: Seq[A])(f: A => Gen[B]): Gen[Vector[B]] =
    Gen.oneofO(as).fold[Gen[Vector[B]]](Gen insert Vector.empty)(
      _.vector.flatMap(Gen.traverse(_)(f)))

  lazy val id =
    Gen.positivelong

  def shortText1 =
    Gen.string1.lim(AppConsts.shortTextMaxLength)

  def shortText =
    Gen.string.lim(AppConsts.shortTextMaxLength)

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

  def revAndIMap[D, I <: TaggedLong](r: Gen[D])(mod: List[D] => List[D])(implicit i: DataIdMAux[D, I]): Gen[RevAnd[IMap[I, D]]] = {
    val d = distinctId[D, I].lift[List]
    val f = mod compose d.run
    val g = f andThen (i.emptyIMap ++ _)
    revAnd(r.list map g)
  }

  def distinctId[D, I <: TaggedLong](implicit i: DataIdMAux[D, I]) =
    Distinct.flong.xmap(i.mkId)(_.value).distinct.contramap[D](i.id, i.setId)

  def isubset[F[_], A](ga: Gen[A], gf: Gen[F[A]]): Gen[ISubset[F, A]] = {
    def h(k: OneAnd[F, A] => ISubset[F, A]) = gf.flatMap(f => ga.map(a => k(OneAnd(a, f))))
    Gen.oneofG(
      Gen.insert(ISubset.All()),
      h(ISubset.Only.apply),
      h(ISubset.Not.apply))
  }

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  lazy val alive =
    Gen.oneof[Alive](Alive, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  lazy val mandatory =
    Gen.oneof[Mandatory](Mandatory, Mandatory.Not)

  lazy val hashRefKey =
    for {
      h <- Gen.alphanumeric
      t <- Gen.charof('.', "_=-", 'a' to 'z', 'A' to 'Z', '0' to '9').list.lim(Grammar.hashRefKeyLength.end - 1)
    } yield HashRefKey((h :: t).mkString)

  // -------------------------------------------------------------------------------------------------------------------
  // Custom issue types

  lazy val customIssueTypeId =
    id map CustomIssueType.Id

  lazy val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, hashRefKey, optionalLargeText, alive)

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val customIssueTypes =
    revAndIMap(customIssueType)(identity)

  // -------------------------------------------------------------------------------------------------------------------
  // ReqTypes

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    id map CustomReqType.Id

  lazy val staticReqType: Gen[StaticReqType] =
    Gen.oneofL(StaticReqType.values)

  lazy val reqTypeId: Gen[ReqType.Id] =
    Gen.oneofG(staticReqType.subst, customReqTypeId.subst)

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
    def dname = Distinct.str.at(CustomReqType.name)
    def dmnemonic = {
      val distm = Distinct.fstr.xmap(Mnemonic.apply)(_.value).addhs(StaticReqType.mnemonics).distinct
      val cur = distm.at(CustomReqType.mnemonic)
      val old = distm.lift[Set].at(CustomReqType.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(customReqType)(d.run)
  }

  val staticReqTypeIdSet = StaticReqType.values.list.toSet[ReqType.Id]

  // -------------------------------------------------------------------------------------------------------------------
  // Tags

  lazy val tagGroupId =
    id map TagGroup.Id

  lazy val applicableTagId =
    id map ApplicableTag.Id

  lazy val tagId: Gen[Tag.Id] = {
    import Gen.Covariance._
    Gen.oneofG(tagGroupId, applicableTagId)
  }

  lazy val mutexChildren =
    Gen.oneof[MutexChildren](MutexChildren, MutexChildren.Not)

  def tagName =
    shortText1

  lazy val tagGroup =
    Gen.apply5(TagGroup.apply)(tagGroupId, tagName, optionalLargeText, mutexChildren, alive)

  lazy val applicableTag =
    Gen.apply5(ApplicableTag.apply)(applicableTagId, tagName, optionalLargeText, hashRefKey, alive)

  lazy val tag =
    Gen.oneofG[Tag](tagGroup.subst, applicableTag.subst)

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val tags: Gen[List[Tag]] = {
    val di = distinctId[Tag, Tag.Id]
    val dn = Distinct.str.at(Tag.name)
    val d = (di * dn).lift[List]
    tag.list map d.run
  }

  type TagTreeStructure = Map[Tag.Id, Vector[Tag.Id]]

  @tailrec
  def preventTagTreeCycles(m: TagTreeStructure /*, i: Int = 0*/): TagTreeStructure =
    Tag.CycleDetectors.multimap.findCycle(m) match {
      case None     =>
        // println(s"No cycles after $i attempts @ size ${m.keyCount}→${m.valueCount}")
        m
      case Some((a, b)) =>
//        println(s"Found cycle #$i [$a→$b] in ${m.m}")
//        preventCycles(m.del(a, b).del(b, a), i + 1) // better but slowwwwww
        preventTagTreeCycles(m - b /*, i + 1*/)
    }

  def tagTreeStructure(tags: Set[Tag.Id]): Gen[TagTreeStructure] =
    if (tags.isEmpty)
      Gen.insert(Map.empty)
    else {
      val tagsSeq = tags.toSeq
      val idset = Gen.oneof(tagsSeq.head, tagsSeq.tail: _*).set
      idset.map(_.toStream)
        .flatMap(ks => Gen sequence ks.map(k => idset.map(ids => (k, (ids - k).toVector)).sup))
        .map(s => preventTagTreeCycles(s.toMap))
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

  // -------------------------------------------------------------------------------------------------------------------
  // Fields

  lazy val staticField: Gen[StaticField] =
    Gen.oneofL(StaticField.values)

  def applicableReqTypes(r: Set[CustomReqType.Id]): Gen[ApplicableReqTypes] = {
    val all = StaticReqType.values.list.foldLeft(r.map(a => a: ReqType.Id))(_ + _).toList
    val a = Gen.oneof(all.head, all.tail: _*)
    isubset(a, a.set)
  }

  lazy val customFieldTextId =
    id map CustomField.Text.Id

  lazy val customFieldTagId =
    id map CustomField.Tag.Id

  lazy val customFieldImplicationId =
    id map CustomField.Implication.Id

  lazy val customFieldId: Gen[CustomField.Id] = {
    import Gen.Covariance._
    Gen.oneofG(customFieldTextId, customFieldTagId, customFieldImplicationId)
  }

  lazy val fieldRefKey =
    for {
      h <- Gen.lower
      t <- Gen.charof('_', "", 'a' to 'z', '0' to '9').list.lim(Grammar.fieldRefKeyLength.end - 1)
    } yield FieldRefKey((h :: t).mkString)

  def customFieldType =
    Gen.oneofL(CustomFieldType.values)

  def customFieldText(art: Gen[ApplicableReqTypes]): Gen[CustomField.Text] =
    Gen.apply6(CustomField.Text.apply)(customFieldTextId, shortText1, fieldRefKey, mandatory, art, alive)

  def customFieldTag(tagId: Gen[Tag.Id], art: Gen[ApplicableReqTypes]): Gen[CustomField.Tag] =
    Gen.apply5(CustomField.Tag.apply)(customFieldTagId, tagId, mandatory, art, alive)

  def customFieldTagSome(tagIds: Set[Tag.Id], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Tag]] =
    Gen.subset(tagIds).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldTag(Gen insert id, art)))

  def customFieldImplication(reqTypeId: Gen[ReqType.Id], art: Gen[ApplicableReqTypes]): Gen[CustomField.Implication] =
    Gen.apply5(CustomField.Implication.apply)(customFieldImplicationId, reqTypeId, mandatory, art, alive)

  def customFieldImplicationSome(reqTypeIds: Set[ReqType.Id], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Implication]] =
    Gen.subset(reqTypeIds).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldImplication(Gen insert id, art)))

  def customField(art: Gen[ApplicableReqTypes],
                  impFields: Boolean,
                  tagFields: Boolean): Gen[CustomField] = {
    import Gen.Covariance._
    lazy val txt: Gen[CustomField] = customFieldText(art)
    customFieldType.flatMap {
      case CustomFieldType.Text        => txt
      case CustomFieldType.Tag         => if (tagFields) customFieldTag(tagId, art) else txt
      case CustomFieldType.Implication => if (impFields) customFieldImplication(reqTypeId, art) else txt
    }
  }

  def customFields(reqTypeIds: Set[ReqType.Id], tagIds: Set[Tag.Id], art: Gen[ApplicableReqTypes]): Gen[IMap[CustomField.Id, CustomField]] = {
    val cf = for {
      f1 <- customField(art, false, false).stream
      f2 <- customFieldTagSome(tagIds, art)
      f3 <- customFieldImplicationSome(reqTypeIds, art)
    } yield f3.toStream #::: f2.toStream #::: f1
    def id   = distinctId(CustomField.IdAccess)
    def name = Distinct.str.at(CustomField.independentName)
    def key  = Distinct.fstr.xmap(FieldRefKey.apply)(_.value).distinct.at(CustomField.key)
    val dist = (id * name * key).lift[Stream]
    cf.map(fs => emptyDataMap(CustomField) ++ dist.run(fs))
  }

  def fieldSet(reqTypeIds: Set[ReqType.Id], tagIds: Set[Tag.Id], r: Set[CustomReqType.Id]): Gen[FieldSet] =
    for {
      cf           ← customFields(reqTypeIds, tagIds, applicableReqTypes(r))
      mandatoryIds = cf.keySet.map(f => f: Field.Id) ++ StaticField.notDeletable
      optionalIds  ← Gen.oneof(StaticField.deletable.head, StaticField.deletable.tail: _*).set
      order        ← Gen.shuffle((mandatoryIds ++ optionalIds).toVector)
    } yield FieldSet(cf, order)

  // -------------------------------------------------------------------------------------------------------------------
  // Text

  object TextGen {
    import Text._, Generic._
    import Gen.Covariance._

    // private[this] implicit def autoSomeG[A](g: Gen[A]) = g.some
    private[this] implicit class NELExt[T <: Generic](val _nel: NonEmptyList[Gen[T#Atom]]) extends AnyVal {
      def <+(o: Option[Gen[T#Atom]]): NonEmptyList[Gen[T#Atom]] =
        o.fold(_nel)(_ <:: _nel)
    }

    private[this] def literal(implicit t: Literal): Gen[t.Literal] =
      Gen.string1.map(t.Literal)

    private[this] def newLine(implicit t: NewLine): Gen[t.NewLine] =
      Gen.insert(t.newLine)

    private[this] def listItem[T <: ListMarkup](g: Name[Gen[T#Atom]]): Gen[T#ListItem] =
      Gen.insert(g).flatMap(_.value).list.lim(MaxTextAtoms)

    private[this] def listItems[T <: ListMarkup](g: Name[Gen[T#Atom]]): Gen[NonEmptyList[T#ListItem]] =
      listItem(g).list1.lim(20)

    private[this] def unorderedList(t: ListMarkup)(g: Name[Gen[t.Atom]]): Gen[t.UnorderedList] =
      listItems(g) map t.UnorderedList

    private[this] def webAddress(implicit t: PlainTextMarkup): Gen[t.WebAddress] =
      for {
        a <- Gen.lowerstring1
        b <- Gen.string1
      } yield t.WebAddress(a + "://" + b)

    private[this] def emailAddress(implicit t: PlainTextMarkup): Gen[t.EmailAddress] =
      for {
        (a, b) <- Gen.charof('.', "_+", 'a' to 'z', 'A' to 'Z', '0' to '9').list1.pair
      } yield t.EmailAddress((a.list ::: '@' :: b.list).mkString)

    private[this] def mathTex(implicit t: PlainTextMarkup): Gen[t.MathTeX] =
      Gen.string1.map(t.MathTeX)

    private[this] def plainTextMarkup(implicit t: PlainTextMarkup): Gen[t.Atom] =
      Gen.oneofG(webAddress, emailAddress, mathTex)

    private[this] def singleLine(implicit t: SingleLine): NonEmptyList[Gen[t.Atom]] =
      NonEmptyList(literal, plainTextMarkup)

    /** Probability [0,9] of an increase in recursive depth. */
    val DepthIncrease: Array[Int] = Array(5, 1, 1, 1) `JVM|JS` Array(3, 1)

    private[this] def multiLine(t: MultiLine, depth: Int)(g: Name[Gen[t.Atom]]): NonEmptyList[(Int, Gen[t.Atom])] = {
      type G  = Gen[t.Atom]
      type IG = (Int, G)
      var gs = singleLine(t).map[IG]((9, _)) :::> List[IG](
                 (9, newLine(t)))
      if (depth < DepthIncrease.length)
        gs = (DepthIncrease(depth), unorderedList(t)(g): G) <:: gs
      gs
    }

    private[this] def multiLinePlusI(t: MultiLine)(plus: (Int, Gen[t.Atom])*): Gen[t.Atom] = {
      type G  = Gen[t.Atom]
      type IG = (Int, G)

      val plusL = plus.toList

      lazy val lvls: Vector[Need[G]] =
        (0 to DepthIncrease.length)
          .toVector
          .map(i => Need[G](
            Gen.frequencyL(
              multiLine(t, i)(Name(lvls(i + 1).value)) :::> plusL
        )))

      lvls(0).value
    }

    private[this] def multiLinePlus(t: MultiLine)(plus: Option[Gen[t.Atom]]*): Gen[t.Atom] =
      multiLinePlusI(t)(
        plus.foldLeft[List[(Int, Gen[t.Atom])]](Nil)((q, o) =>
          o.fold(q)(g => (9, g) :: q)): _*)

    private[this] def reqRef(g: Gen[Req.Id])(implicit t: ReqRef): Gen[t.ReqRef] =
      g map t.ReqRef

    private[this] def tagRef(g: Gen[ApplicableTag.Id])(implicit t: TagRef): Gen[t.TagRef] =
      g map t.TagRef

    private[this] def issue(i: Gen[CustomIssueType.Id], r: Option[Gen[Req.Id]])(implicit t: Issue): Gen[t.Issue] =
      Gen.apply2(t.Issue)(i, inlineIssueDescAtom(r).list)

    private[this] def reqTitle(t: ReqTitle)(r: Option[Gen[Req.Id]], i: Option[Gen[CustomIssueType.Id]]): Gen[t.Atom] = {
      @inline implicit def tt: t.type = t
      val gs = singleLine(t) <+ r.map(reqRef(_)) <+ i.map(issue(_, r))
      Gen oneofGL gs
    }

    // Specific text types

    def recCodeGroupDescAtom = reqTitle(RecCodeGroupDesc) _

    def genericReqDescAtom   = reqTitle(GenericReqDesc) _

    def inlineIssueDescAtom(r: Option[Gen[Req.Id]]): Gen[InlineIssueDesc.Atom] = {
      @inline implicit def t: InlineIssueDesc.type = InlineIssueDesc
      val gs = singleLine(t) <+ r.map(reqRef(_))
      Gen oneofGL gs
    }

    def customTextFieldAtom(gr: Option[Gen[Req.Id]],
                            gi: Option[Gen[CustomIssueType.Id]],
                            gt: Option[Gen[ApplicableTag.Id]]): Gen[CustomTextField.Atom] = {
      @inline implicit def t: CustomTextField.type = CustomTextField
      multiLinePlus(t)(gr.map(reqRef(_)), gi.map(issue(_, gr)), gt.map(tagRef(_)))
    }
  }

  val MaxTextAtoms = 30 `JVM|JS` 8

  val MaxTextAtomsInProject = 6 `JVM|JS` 2

  implicit class TextGenExt[T <: Text.Generic](val g: Gen[T#Atom]) extends AnyVal {
    def text : GenS[T#OptionalText] = g.list lim MaxTextAtoms
    def text1: GenS[T#NonEmptyText] = g.list1 lim MaxTextAtoms

    def ptext : GenS[T#OptionalText] = g.list lim MaxTextAtomsInProject
    def ptext1: GenS[T#NonEmptyText] = g.list1 lim MaxTextAtomsInProject
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Requirements

  lazy val genericReqId =
    id map GenericReq.Id

  lazy val reqId: Gen[Req.Id] = {
    import Gen.Covariance._
    Gen.oneofG(genericReqId)
  }

  def pubidS(reqTypeIds: NonEmptyList[ReqType.Id])(reqId: Req.Id): StateG[Pubid.Register, Pubid] =
    StateT(register =>
      Gen.oneofL(reqTypeIds).map(reqTypeId =>
        Pubid.alloc(reqId, reqTypeId, register)))

  def genericReqIdS(pubidS: Req.Id => StateG[Pubid.Register, Pubid]): StateG[Pubid.Register, Req.Id] =
    for {
      id <- genericReqId |> gliftS[Pubid.Register, GenericReq.Id]
      _  <- pubidS(id)
    } yield id

  def genericReqS(pubidS: Req.Id => StateG[Pubid.Register, Pubid]): StateG[Pubid.Register, GenericReq] =
    for {
      id    <- genericReqId |> gliftS[Pubid.Register, GenericReq.Id]
      pubid <- pubidS(id)
      desc  <- TextGen.genericReqDescAtom(None, None).text // TODO GenericReq.desc needs more input
      live  <- alive
    } yield GenericReq(id, pubid, desc, live)

  def pubidRegisterAnd[A, B](inita: A, genb: StateG[Pubid.Register, B])
                            (f: (A, B) => A): GenS[(Pubid.Register, A)] = {
    val init = StateT.stateT[Gen, Pubid.Register, A](inita)
    GenS.choosesize flatMap { sz =>
      val prog = Stream.fill(sz)(genb).foldLeft(init)((sn, ga) =>
        for {
          b <- sn
          a <- ga
        } yield f(b, a)
      )
      prog(Pubid.emptyRegister)
    }
  }

  def pubidRegisterAndIds(reqTypeIds: NonEmptyList[ReqType.Id]): GenS[(Pubid.Register, Set[Req.Id])] =
    pubidRegisterAnd(Set.empty[Req.Id], genericReqIdS(pubidS(reqTypeIds)))(_ + _)

  def requirements(reqTypeIds: NonEmptyList[ReqType.Id]): GenS[Requirements] =
    pubidRegisterAnd(Req.IdAccess.emptyIMap, genericReqS(pubidS(reqTypeIds)))(_ + _)
      .map { case (pr, reqs) => Requirements(reqs, pr) }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Data

  def reqFieldDataText(cols: Set[CustomField.Text.Id], reqs: Set[Req.Id], txt: Gen[Text.CustomTextField.NonEmptyText]): Gen[ReqFieldData.Text] =
    txt mapByKeySubset reqs mapByKeySubset cols

  def reqFieldDataTags(reqs: TraversableOnce[Req.Id], tags: Set[ApplicableTag.Id]): Gen[ReqFieldData.Tags] = {
    val rndTags = Gen.subset(tags).map(_.toSet)
    (rndTags mapByKeySubset reqs).map(Multimap(_))
  }

  type ImplicationsUM = Map[Req.Id, Set[Req.Id]]
  @tailrec def preventImplicationCycles(m: ImplicationsUM): ImplicationsUM =
    ReqFieldData.implicationCycleDetector.findCycle(m) match {
      case None         => m
      case Some((a, b)) => preventImplicationCycles(m - b)
    }

  val emptyImplicationsU: ImplicationsU = Multimap.empty

  val MaxImplicationPairs = 100 `JVM|JS` 40
  // val MaxImplicationsPerSrc = 2  `JVM|JS` 4
  // val MaxImplicationKeys    = 10 `JVM|JS` 4

  def reqFieldDataImplications(reqIds: Set[Req.Id]): Gen[Implications] = {
    def fix(m: ImplicationsUM): Implications = {
      val m2 = preventImplicationCycles(m)
      // println(m2); println()
      Implications(Multimap(m2))
    }

    def method1(g: Gen[Req.Id]) =
      g.pair
        .list.lim(MaxImplicationPairs)
        .map(kvs => emptyImplicationsU.addPairs(kvs: _*).m |> fix)

//    def method2(g: Gen[Req.Id]) =
//      Gen.tuple2(g, g.set1 lim MaxImplicationsPerSrc)
//        .list.lim(MaxImplicationKeys)
//        .map(_.toMap |> fix)

    Gen.oneofO(reqIds.toSeq) match {
      case Some(g) => method1(g)
      case None    => Gen insert Implications(emptyImplicationsU)
    }
  }

  // def customTextFieldAtom(gr: Gen[Req.Id], gi: Gen[CustomIssueType.Id], gt: Gen[ApplicableTag.Id]): Gen[CustomTextField.Atom] = {
  def reqFieldData(reqs   : Set[Req.Id],
                   txtCols: Set[CustomField.Text.Id],
                   cissues: Set[CustomIssueType.Id],
                   tags   : Set[ApplicableTag.Id]): Gen[ReqFieldData] = {

    val gr = Gen.oneofO(reqs.toSeq)
    val gt = Gen.oneofO(tags.toSeq)
    val gi = Gen.oneofO(cissues.toSeq)

    Gen.apply3(ReqFieldData.apply)(
      reqFieldDataText(txtCols, reqs, TextGen.customTextFieldAtom(gr, gi, gt).ptext1),
      reqFieldDataTags(reqs, tags),
      reqFieldDataImplications(reqs))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Codes

  lazy val reqCodeNode: Gen[ReqCode.Node] =
    Gen.charof('_', "", 'a' to 'z', '0' to '9').list1.lim(Grammar.reqCodeNodeLength.max)
      .map(cs => ReqCode.Node(cs.list.mkString))

  lazy val reqCode: GenS[ReqCode] =
    reqCodeNode.list1.map(ReqCode.apply)

  lazy val reqCodeDFixer = {
    def fix(ss: Set[ReqCode]): ReqCode = {
      var c = ss.head
      while (ss contains c) {
        val n1 = c.backwards.head
        val n2 = ReqCode.Node(n1.value + "x")
        c = ReqCode(NonEmptyList.nel(n2, c.backwards.tail))
      }
      c
    }
    Distinct.Fixer lift fix
  }

  def reqCodeTrie(possibleTargets: Seq[ReqCode.Target]) = GenS[ReqCode.Trie] { sz =>
    import ReqCode._
    type FlatValue = (Target, ReqCode)
    someOfWithDups(possibleTargets)(reqCode.strengthL)
      .map(reqCodeDFixer.distinct.at(second[FlatValue, ReqCode]).lift[Vector].run)
      .map(_.foldLeft(Trie.empty) { case (t, (tgt, c)) => Trie.putCF(t, c.backwards)(tgt) })
  }

  def reqCodes(g: Gen[ReqCode.Trie]) =
    revAnd(g map ReqCodes.apply)

  // -------------------------------------------------------------------------------------------------------------------
  // Project

  def distinctHashRefKeys = {
    type A = RevAnd[CustomIssueTypeIMap]
    type B = RevAnd[TagTree]
    type T = (A, B)
    val keyDist = Distinct.fstr.xmap(HashRefKey.apply)(_.value).distinct
    val issues = keyDist
      .at(CustomIssueType.key).liftMapValues[CustomIssueType.Id]
      .at(first[T, A] ^|-> RevAnd.data[CustomIssueTypeIMap] ^|-> imapToMapLens)
    val tags = keyDist
      .lift[Option].contramap[Tag](_.keyO, setTagKey)
      .at(TagInTree.tag).liftMapValues[Tag.Id]
      .at(second[T, B] ^|-> RevAnd.data[TagTree] ^|-> imapToMapLens)
    issues + tags
  }

  lazy val project: Gen[Project] =
    for {
      (issues, tags) ← Gen.tuple2(customIssueTypes, revAndTagTree) map distinctHashRefKeys.run
      cissueIds      = issues.data.keySet
      reqtypes       ← customReqTypes
      reqTypeIds     = StaticReqType.values :::> reqtypes.data.keys.toList
      reqTypeIdSet   = reqTypeIds.list.toSet
      fields         ← revAnd(fieldSet(reqTypeIdSet, tags.data.keySet, reqtypes.data.keySet))
      reqs           ← revAnd(requirements(reqTypeIds))
      reqCodes       ← reqCodes(reqCodeTrie(reqs.data.reqs.keys.toSeq).lim(22 `JVM|JS` 8)) // TODO add SHRs
      atagIds        = tags.data.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
      textColIds     = fields.data.customFields.values.filterT[CustomField.Text].map(_.id).toSet
      reqIds         = reqs.data.reqs.keySet
      reqFieldData   ← revAnd(reqFieldData(reqIds, textColIds, cissueIds, atagIds))
    } yield Project(issues, reqtypes, fields, tags, reqs, reqCodes, reqFieldData)

  // ===================================================================================================================
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
      Gen.apply2(FP.Delta.apply)(staticField \/ customField(applicableReqTypes, true, true), fieldPosition)

    object fieldCfgAction {
      import FP.CfgAction, CfgAction._
      lazy val create      : Gen[Create]       = fieldValues map Create
      lazy val updateValues: Gen[UpdateValues] = Gen.apply2(UpdateValues)(customFieldId, fieldValues)
      lazy val updateOrder : Gen[UpdateOrder]  = Gen.apply2(UpdateOrder)(fieldId, fieldPosition)
      lazy val delete      : Gen[Delete]       = Gen.apply2(Delete)(fieldId, deletionAction)
      lazy val any         : Gen[CfgAction]    = Gen.oneofG(create, updateValues, updateOrder, delete)
    }

    def tagProtocolValues: Tag => TagProtocol.Values = {
      case TagGroup(_, n, d, mc, _)     => TagProtocol.TagGroupValues(n, mc, d)
      case ApplicableTag(_, n, d, k, _) => TagProtocol.ApplicableTagValues(n, k, d)
    }

    lazy val tagCrudInput =
      remoteDeltaG.povTag.flatMap(t => {
        val a = Gen insert tagProtocolValues(t.tag)
        val b = Gen insert t.rels
        a \&/ b
      })
  }

  // ===================================================================================================================
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

  // ===================================================================================================================
  object routines {
    import shipreq.webapp.base.protocol._
    import Routine._, Routines._
    import RandomData.protocol._

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val projectSPA =
      Gen.apply7(ProjectSPA)(
        remote(ProjectInit),
        remote(CustomIssueTypeCrud),
        remote(CustomReqTypeCrud),
        remote(ReqTypeImplicationMod),
        remote(FieldMandatorinessMod),
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
