package shipreq.webapp.base.data

import japgolly.nyaya._
import scala.reflect.ClassTag
import scalaz.syntax.equal._
import scalaz.std.AllInstances._
import shipreq.base.util._, MTrie.Ops, ScalaExt._, Debug._
import shipreq.webapp.base.text.{Atom, Text}
import TaggedTypes.TaggedInt
import DataImplicits._

object DataProp {
  implicit def autoLiftL(e: Eval) = e.liftL

  def id[T <: TaggedInt] =
    Prop.test[T]("id > 0", _.value > 0)

  def dataId[O, D, Id <: TaggedInt](o: O)(implicit O: ObjDataId[O, D, Id]) =
    id[Id].contramap[D](O.id)

  val rev =
    Prop.test[Rev]("rev ≥ 0", _.value >= 0)

  def justRevAnd[T]: Prop[RevAnd[T]] =
    rev.contramap[RevAnd[T]](_.rev)

  def revAnd[T](p: Prop[T]): Prop[RevAnd[T]] =
    justRevAnd ∧ p.contramap[RevAnd[T]](_.data)

  def must[A](name: => String): Prop[Must[A]] =
    Prop.atom[Must[A]](name, _.fold(Some(_), _ => None))

  def must[A, B](name: => String, f: A => Must[B]): Prop[A] =
    must[B](name).contramap(f)

  def mustThen[A](name: => String, ifExists: Prop[A]): Prop[Must[A]] =
    Prop.eval[Must[A]](m => m.fold(
      e => Eval.atom(name, m, Some(e)),
      a => ifExists(a).liftL))

  def isubsetContents[A]: ISubset[A] => Set[A] = {
    case ISubset.All()   => Set.empty[A]
    case ISubset.Only(v) => v.whole
    case ISubset.Not(v)  => v.whole
  }

  private implicit class MapStreamingExt[K, V](val m: Map[K, V]) extends AnyVal {
    @inline def vstream[A](f: V => A): Stream[A] = m.values.toStream.map(f)
    @inline def vstreamf[A](f: V => Stream[A]): Stream[A] = m.values.toStream.flatMap(f)
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customIssueTypes {
    type T = CustomIssueTypeIMap

    def ids =
      id[CustomIssueTypeId].forall((_: T).keys.toStream)

    def all = revAnd(ids) rename "CustomIssueTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customReqType {

    def id = dataId(CustomReqType)

    def reqType =
      Prop.test[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

    // starting to overlap with validation....
    def mnemonicStatic =
      Prop.blacklist[CustomReqType]("mnemonic doesn't overlap with static")(
        _ => StaticReqType.mnemonics, a => a.oldMnemonics + a.mnemonic)

    val all = id ∧ mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {
    type T = CustomReqTypeIMap

    def uniqueMnemonics =
      Prop.distinct("mnemonic", (_: T).values.toStream.flatMap(b => b.mnemonic #:: b.oldMnemonics.toStream).map(_.value))

    def uniqueNames =
      Prop.distinct("name", (_: T).values.toStream.map(_.name))

    def each =
      customReqType.all.forall[T, Stream](_.values.toStream)

    lazy val all =
      revAnd[T](uniqueMnemonics ∧ uniqueNames ∧ each) rename "CustomReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fields {

    type Fields = Vector[Field]

    def ids =
      id[CustomFieldId].forall((_: FieldSet).customFields.keys.toStream)

    def uniqueNames =
      Prop.distinct("name", (_: Fields).flatMap(_.independentName.toVector))

    def uniqueKeys =
      Prop.distinct("FieldRefKey", (_: Fields).flatMap(_.keyO.toVector))

    def fields =
      mustThen[Fields]("FieldSet.config.fields", uniqueNames ∧ uniqueKeys)
        .contramap[FieldSet](_.fields)

    def orderNoDups =
      Prop.distinct("order", (_: FieldSet).order)

    def orderCustomFieldsIso =
      Prop.equal[FieldSet]("order.customFields = fieldSet.customFields")(
        _.customFields.keySet,
        _.order.foldLeft(Set.empty[CustomFieldId])((q, id) => id match {
          case i: CustomFieldId => q + i
          case _: StaticField    => q
        }))

    def orderHasAllUndeletableStaticFields =
      Prop.allPresent[FieldSet]("order ⊇ undeletable static")(Function const StaticField.notDeletable.toSet, _.order)

    def filteredFields[T](f: PartialFunction[CustomField, T]): FieldSet => Stream[T] = {
      val ff = f.lift
      _.customFields.values.toStream.flatMap(ff(_).toStream)
    }

    def tagFieldsUnique =
      Prop.distinct("Tag field", filteredFields { case t: CustomField.Tag => t.tagId })

    def implicationFieldsUnique =
      Prop.distinct("Implication field", filteredFields { case t: CustomField.Implication => t.reqTypeId })

    def fieldSet = "FieldSet" rename_: (
      ids ∧ fields ∧
      orderNoDups ∧ orderCustomFieldsIso ∧ orderHasAllUndeletableStaticFields ∧
      tagFieldsUnique ∧ implicationFieldsUnique)

    lazy val all =
      revAnd(fieldSet) rename "Fields"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tags {
    type T = TagTree

    def ids =
      id[TagId].forall((_: T).keys.toStream)

    def uniqueNames =
      Prop.distinct("name", (_: T).vstream(_.tag.name))

    def uniqueSiblings =
      Prop.distinctC[Vector, TagId]("siblings").forall((_: T).vstream(_.children))

    def noCycles =
      Tag.CycleDetectors.tagTree.noCycleProp("structure")

    def noDeadLinks =
      Prop.whitelist[T]("ids refer to available tags")(_.keySet, _.vstreamf(_.children.toStream))

    def tagTree =
      (ids ∧ uniqueNames ∧ uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TagTree"

    lazy val all =
      revAnd(tagTree) rename "Tags"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqs {
    type T = Requirements

    def ids =
      id[ReqId].forall((_: T).reqs.keys.toStream)

    def reqPubidsInRegister =
      Prop.forall((_: T).reqs.values.toStream)(t =>
        Prop.equal[Req]("Req's pubid refers to itself in the Pubid register")(
          _.id.some,
          r => t.pubids(r.pubid)))

    def pubidsResolveToReqs =
      Prop.whitelist[T]("Pubid register")(_.reqs.keySet, _.pubids.value.m.vstreamf(_.toStream))


    def pubidReqTypeAssociations = {
      import StaticReqType._
      def test[T <: ReqTypeId](rt: T, reqIds: Vector[ReqId])(implicit reqIdT: ClassTag[ReqIdT[T]]): FailureReasonO =
        reqIds.toStream.map {
          case reqIdT(_) => None
          case reqId     => Some(s"Illegal association: $reqId to $rt")
        }.find(_.isDefined).flatten
      Prop.atom[PubidRegister]("Pubid reqtype-to-req associations",
        pr => pr.value.m.toStream.map {
          case (rt: CustomReqTypeId, reqIds) => test(rt, reqIds)
          case (rt@ UseCase        , reqIds) => test(rt, reqIds)
        }.find(_.isDefined).flatten
      ).contramap[T](_.pubids)
    }

    lazy val all =
      revAnd(ids ∧ reqPubidsInRegister ∧ pubidsResolveToReqs ∧ pubidReqTypeAssociations) rename "Requirements"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqCodes {
    type T = ReqCodes
    import ReqCode._
    type TrieBranch = MTrie.Branch[Node, Data]
    type Terminal = MTrie.Value[Node, Data]

    def branchesMustBranch =
      Prop.test[TrieBranch]("TrieBranch branches", _.next.nonEmpty)
        .forall[T, List](_.trie.cataN[List[TrieBranch]](Nil)((q, n) => n.fold(_ :: q, _ => q)))
        .rename("All TrieBranches branch")

    def nonEmptyData(d: Data) =
      d.active.nonEmpty || d.refsToGroup.nonEmpty || d.refsToReqs.nonEmpty

    def nonEmptyTerminals =
      Prop.test[Data]("Terminal not empty", nonEmptyData)
        .forall[T, List](_.trie.cataN[List[Data]](Nil)((q, n) => n.fold(_ => q, _.value :: q)))
        .rename("No empty terminals")

    def ids =
      id[ReqCodeId].forall((_: T).allIds)

    def uniqueIds =
      Prop.distinct("ID", (_: T).allIds)

    lazy val all =
      revAnd(branchesMustBranch ∧ nonEmptyTerminals ∧ uniqueIds ∧ ids) rename "ReqCodes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object implications {
    type T = Implications

    def noCycles =
      Implications.cycleDetector.noCycleProp("implications").contramap[T](_.srcToTgt.m)

    lazy val all = noCycles
  }

  // -------------------------------------------------------------------------------------------------------------------
  object text {
    import Atom._

    def show(s: String) = "'" + s.replace("\n", "\\n").replace("\r", "\\r") + "'"
    def contains(substr: String) = Prop.test[String](s"Contains ${show(substr)}", _ contains substr)
    def startsWith(substr: String) = Prop.test[String](s"Starts with ${show(substr)}", _ startsWith substr)
    def endsWith(substr: String) = Prop.test[String](s"Ends with ${show(substr)}", _ endsWith substr)
    def matches(regex: String) = {
      val p = regex.r.pattern
      Prop.test[String](s"Matches ${show(regex)}", p.matcher(_).matches())
    }
    def trimmed = ~startsWith(" ") & ~endsWith(" ")
    val noNLT = ~matches(".*[\r\n\t].*")
    val noWS = ~matches(".*[\r\n\t ].*")
    val nonEmpty = Prop.test[String](s"non-empty", _.nonEmpty)

    val isBlankLine: Prop[AnyAtom] =
      Prop.test("Is a BlankLine", { case _: NewLine#BlankLine => true; case _ => false })

    val literal =
      (nonEmpty ∧ noNLT).contramap[Literal#Literal](_.value)

    val webAddress =
      (nonEmpty ∧ noWS ∧ contains("://")).contramap[PlainTextMarkup#WebAddress](_.value)

    val emailAddress =
      (nonEmpty ∧ noWS ∧ contains("@")).contramap[PlainTextMarkup#EmailAddress](_.value)

    val mathtex =
      (nonEmpty ∧ trimmed).contramap[PlainTextMarkup#MathTeX](_.value)

    def nonEmptyText: Prop[Text.AnyNonEmpty] = {
      val litval: AnyAtom => String =
        { case a: Literal#Literal => a.value; case _ => "" }
      val head = "Doesn't start with whitespace" rename_: ~(isBlankLine | startsWith(" ").contramap(litval))
      val last = "Doesn't end with whitespace"   rename_: ~(isBlankLine | endsWith(" ").contramap(litval))
      head.contramap[Text.AnyNonEmpty](_.head) ∧ last.contramap[Text.AnyNonEmpty](_.last)
    }.rename("NonEmptyText")

    lazy val anyText: Prop[Text.AnyOptional] =
      anyAtom.forallF[Vector] ∧ nonEmptyText.forallF[Option].contramap(NonEmptyVector.option)

    lazy val anyTextV: Prop[Vector[Text.AnyOptional]] = anyText.forallF
    lazy val anyTextS: Prop[Stream[Text.AnyOptional]] = anyText.forallF

    val nop = Eval.pass()

    lazy val anyAtom: Prop[AnyAtom] = Prop.eval[AnyAtom] {
      case a: Literal         # Literal       => literal(a)
      case _: NewLine         # BlankLine     => nop
      case a: PlainTextMarkup # WebAddress    => webAddress(a)
      case a: PlainTextMarkup # EmailAddress  => emailAddress(a)
      case a: PlainTextMarkup # MathTeX       => mathtex(a)
      case a: ListMarkup      # UnorderedList => anyTextV(a.items.whole)
      case _: ReqRef          # ReqRef        => nop
      case _: ReqRef          # CodeRef       => nop
      case a: Issue           # Issue         => anyText(a.desc)
      case _: TagRef          # TagRef        => nop
    } rename "AnyAtom"
  }

  // ===================================================================================================================
  object projectConfig {
    type P = ProjectConfig

    case class Refs(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId])

    def constituents = (
        customIssueTypes.all.contramap[P](_.customIssueTypes)
      ∧   customReqTypes.all.contramap[P](_.customReqTypes)
      ∧           fields.all.contramap[P](_.fields)
      ∧             tags.all.contramap[P](_.tags)
    ) rename "constituents"

    def uniqueHashRefKeys =
      Prop.distinct[P, String]("HashRefKey", p => (
          p.customIssueTypes.data.values.toStream.map(_.key) append
          p.tags.data.vstreamf(_.tag.keyO.toStream)
        ).map(_.value.toLowerCase))

    def liveTagFieldRequiresLiveTag =
      Prop.whitelist[P]("Live tag-field requires a live tag")(
        _.tags.data.values.filter(_.tag.live :: Live).map(_.id).toSet,
        p => fields.filteredFields({ case t: CustomField.Tag if t.live :: Live => t.tagId})(p.fields.data))

    def liveImpFieldRequiresLiveReqType =
      Prop.whitelist[P]("Live implication-field requires a live req-type")(
        _.reqTypes.filter(_.live :: Live).map(_.reqTypeId).toSet,
        p => fields.filteredFields({ case t: CustomField.Implication if t.live :: Live => t.reqTypeId})(p.fields.data))

    def validRefs = {
      type TR = (P, Refs)

      def mkRefs(p: ProjectConfig): Refs = Refs(
        p.reqTypes.map(_.reqTypeId).toSet,
        p.tags.data.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => Traversable[A]) =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _

      (  validReqTypeIds("Field.reqTypes",
          _.fields.data.customFields.values.toStream.flatMap(f => isubsetContents(f.reqTypes).toStream))
      ∧ validTagIds("CustomField.Tag.tagIds",
        p => fields.filteredFields({ case t: CustomField.Tag => t.tagId})(p.fields.data))
      ∧ validReqTypeIds("CustomField.Implication.reqTypeIds",
          p => fields.filteredFields({ case t: CustomField.Implication => t.reqTypeId})(p.fields.data))
      ).rename("Cross-constituent refs").contramap[P](_ mapStrengthR mkRefs)
    }

    val all: Prop[ProjectConfig] = "ProjectConfig" rename_: (
      constituents ∧ uniqueHashRefKeys ∧ validRefs ∧ liveTagFieldRequiresLiveTag ∧ liveImpFieldRequiresLiveReqType)
  }

  // ===================================================================================================================
  object project {
    type P = Project

    case class Refs(fieldIds: Set[CustomFieldId], reqIds: Set[ReqId], reqCodeIds: Set[ReqCodeId],
                    reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId])

    def atoms =
      Prop.eval[(String, Stream[Text.AnyOptional])](t => text.anyTextS(t._2).rename(t._1))
        .forallF[Stream].contramap[P](_.allRichText) rename "Atoms"

    def constituents = (
                reqs.all.contramap[P](_.reqs)
      ∧     reqCodes.all.contramap[P](_.reqCodes)
      ∧ implications.all.contramap[P](_.implications.data)
    ) rename "constituents"

    def liveReqRequiresLiveReqType =
      Prop.whitelist[Project]("Live Req requires Live ReqType")(
        _.config.reqTypes.filter(_.live :: Live).map(_.reqTypeId).toSet,
        _.reqs.data.reqs.values.toStream.filter(_.live :: Live).map(_.reqTypeId))

    def liveReqCodeRequiresLiveTarget =
      Prop.whitelist[Project]("Live ReqCode requires Live Target")(
        _.reqs.data.reqs.values.toStream.filter(_.live :: Live).map(_.id).toSet,
        _.reqCodes.data.cataA(UnivEq.emptySet[ReqId])((q, _, a) => a.target match {
          case id: ReqId       => q + id
          case _: ReqCodeGroup => q
        }))

    def validRefs = {
      type TR = (P, Refs)
      import Atom._

      def mkRefs(p: Project): Refs = Refs(
        p.config.fields.data.customFields.keySet,
        p.reqs.data.reqs.vstream(_.id).toSet,
        p.reqCodes.data.allIds.toSet,
        p.config.reqTypes.map(_.reqTypeId).toSet,
        p.config.tags.data.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => Traversable[A]) =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validFieldIds   = whitelist(_._2.fieldIds) _
      def validReqIds     = whitelist(_._2.reqIds) _
      def validReqCodeIds = whitelist(_._2.reqCodeIds) _
      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _
      def validIssueTypes = whitelist(_._1.config.customIssueTypes.data.keySet) _

      def inText[A](f: PartialFunction[AnyAtom, A]): P => Traversable[A] = {
        def go(a0: AnyAtom): Stream[A] = a0 match {
          case a if f.isDefinedAt(a)         => f(a) +: Stream.empty
          case a: ListMarkup # UnorderedList => a.items.toStream.flatMap(_.toStream).flatMap(go)
          case a: Issue      # Issue         => a.desc.toStream.flatMap(go)
          case _                             => Stream.empty
        }
        _.allRichText.flatMap(_._2).flatMap(_.toStream).flatMap(go)
      }

      ( validReqTypeIds("Pubid keys",                 _.reqs.data.pubids.value.m.keys)
      ∧ validReqIds    ("ReqCode targets"             , _.reqCodes.data.trie.cataV(Set.empty[ReqId])((q, _, d) => q ++ d.reqIds))
      ∧ validFieldIds  ("ReqData.text TextField ids", _.reqText.data.keys)
      ∧ validReqIds    ("ReqData.text.*.reqIds",      _.reqText.data.vstreamf(_.keys.toStream))
      ∧ validReqIds    ("ReqData.config.tags keys",   _.reqTags.data.keys)
      ∧ validTagIds    ("ReqData.config.tags values", _.reqTags.data.allValues)
      ∧ validReqIds    ("ReqData.implications",       _.implications.data.members)
      ∧ validReqIds    ("Atoms: ReqRefs",             inText { case a: ReqRef # ReqRef  => a.value })
      ∧ validReqCodeIds("Atoms: CodeRefs",            inText { case a: ReqRef # CodeRef => a.value })
      ∧ validTagIds    ("Atoms: TagRefs",             inText { case a: TagRef # TagRef  => a.value })
      ∧ validIssueTypes("Atoms: Issues",              inText { case a: Issue  # Issue   => a.typ })
      ).rename("Cross-constituent refs").contramap[P](_ mapStrengthR mkRefs)
    }

    val allExcludingConfig: Prop[Project] = "Project" rename_: (
      constituents ∧ atoms ∧ liveReqRequiresLiveReqType ∧ liveReqCodeRequiresLiveTarget ∧ validRefs)

    val allIncludingConfig: Prop[Project] =
      allExcludingConfig ∧ projectConfig.all.contramap(_.config)
  }
}
