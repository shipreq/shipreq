
package shipreq.webapp.base.data

import nyaya.prop._
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaz.{Monoid, Foldable}
import scalaz.syntax.equal._
import scalaz.std.AllInstances._
import shipreq.base.util._, MTrie.Ops, ScalaExt._, Debug._
import shipreq.webapp.base.text.{Atom, Text}
import TaggedTypes.TaggedInt
import DataImplicits._
import ScalaExt._

object DataProp {
  implicit def autoLiftL(e: Eval) = e.liftL

  implicit val iteratorFoldable: Foldable[Iterator] =
    new Foldable[Iterator] {
      def foldMap[A, B](fa: Iterator[A])(f: A => B)(implicit F: Monoid[B]) =
        foldLeft(fa, F.zero)((x, y) => Monoid[B].append(x, f(y)))

      def foldRight[A, B](fa: Iterator[A], b: => B)(f: (A, => B) => B) =
        fa.foldRight(b)(f(_, _))

      override def foldLeft[A, B](fa: Iterator[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def any[A](fa: Iterator[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def all[A](fa: Iterator[A])(p: A => Boolean): Boolean =
        fa.forall(p)
    }

  def id[T <: TaggedInt] =
    Prop.test[T]("id > 0", _.value > 0)

  def dataId[O, D, Id <: TaggedInt](o: O)(implicit O: ObjDataId[O, D, Id]) =
    id[Id].contramap[D](O.id)

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
      id[CustomIssueTypeId].forall((_: T).keysIterator)

    def all = ids rename "CustomIssueTypes"
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
      customReqType.all.forall[T, Iterator](_.valuesIterator)

    val all =
      (uniqueMnemonics ∧ uniqueNames ∧ each) rename "CustomReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fields {

    type Fields = Vector[Field]

    def ids =
      id[CustomFieldId].forall((_: FieldSet).customFields.keysIterator)

    def uniqueNames =
      Prop.distinct("name", (_: Fields).flatMap(_.independentName.toVector))

    def uniqueKeys =
      Prop.distinct("FieldRefKey", (_: Fields).flatMap(_.keyO.toVector))

    def fields =
      (uniqueNames ∧ uniqueKeys).contramap[FieldSet](_.fields)

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

    val all =
      fieldSet rename "Fields"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tags {
    type T = TagTree

    def ids =
      id[TagId].forall((_: T).keysIterator)

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

    val all =
      tagTree rename "Tags"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqs {
    type T = Requirements

    def ids =
      id[ReqId].forall((_: T).reqs.keysIterator)

    def reqPubidsInRegister =
      Prop.forall((_: T).reqs.valuesIterator)(t =>
        Prop.equal[Req]("Req's pubid refers to itself in the Pubid register")(
          _.id.some,
          r => t.pubids(r.pubid)))

    def pubidsResolveToReqs =
      Prop.whitelist[T]("Pubid register")(_.reqs.keySet, _.pubids.value.m.vstreamf(_.toStream))


    def pubidReqTypeAssociations = {
      import StaticReqType._
      def test[T <: ReqTypeId](rt: T, reqIds: Vector[ReqId])(implicit reqIdT: ClassTag[ReqIdT[T]]): FailureReasonO =
        reqIds.iterator.map {
          case reqIdT(_) => None
          case reqId     => Some(s"Illegal association: $reqId to $rt")
        }.find(_.isDefined).flatten
      Prop.atom[PubidRegister]("Pubid reqtype-to-req associations",
        pr => pr.value.m.iterator.map {
          case (rt: CustomReqTypeId, reqIds) => test(rt, reqIds)
          case (rt@ UseCase        , reqIds) => test(rt, reqIds)
        }.find(_.isDefined).flatten
      ).contramap[T](_.pubids)
    }

    val all =
      (ids ∧ reqPubidsInRegister ∧ pubidsResolveToReqs ∧ pubidReqTypeAssociations) rename "Requirements"
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

    def nonEmptyData: Prop[Data] =
      Prop.test[Data]("Data not empty.", _.nonEmpty)

    def allData: Prop[T] =
      nonEmptyData
        .forall[T, List](_.trie.cataV[List[Data]](Nil)((q, _, d) => d :: q))

    def idFormat =
      id[ReqCodeId].forall((_: T).idList)

    def uniqueIds =
      Prop.distinct("ID", (_: T).idList)

    val all =
      (branchesMustBranch ∧ allData ∧ uniqueIds ∧ idFormat) rename "ReqCodes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object implications {
    @inline def all = Implications.acyclicPropBi
  }

  // -------------------------------------------------------------------------------------------------------------------
  object deletionReasons {
    type T = DeletionReasons

    def ids: Prop[T] =
      Prop.atom("Valid IDs", dr => {
        val len = dr.reasons.length
        var unreferenced = dr.reasons.indices.toSet

        val testIdFn = (id: DeletionReasonId) => {
          val n = id.value
          unreferenced -= n
          n >= 0 && n < len
        }

        val allPass = dr.reqApplication.values.forall(_.forall(_ forall testIdFn))

        if (!allPass) {
          val errors = dr.reqApplication.streamKV
            .filter(_._2.isDefined)
            .map(_.map2(_.get))
            .filterNot(x => testIdFn(x._2))
            .map(x => s"${x._1} → ${x._2}")
            .toVector
          Some(s"Found ${errors.size} invalid DeletionReasonIds: ${errors mkString ", "}")
        } else if (unreferenced.nonEmpty)
          Some(s"There exist deletion reasons with deletion targets: $unreferenced")
        else
          None
      })

    def reqApplicationVectors: Prop[T] = {
      @tailrec
      def go(v: Vector[Option[DeletionReasonId]], i: Int, prev: Int): Option[String] =
        if (i == -1)
          None
        else
          v(i) match {
            case Some(id) =>
              val n = id.value
              if (n < prev)
                go(v, i - 1, n)
              else
                Some(s"Not in order: $v")
            case None => go(v, i - 1, prev)
          }

      val test: Vector[Option[DeletionReasonId]] => Option[String] = v => {
        val l = v.length
        if (l < 2)
          None
        else
          go(v, l - 1, Int.MaxValue)
      }

      Prop.atom("reqApplication vectors",
        _.reqApplication.values.iterator.map(test).find(_.isDefined).flatten)
    }

    val all = ids ∧ reqApplicationVectors
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

    private val nop = Eval.pass()

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

    lazy val anyText: Prop[Text.AnyOptional] =
      anyAtom.forallF[Vector] ∧ nonEmptyText.forallF[Option].contramap(NonEmptyVector.option)

    lazy val anyTextV: Prop[Vector[Text.AnyOptional]] = anyText.forallF

    val anyTextI: Prop[Iterator[Text.AnyOptional]] = anyText.forallF
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
          p.customIssueTypes.values.toStream.map(_.key) append
          p.tags.vstreamf(_.tag.keyO.toStream)
        ).map(_.value.toLowerCase))

    def validRefs = {
      type TR = (P, Refs)

      def mkRefs(p: ProjectConfig): Refs = Refs(
        p.reqTypes.map(_.reqTypeId)(collection.breakOut),
        p.tags.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => Traversable[A]) =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _

      (  validReqTypeIds("Field.reqTypes",
          _.fields.customFields.values.toStream.flatMap(f => isubsetContents(f.reqTypes).toStream))
      ∧ validTagIds("CustomField.Tag.tagIds",
        p => fields.filteredFields({ case t: CustomField.Tag => t.tagId})(p.fields))
      ∧ validReqTypeIds("CustomField.Implication.reqTypeIds",
          p => fields.filteredFields({ case t: CustomField.Implication => t.reqTypeId})(p.fields))
      ).rename("Cross-constituent refs").contramap[P](_ mapStrengthR mkRefs)
    }

    val all: Prop[ProjectConfig] = "ProjectConfig" rename_: (
      constituents ∧ uniqueHashRefKeys ∧ validRefs)
  }

  // ===================================================================================================================
  object project {
    type P = Project

    case class Refs(fieldIds: Set[CustomFieldId], reqIds: Set[ReqId], reqCodeIds: Set[ReqCodeId],
                    reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId])

    def atoms =
      Prop.eval[(String, Iterator[Text.AnyOptional])](t => text.anyTextI(t._2).rename(t._1))
        .forallF[List].contramap[P](_.allRichText) rename "Atoms"

    def constituents = (
                   reqs.all.contramap[P](_.reqs)
      ∧        reqCodes.all.contramap[P](_.reqCodes)
      ∧    implications.all.contramap[P](_.implications)
      ∧ deletionReasons.all.contramap[P](_.deletionReasons)
    ) rename "constituents"

    def liveReqCodeRequiresLiveTarget =
      Prop.whitelist[Project]("Live ReqCode requires Live Target")(
        p => p.reqs.reqs.valuesIterator.filter(_.live(p.config.customReqTypes) :: Live).map(_.id).toSet,
        _.reqCodes.activeReqCodesByReqId.keySet)

    def validRefs = {
      type TR = (P, Refs)
      import Atom._

      def mkRefs(p: Project): Refs = Refs(
        p.config.fields.customFields.keySet,
        p.reqs.reqs.valuesIterator.map(_.id).toSet,
        p.reqCodes.idSet,
        p.config.reqTypes.map(_.reqTypeId)(collection.breakOut),
        p.config.tags.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => Traversable[A]) =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validFieldIds   = whitelist(_._2.fieldIds) _
      def validReqIds     = whitelist(_._2.reqIds) _
      def validReqCodeIds = whitelist(_._2.reqCodeIds) _
      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _
      def validIssueTypes = whitelist(_._1.config.customIssueTypes.keySet) _

      ( validReqTypeIds("Pubid keys",                 _.reqs.pubids.value.m.keys)
      ∧ validReqIds    ("ReqCode ReqIds (active)",    _.reqCodes.activeReqCodesByReqId.keys)
      ∧ validReqIds    ("ReqCode ReqIds (inactive)",  _.reqCodes.inactiveIdsByReqId.keys)
      ∧ validFieldIds  ("ReqData.text TextField ids", _.reqText.keys)
      ∧ validReqIds    ("ReqData.text.*.reqIds",      _.reqText.vstreamf(_.keys.toStream))
      ∧ validReqIds    ("ReqData.config.tags keys",   _.reqTags.keys)
      ∧ validTagIds    ("ReqData.config.tags values", _.reqTags.allValues)
      ∧ validReqIds    ("ReqData.implications",       _.implications.members)
      ∧ validReqIds    ("Atoms: ReqRefs",             _.atomScan.reqRefs)
      ∧ validReqCodeIds("Atoms: CodeRefs",            _.atomScan.codeRefs)
      ∧ validTagIds    ("Atoms: TagRefs",             _.atomScan.tagRefs.all.all)
      ∧ validIssueTypes("Atoms: Issues",              _.atomScan.issues.all.all.map(_.typ))
      ∧ validReqIds    ("DeletionReason reqIds",      _.deletionReasons.reqApplication.keys)
      ).rename("Cross-constituent refs").contramap[P](_ mapStrengthR mkRefs)
    }

    val validateIdCeiling: Prop[Project] = {
      val idCeilingIndices = 0 until IdCeilings.zero.productArity
      @inline def lookup(ic: IdCeilings, i: Int): Int =
        ic.productElement(i).asInstanceOf[Int]
      Prop.atom("IdCeiling", p => {
        val actual = p.idCeilings
        val ref = IdCeilings.calculate(p)
        // The actual high-water mark must be at least as high as current levels
        if (idCeilingIndices.exists(i => lookup(actual, i) < lookup(ref, i)))
          Some(s"Invalid ID ceiling(s).\nHave: $actual\nCalc: $ref")
        else
          None
      })
    }

    val allExcludingConfig: Prop[Project] = "Project" rename_: (
      constituents ∧ atoms ∧ liveReqCodeRequiresLiveTarget ∧ validRefs ∧ validateIdCeiling)

    val allIncludingConfig: Prop[Project] =
      allExcludingConfig ∧ projectConfig.all.contramap(_.config)
  }
}
