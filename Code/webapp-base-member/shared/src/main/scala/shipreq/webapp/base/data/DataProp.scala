package shipreq.webapp.base.data

import japgolly.microlibs.recursion._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.prop._
import scala.collection.{IterableOnce, mutable}
import scala.reflect.ClassTag
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import shipreq.base.util._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.text.{Atom, Text}

object DataProp {
  import DataImplicits._
  import MTrie.Ops
  import ScalaExt._
  import ScalazExtra._
  import TaggedTypes.TaggedInt

  implicit def autoLiftL(e: Eval) = e.liftL

  def id[T <: TaggedInt] =
    Prop.test[T]("id > 0", _.value > 0)

  @nowarn("cat=unused")
  def dataId[O, D, Id <: TaggedInt](o: O)(implicit O: ObjDataId[O, D, Id]) =
    id[Id].contramap[D](O.id)

  /**
   * WARNING: Ignores negative numbers.
   * WARNING: Slow with large number values.
   */
  def uniqueNonNegInts[C[x] <: Iterable[x], A](name: => String, f: A => Int): Prop[C[A]] =
    Prop.atom[C[A]]("Unique " + name, as => {
      val log = mutable.BitSet.empty
      if (as.forall(a => {
        val i = f(a)
        (i < 0) || log.add(i)
      }))
        None
      else {
        val is = as.iterator.map(f).toList
        val dups = is.diff(is.distinct).sorted
        Some(dups.mkString("Dups detected: [", ",", "]"))
      }
    })

  /**
   * WARNING: Ignores negative numbers.
   * WARNING: Slow with large number values.
   */
  def uniqueNonNegIntsT[C[x] <: Iterable[x], A <: TaggedInt](name: => String): Prop[C[A]] =
    uniqueNonNegInts[C, A](name, _.value)

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
    type T = ReqTypes

    def uniqueMnemonics =
      Prop.distinctI("mnemonic", (_: T).custom.valuesIterator.flatMap(_.allMnemonics).map(_.value))

    def uniqueNames =
      Prop.distinctI("name", (_: T).custom.valuesIterator.map(_.name))

    def each =
      customReqType.all.forall[T, Iterator](_.custom.valuesIterator)

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

    def fields =
      (uniqueNames).contramap[FieldSet](_.fields)

    def orderNoDups =
      Prop.distinct("order", (_: FieldSet).order)

    def orderCustomFieldsIso =
      Prop.equal[FieldSet]("order.customFields = fieldSet.customFields")(
        _.customFields.keySet,
        _.order.foldLeft(Set.empty[CustomFieldId])((q, id) => id match {
          case i: CustomFieldId => q + i
          case _: StaticField    => q
        }))

    def orderHasAllMandatoryStaticFields =
      Prop.allPresent[FieldSet]("order ⊇ mandatory static")(_ => StaticField.mandatory.whole, _.order)

    def filteredFields[T](f: PartialFunction[CustomField, T]): FieldSet => Iterator[T] = {
      val ff = f.lift
      _.customFields.valuesIterator.map(ff).filterDefined
    }

    def tagFieldsUnique =
      Prop.distinctI("Tag field", filteredFields { case t: CustomField.Tag => t.tagId })

    def implicationFieldsUnique =
      Prop.distinctI("Implication field", filteredFields { case t: CustomField.Implication => t.reqTypeId })

    def noDuplicateTagFieldReqTypeResolutions =
      Prop.distinctI("TagField/ReqTypeResolution", filteredFields { case t: CustomField.Tag => t.fieldReqTypeRules.resolutionIterator() })

    def fieldSet = "FieldSet" rename_: (
      ids ∧ fields ∧
      orderNoDups ∧ orderCustomFieldsIso ∧ orderHasAllMandatoryStaticFields ∧
      tagFieldsUnique ∧ implicationFieldsUnique ∧ noDuplicateTagFieldReqTypeResolutions
    )

    val all =
      fieldSet rename "Fields"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tags {
    type T = TagTree

    def ids =
      id[TagId].forall((_: T).keysIterator)

    def uniqueNames =
      Prop.distinctI("name", (_: T).valuesIterator.map(_.tag.name))

    def uniqueSiblings =
      uniqueNonNegIntsT[Vector, TagId]("siblings").forall((_: T).valuesIterator.map(_.children))

    def noCycles =
      Tag.CycleDetectors.tagTree.noCycleProp("structure")

    def noDeadLinks =
      Prop.whitelist[T]("ids refer to available tags")(_.keySet, _.valuesIterator.flatMap(_.children))

    def tagTree =
      (ids ∧ uniqueNames ∧ uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TagTree"

    val all =
      tagTree rename "Tags"

    def treeStructure =
      (uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TreeStructure"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqs {
    type T = Requirements

    def ids =
      id[ReqId].forall((_: T).idIterator())

    def idsUnique =
      uniqueNonNegIntsT[List, ReqId]("req IDs").contramap[T](_.idIterator().toList)

    def reqPubidsInRegister =
      Prop.forall((_: T).reqIterator())(t =>
        Prop.equal[Req]("Req's pubid refers to itself in the Pubid register")(
          _.id.some,
          r => t.pubids(r.pubid)))

    def pubidsResolveToReqs =
      Prop.whitelist[T]("Pubid register")(_.idIterator().toSet, _.pubids.value.valueIterator)

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
          case (rt: UseCase.type   , reqIds) => test(rt, reqIds)
        }.find(_.isDefined).flatten
      ).contramap[T](_.pubids)
    }

    val all = "Requirements" rename_: (
      ids ∧ idsUnique ∧
      useCases.all.contramap[T](_.useCases).rename("Use Cases") ∧
      reqPubidsInRegister ∧ pubidsResolveToReqs ∧ pubidReqTypeAssociations)
  }

  object useCases {
    type T = UseCases

    def stepIds = {
      val valid  = id[UseCaseStepId].forallF[Set]
      val unique = uniqueNonNegIntsT[Set, UseCaseStepId]("step ids")
      (valid ∧ unique).contramap[T](_.stepIndex.keySet) rename "step ids"
    }

    def stepTrees = {
      import StaticField.{NormalAltStepTree => N, ExceptionStepTree => E}

      def rootStep =
        Prop.test[UseCase]("Root step", _.rootStep.liveExplicitly is Live)

      def eachTree(f: StaticField.UseCaseStepTree) =
        VectorTree.maxDimsProp(
          maxLengthInclusive = WebappConfig.useCaseStepsMaxLength,
          maxDepthInclusive  = f.maxDepth)

      val treesInUseCase: Prop[UseCase] =
        eachTree(N).contramap[UseCase](_.stepsNA.tree).rename(N.name) ∧
        eachTree(E).contramap[UseCase](_.stepsE .tree).rename(E.name) ∧
        rootStep

      treesInUseCase.forall((_: T).imap.valuesIterator) rename "UC trees"
    }

    def stepIndex: Prop[T] =
      Prop.atom("Step index", ucs => {
        var count = 0
        var errors: Set[UseCaseStepId] = UnivEq.emptySet
        val useCaseStepTrees = StaticField.useCaseStepTrees.whole
        for {
          uc <- ucs.imap.valuesIterator
          id = uc.id
          f  <- useCaseStepTrees
          s  <- f.useCaseStepTree.get(uc).valueIterator
        } {
          count += 1
          ucs.stepIndex.get(s.id) match {
            case Some(p) =>
              if (p.useCaseId !=*id || p.field !=* f)
                errors += s.id
            case None =>
              errors += s.id
          }
        }

        if (errors.nonEmpty)
          Some("Incorrect indexes for steps: " + errors.iterator.map(_.value).toList.sorted.mkString(","))
        else if (count != ucs.stepIndex.size)
          Some(s"Expected $count steps in index, found ${ucs.stepIndex.size}.")
        else
          None
      })

    val all = stepIds ∧ stepIndex ∧ stepTrees
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
      id[ReqCodeId].forall((_: T).idSeq)

    def uniqueIds =
      uniqueNonNegIntsT[ArraySeq, ReqCodeId]("IDs").contramap[T](_.idSeq)

    val all =
      (branchesMustBranch ∧ allData ∧ uniqueIds ∧ idFormat) rename "ReqCodes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object implications {
    @inline def all = Implications.Graph.acyclicPropBi
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
          val errors = dr.reqApplication.kvIterator
            .filterDefined_2
            .filterNot(x => testIdFn(x._2))
            .map(x => s"${x._1} → ${x._2}")
            .toVector
          Some(s"Found ${errors.size} invalid DeletionReasonIds: ${errors mkString ", "}")
        } else if (unreferenced.nonEmpty)
          Some(s"There exist deletion reasons without deletion targets: $unreferenced")
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

    val monospace =
      (nonEmpty & ~matches(".*[\r\n`].*")).contramap[PlainTextMarkup#Monospace](_.value)

    val tex =
      (nonEmpty ∧ trimmed).contramap[PlainTextMarkup#TeX](_.value)

    def nonEmptyText: Prop[Text.AnyNonEmpty] = {
      val litval: AnyAtom => String =
        { case a: Literal#Literal => a.value; case _ => "" }
      val head = "Doesn't start with whitespace" rename_: ~(isBlankLine | startsWith(" ").contramap(litval))
      val last = "Doesn't end with whitespace"   rename_: ~(isBlankLine | endsWith(" ").contramap(litval))
      head.contramap[Text.AnyNonEmpty](_.head) ∧ last.contramap[Text.AnyNonEmpty](_.last)
    }.rename("NonEmptyText")

    private val nop = Eval.pass()

    lazy val anyAtom: Prop[AnyAtom] = Prop.eval[AnyAtom] {
      case a: Literal         # Literal        => literal(a)
      case _: NewLine         # BlankLine      => nop
      case a: PlainTextMarkup # WebAddress     => webAddress(a)
      case a: PlainTextMarkup # EmailAddress   => emailAddress(a)
      case a: PlainTextMarkup # Monospace      => monospace(a)
      case a: PlainTextMarkup # TeX            => tex(a)
      case a: PlainTextMarkup # Bold           => anyText(a.inner.whole)
      case a: PlainTextMarkup # Italic         => anyText(a.inner.whole)
      case a: PlainTextMarkup # Underline      => anyText(a.inner.whole)
      case a: PlainTextMarkup # Strikethrough  => anyText(a.inner.whole)
      case a: ListMarkup      # OrderedList    => anyTextV(a.items.whole)
      case a: ListMarkup      # UnorderedList  => anyTextV(a.items.whole)
      case _: ContentRef      # ReqRef         => nop
      case _: ContentRef      # CodeRef        => nop
      case _: ContentRef      # UseCaseStepRef => nop
      case a: Issue           # Issue          => anyText(a.desc)
      case _: TagRef          # TagRef         => nop
      case _: CodeBlock       # CodeBlock      => nop
      case a: Headings        # Heading1       => anyText(a.title.whole)
      case a: Headings        # Heading2       => anyText(a.title.whole)
      case a: Headings        # Heading3       => anyText(a.title.whole)
      case a: Headings        # Heading4       => anyText(a.title.whole)
      case a: Headings        # Heading5       => anyText(a.title.whole)
      case a: Headings        # Heading6       => anyText(a.title.whole)
    } rename "AnyAtom"

    lazy val anyText: Prop[Text.AnyOptional] =
      anyAtom.forallF[ArraySeq] ∧ nonEmptyText.forallF[Option].contramap(NonEmptyArraySeq.option)

    lazy val anyTextV: Prop[ArraySeq[Text.AnyOptional]] = anyText.forallF

    val anyTextI: Prop[Iterator[Text.AnyOptional]] = anyText.forallF
  }

  // -------------------------------------------------------------------------------------------------------------------
  object savedViews {
    import shipreq.webapp.base.data.savedview._

    private def single(allViews: SavedViews.NonEmpty): Prop[SavedView] = {

      val viewId: Prop[SavedView] =
        id[SavedView.Id].contramap(_.id)

      val name: Prop[SavedView] =
        Prop.equal("name")(s => \/-(s.name), s =>
          SavedView.Name.validator(SavedView.Name.State(Some(s.id), allViews))
            .unnamed apply s.name.value)

      val visibleColumnsUnique: Prop[SavedView] =
        Prop.distinct("Column", _.view.columns.whole)

      val sortByVisibleColumns: Prop[SavedView] =
        Prop.whitelist[SavedView]("column")(_.view.columns.whole.toSet, _.view.order.all.iterator.map(_.column))
          .rename("All sort columns are visible")

      val sortColumnsUnique: Prop[SavedView] =
        Prop.distinctI("Sort Column", _.view.order.all.iterator.map(_.column))

      (viewId & name & visibleColumnsUnique & sortByVisibleColumns & sortColumnsUnique)
        .rename("SavedView")
    }

    private def nonEmpty: Prop[SavedViews.NonEmpty] = {
      val noDup: Prop[SavedViews.NonEmpty] =
        Prop.blacklist("non-default")(_.nonDefault.keySet, _.default.id :: Nil)

      val all: Prop[SavedViews.NonEmpty] =
        Prop.forall((_: SavedViews.NonEmpty).iterator)(single)

      noDup ∧ all
    }

    val optional: Prop[SavedViews.Optional] =
      nonEmpty.forallF[Option].rename("Saved ReqTable Views")
  }

  // ===================================================================================================================
  object projectConfig {
    type P = ProjectConfig

    case class Refs(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId])

    def constituents = (
        customIssueTypes.all.contramap[P](_.customIssueTypes)
      ∧   customReqTypes.all.contramap[P](_.reqTypes)
      ∧           fields.all.contramap[P](_.fields)
      ∧             tags.all.contramap[P](_.tags.tree)
    ) rename "constituents"

    def uniqueHashRefKeys =
      Prop.distinctI[P, String]("HashRefKey", p => (
          p.customIssueTypes.valuesIterator.map(_.key) ++
          p.tags.tree.valuesIterator.map(_.tag.keyO).filterDefined
        ).map(_.value.toLowerCase))

    def validRefs = {
      type TR = (P, Refs)

      def mkRefs(p: ProjectConfig): Refs = Refs(
        p.reqTypes.all.iterator.map(_.reqTypeId).toSet,
        p.tags.tree.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => IterableOnce[A]) =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _

      ( validReqTypeIds("Field.fieldReqTypeRules.reqTypes",
         _.fields.customFields.valuesIterator.flatMap(_.fieldReqTypeRules.perReqType.keys))

      ∧ validTagIds("Field.fieldReqTypeRules.defaults",
        p => fields.filteredFields({ case t: CustomField.Tag => t.fieldReqTypeRules})(p.fields)
          .flatMap(_.resolutionIterator().collect { case FieldReqTypeRules.Resolution.DefaultTo(id) => id }))

      ∧ validTagIds("CustomField.Tag.tagIds",
        p => fields.filteredFields({ case t: CustomField.Tag => t.tagId})(p.fields))

      ∧ validReqTypeIds("CustomField.Implication.reqTypeIds",
        p => fields.filteredFields({ case t: CustomField.Implication => t.reqTypeId})(p.fields))

      ∧ validReqTypeIds("ApplicableTag.applicableReqTypes",
        _.tags.applicableTagIterator().flatMap(_.applicableReqTypes.reqTypes))

      ).rename("Cross-constituent refs").contramap[P](_ mapStrengthR mkRefs)
    }

    val all: Prop[ProjectConfig] = "ProjectConfig" rename_: (
      constituents ∧ uniqueHashRefKeys ∧ validRefs)
  }

  // ===================================================================================================================
  object project {
    type P = Project

    final case class Refs(fieldIds      : Set[CustomFieldId],
                          issueIds      : Set[CustomIssueTypeId],
                          reqIds        : Set[ReqId],
                          reqCodeIds    : Set[ReqCodeId],
                          useCaseStepIds: Set[UseCaseStepId],
                          reqTypeIds    : Set[ReqTypeId],
                          tagIds        : Set[TagId]) {
      def addCustomFieldId    (id : CustomFieldId    ): Refs = copy(fieldIds       = fieldIds       + id)
      def addCustomIssueTypeId(id : CustomIssueTypeId): Refs = copy(issueIds       = issueIds       + id)
      def addReqId            (id : ReqId            ): Refs = copy(reqIds         = reqIds         + id)
      def addReqCodeId        (id : ReqCodeId        ): Refs = copy(reqCodeIds     = reqCodeIds     + id)
      def addUseCaseStepId    (id : UseCaseStepId    ): Refs = copy(useCaseStepIds = useCaseStepIds + id)
      def addReqTypeId        (id : ReqTypeId        ): Refs = copy(reqTypeIds     = reqTypeIds     + id)
      def addTagId            (id : TagId            ): Refs = copy(tagIds         = tagIds         + id)
      def ++(o: Refs): Refs =
        Refs(
          fieldIds       = fieldIds       ++ o.fieldIds      ,
          issueIds       = issueIds       ++ o.issueIds      ,
          reqIds         = reqIds         ++ o.reqIds        ,
          reqCodeIds     = reqCodeIds     ++ o.reqCodeIds    ,
          useCaseStepIds = useCaseStepIds ++ o.useCaseStepIds,
          reqTypeIds     = reqTypeIds     ++ o.reqTypeIds    ,
          tagIds         = tagIds         ++ o.tagIds        )
    }

    object Refs {
      val empty: Refs =
        Refs(UnivEq.emptySet, UnivEq.emptySet, UnivEq.emptySet, UnivEq.emptySet, UnivEq.emptySet, UnivEq.emptySet, UnivEq.emptySet)

      import shipreq.webapp.base.data.savedview._

      def savedViewFilters(svs: SavedViews.Optional): Refs =
        svs.fold(empty)(savedViewFiltersNE)

      def savedViewFiltersNE(svs: SavedViews.NonEmpty): Refs =
        svs.iterator.foldLeft(empty)(_ ++ savedViewFilter(_))

      def savedViewFilter(sv: SavedView): Refs =
        sv.view.filter.fold(empty)(Recursion.cata(validFilter)(_))

      private def validFilterReqSetRefs(reqs: Filter.Valid.ReqSet): Refs =
        Refs.empty.copy(reqTypeIds =
          reqs.iterator.map {
            case IntensionalReqSet.WholeType(r)     => r
            case IntensionalReqSet.SomeOfType(r, _) => r
          }.toSet)

      import FilterAst.{ImpCriteria, FieldCriteria}

      private val validFilter: FAlgebra[Filter.ValidF, Refs] = {
        case FilterAst.Reqs          (reqs)                     => validFilterReqSetRefs(reqs)
        case FilterAst.ImpliesAnyOf  (ImpCriteria.Reqs(reqs))   => validFilterReqSetRefs(reqs)
        case FilterAst.ImpliedByAnyOf(ImpCriteria.Reqs(reqs))   => validFilterReqSetRefs(reqs)
        case FilterAst.ImpliesAnyOf  (ImpCriteria.Query(refs))  => refs
        case FilterAst.ImpliedByAnyOf(ImpCriteria.Query(refs))  => refs
        case FilterAst.ReqType       (rt)                       => Refs.empty addReqTypeId rt
        case FilterAst.HashRef       (-\/(issue))               => Refs.empty addCustomIssueTypeId issue
        case FilterAst.HashRef       (\/-(tag))                 => Refs.empty addTagId tag
        case FilterAst.AllOf         (fs)                       => fs.reduce(_ ++ _)
        case FilterAst.AnyOf         (f, fs)                    => f ++ fs.reduce(_ ++ _)
        case FilterAst.Not           (f)                        => f
        case _: FilterAst.Text
           | _: FilterAst.Regex
           | _: FilterAst.HasIssue[Filter.Valid.IssueCat]
           | _: FilterAst.Presence[Filter.Valid.Attr] => Refs.empty

        case FilterAst.FieldProp(field, criteria) =>
          var refs: Refs =
            criteria match {
              case FieldCriteria.Query(r)         => r
              case FieldCriteria.Attr(_)
                 | FieldCriteria.ReqTypePosSet(_) => Refs.empty
            }
          field match {
            case \/-(f: CustomFieldId) => refs = refs.addCustomFieldId(f)
            case _                     => ()
          }
          refs
      }

      val reqtableColumnField: savedview.Column => List[CustomFieldId] = {
        case x: savedview.Column.CustomField => x.id :: Nil
        case savedview.Column.AllTags
           | savedview.Column.OtherTags
           | _: savedview.Column.BuiltIn     => Nil
      }
    }

    def atoms: Prop[P] =
      Prop.eval[(String, Iterator[Text.AnyOptional])](t => text.anyTextI(t._2).rename(t._1))
        .forallF[List].contramap[P](_.content.allRichText) rename "Atoms"

    def constituents = (
                   reqs.all.contramap[P](_.content.reqs)
      ∧        reqCodes.all.contramap[P](_.content.reqCodes)
      ∧    implications.all.contramap[P](_.content.implications.graph)
      ∧ deletionReasons.all.contramap[P](_.content.deletionReasons)
      ∧ savedViews.optional.contramap[P](_.savedViews)
    ) rename "constituents"

    def liveReqCodeRequiresLiveTarget =
      Prop.whitelist[Project]("Live ReqCode requires Live Target")(
        p => p.content.reqs.reqIterator().filter(_.live(p.config.reqTypes) is Live).map(_.id).toSet,
        _.content.reqCodes.activeReqCodesByReqId.keySet)

    def validRefs = {
      type TR = (P, Refs)

      def mkRefs(p: Project): Refs = Refs(
        p.config.fields.customFields.keySet,
        p.config.customIssueTypes.keySet,
        p.content.reqs.reqIterator().map(_.id).toSet,
        p.content.reqCodes.idSet,
        p.content.reqs.useCases.stepIterator.map(_.id).toSet,
        p.config.reqTypes.all.iterator.map(_.reqTypeId).toSet,
        p.config.tags.tree.keySet)

      def whitelist[A](refs: TR => Set[A])(name: String, test: P => IterableOnce[A]): Prop[TR] =
        // Two steps here results in better failure messages
        Prop.whitelist[(P, Set[A])](name + " resolve")(_._2, _._1 |> test)
          .contramap[TR](t => t put2 refs(t))

      def validFieldIds   = whitelist(_._2.fieldIds) _
    //def validIssueIds   = whitelist(_._2.issueIds) _
      def validReqIds     = whitelist(_._2.reqIds) _
      def validUCStepIds  = whitelist(_._2.useCaseStepIds) _
      def validReqCodeIds = whitelist(_._2.reqCodeIds) _
      def validReqTypeIds = whitelist(_._2.reqTypeIds) _
      def validTagIds     = whitelist(_._2.tagIds) _
      def validIssueTypes = whitelist(_._1.config.customIssueTypes.keySet) _

      def fullRefCmp[A](name: String, refs: P => Refs): Prop[TR] = {
        type X = (Refs, TR)
        val px = (
          Prop.whitelist[X](name + ": fieldIds resolve")      (_._2._2.fieldIds      , _._1.fieldIds)       &
          Prop.whitelist[X](name + ": issueIds resolve")      (_._2._2.issueIds      , _._1.issueIds)       &
          Prop.whitelist[X](name + ": reqIds resolve")        (_._2._2.reqIds        , _._1.reqIds)         &
          Prop.whitelist[X](name + ": reqCodeIds resolve")    (_._2._2.reqCodeIds    , _._1.reqCodeIds)     &
          Prop.whitelist[X](name + ": useCaseStepIds resolve")(_._2._2.useCaseStepIds, _._1.useCaseStepIds) &
          Prop.whitelist[X](name + ": reqTypeIds resolve")    (_._2._2.reqTypeIds    , _._1.reqTypeIds)     &
          Prop.whitelist[X](name + ": tagIds resolve")        (_._2._2.tagIds        , _._1.tagIds)         )
        px.contramap[TR](tr => (refs(tr._1) , tr))
      }

      ( validReqTypeIds("Pubid keys",                       _.content.reqs.pubids.value.m.keys)
      ∧ validReqIds    ("ReqCode ReqIds (active)",          _.content.reqCodes.activeReqCodesByReqId.keys)
      ∧ validReqIds    ("ReqCode ReqIds (inactive)",        _.content.reqCodes.inactiveIdsByReqId.keys)
      ∧ validFieldIds  ("ReqData.text TextField ids",       _.content.reqText.data.keys)
      ∧ validReqIds    ("ReqData.text.*.reqIds",            _.content.reqText.data.valuesIterator.flatMap(_.keysIterator))
      ∧ validReqIds    ("ReqData.config.tags keys",         _.content.reqTags.keys)
      ∧ validTagIds    ("ReqData.config.tags values",       _.content.reqTags.valueIterator)
      ∧ validReqIds    ("ReqData.implications",             _.content.implications.members)
      ∧ validReqIds    ("Atoms: ReqRefs",                   _.atomScan.reqRefs)
      ∧ validReqCodeIds("Atoms: CodeRefs",                  _.content.codeRefs)
      ∧ validUCStepIds ("Atoms: UseCaseStepRefs",           _.content.useCaseStepRefs)
      ∧ validTagIds    ("Atoms: TagRefs",                   _.atomScan.tagRefs.all.all.iterator.map(_.value)) // TODO check .loc
      ∧ validIssueTypes("Atoms: Issues in reqs",            _.atomScan.issuesInReqs.all.all.map(_.value.typ))
      ∧ validIssueTypes("Atoms: Issues in RCGs",            _.atomScan.issuesInRcgs.all.all.map(_.typ))
      ∧ validReqIds    ("DeletionReason reqIds",            _.content.deletionReasons.reqApplication.keys)
      ∧ validUCStepIds ("UseCase step flow",                p => Digraph.memberIterator(p.content.reqs.useCases.stepFlow.forwards))
      ∧ fullRefCmp     ("SavedView filters",                p => Refs.savedViewFilters(p.savedViews))
      ∧ validFieldIds  ("SavedViews: Columns",              _.savedViewIterator.flatMap(_.view.columns.whole).flatMap(Refs.reqtableColumnField))
      ∧ validFieldIds  ("SavedViews: Sort Columns",         _.savedViewIterator.flatMap(_.view.order.all.whole).map(_.column).flatMap(Refs.reqtableColumnField))

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
