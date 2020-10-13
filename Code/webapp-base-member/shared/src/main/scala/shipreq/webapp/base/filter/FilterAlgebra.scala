package shipreq.webapp.base.filter

import japgolly.microlibs.recursion._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.ConciseIntSetFormat
import java.util.regex.Pattern
import scalaz.syntax.traverse1._
import scalaz.{Functor, Traverse}
import shipreq.base.util.{Applicable, ErrorMsg, OptionalBoolFn, TransitiveClosure}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.derivation.DataLogic.IssueLookup
import shipreq.webapp.base.data.derivation.VirtualProjectTags
import shipreq.webapp.base.data.{FilterDead, On}
import shipreq.webapp.base.issue.Issues
import shipreq.webapp.base.text.{Atom, Grammar, PlainText, TextSearch}

/** Algebras:
  *
  * {{{
  *   unparse        : FAlgebra [             PotentialF,   AtomOrComposite[String]]
  *   validate       : FAlgebraM[String \/ *, PotentialF,   Valid                  ]
  *   unvalidate     : FAlgebra [             ValidF,       Potential              ]
  *   makeExtensional: FAlgebra [             ValidF,       Extensional            ]
  *   compile        : FAlgebra [             ExtensionalF, CompiledFilter         ]
  *   remove         : FAlgebra [             ValidF,       Boolean \/ Valid       ]
  * }}}
  */
object FilterAlgebra {
  import Filter._
  import FilterAst._
  import FilterAst.FieldCriteria

  val isFieldNameUnquotedChar: Char => Boolean = {
    case ':' | '=' | '"' => false
    case c               => !c.isWhitespace
  }

  def quoteFieldName(name: String): String =
    if (name.forall(isFieldNameUnquotedChar))
      name
    else
      "\"" + name + "\""

  val unparse: FAlgebra[PotentialF, AtomOrComposite[String]] = {
    import shipreq.base.util.SafeStringOps._
    import AtomOrComposite.string._
    implicit def autoAtom(s: String) = atom(s)

    def fmtReqs(r: Potential.ReqSet, sep: Char): String =
      r.reduceMapLeft1(fmtReqsSpec)(_ ~ sep ~ _)

    def fmtReqsSpec(r: Potential.ReqSubset): String =
      r match {
        case IntensionalReqSet.WholeType (mn)     => mn.value
        case IntensionalReqSet.SomeOfType(mn, ns) => fmtSomeReqs(mn, ns)
      }

    def fmtSomeReqs(m: data.ReqType.Mnemonic, ns: NonEmptySet[Int]): String = {
      val n =
        if (ns.tail.isEmpty)
          ns.head.toString
        else
          '{' ~ ConciseIntSetFormat(ns.whole) ~ '}'
      m.value ~ '-' ~ n
    }

    val subquery: AtomOrComposite[String] => String = {
      case AtomOrComposite.Atom(a)            => "(" ~ a ~ ")"
      case c@ AtomOrComposite.Composite(_, _) => c.atom
    }

    val fieldCriteria: Potential.FieldCriteriaF[AtomOrComposite[String]] => String = {
      case FieldCriteria.Attr(a)          => a
      case FieldCriteria.ReqTypePosSet(s) => ConciseIntSetFormat(s.whole.map(_.value))
      case FieldCriteria.Query(q)         => subquery(q)
    }

    val impCriteria: Potential.ImpCriteriaF[AtomOrComposite[String]] => String = {
      case ImpCriteria.Reqs(r)  => fmtReqs(r, ',')
      case ImpCriteria.Query(q) => subquery(q)
    }

    @inline def fmtScoped(main: Boolean, scope: Potential.Scope, clause: AtomOrComposite[String]): AtomOrComposite[String] = {
      val prefix =
        if(main) Scope.main else ""

      val scopeText =
        scope.iterator.map {
          case Scope.Derivation(None) =>
            Scope.Derivation.keyword

          case Scope.Derivation(Some(f)) =>
            Scope.Derivation.keyword +
              Scope.Derivation.fieldPrefix +
              Scope.Derivation.quoteIfNecessary(f) +
              Scope.Derivation.fieldSuffix

        }.mkString(prefix, Scope.separator, Scope.suffix)

      val clauseText =
        clause match {
          case AtomOrComposite.Atom(t)         => t
          case AtomOrComposite.Composite(t, _) => t
        }

      s"$scopeText($clauseText)"
    }

    {
      case Text          (text, None)        => text
      case Text          (text, Some(qChar)) => qChar ~ text ~ qChar
      case Regex         (text)              => '/' ~ text.replace("/", "\\/") ~ '/'
      case ReqType       (value)             => value.value
      case HashRef       (text)              => Grammar.hashRefKey.prefix ~ text.value
      case FieldProp     (field, attr)       => "field:" ~ quoteFieldName(field) ~ "=" ~ fieldCriteria(attr)
      case HasIssue      (on, criteria)      => "has:issue:" ~ (if (on is On) "" else "-") ~ criteria.mkString(",")
      case ImpliesAnyOf  (criteria)          => "implies:" ~ impCriteria(criteria)
      case ImpliedByAnyOf(criteria)          => "impliedBy:" ~ impCriteria(criteria)
      case Reqs          (reqs)              => fmtReqs(reqs, ' ')
      case Presence      (attr)              => "has:" ~ attr
      case Scoped        (main, s, c)        => fmtScoped(main, s, c)
      case Not           (clause)            => '-' ~ clause.atom
      case AllOf         (clauses)           => composite("(", clauses.iterator.map(_.atom).mkString(" "),  ")")
      case AnyOf         (c, cs)             => composite("(", (c +: cs).iterator.map(_.atom).mkString(" | "),  ")")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def validate(cfg: data.ProjectConfig): FAlgebraM[ErrorMsg \/ *, PotentialF, Valid] = {
    type R = ErrorMsg \/ Valid

    @inline def fail(err: String) = -\/(ErrorMsg(err))

    def byAttr(attrStr: String, f: Attr => ValidF[Nothing]): R =
      Attr(attrStr) match {
        case Some(a) => \/-(Valid(f(a)))
        case None    => fail(s"Unknown attribute: '$attrStr'. Known: ${Attr.availableText}.") // English
      }

    def lookupFieldAttr(attrStr: String)(f: FieldAttr => R): R =
      FieldAttr(attrStr) match {
        case Some(a) => f(a)
        case None    => fail(s"Unknown attribute: '$attrStr'. Known: ${FieldAttr.availableText}.") // English
      }

    def byHashTag(k: data.HashRefKey): R =
      cfg.hashRefLookup(k.value) match {
        case Some(-\/(t)) => \/-(Valid(HashRef(\/-(t.id))))
        case Some(\/-(i)) => \/-(Valid(HashRef(-\/(i.id))))
        case None         => fail(s"Unknown tag or issue: '${k.value}'") // English
      }

    val reqTypesByMnemonic = cfg.reqTypes.allByMnemonic

    def lookupReqType(mn: data.ReqType.Mnemonic): ErrorMsg \/ data.ReqType =
      reqTypesByMnemonic.get(mn) match {
        case Some(rt) => \/-(rt)
        case None     => fail(s"Unknown type: '${mn.value}'") // English
      }

    val lookupReqSubset: Potential.ReqSubset => ErrorMsg \/ Valid.ReqSubset =
      Traverse[IntensionalReqSet].traverse(_)(lookupReqType(_).map(_.reqTypeId))

    def byReqSet(reqs: Potential.ReqSet, f: Valid.ReqSet => ValidF[Valid]): R =
      reqs.traverse1(lookupReqSubset).map(nev => Valid(f(nev)))

    def byRegex(regex: String): R =
      try {
        Pattern compile regex
        \/-(Valid(Regex(regex)))
      } catch {
        // PatternSyntaxException not available in Scala.JS
        // case e: PatternSyntaxException => error(e.getDescription)
        case _: Throwable => fail(s"Invalid regex: /$regex/")
      }

    type ParsedField = data.SpecialBuiltInField.FilterOk \/ data.Field

    def parseFieldName(name: String): ErrorMsg \/ ParsedField = {
      val nameLower = name.toLowerCase
      def tryL      = data.SpecialBuiltInField.filterOkByNameLowercase.get(nameLower).map(-\/(_))
      def tryR      = cfg.fieldsByNameLowercaseWithFilterAliases.get(nameLower).map(\/-(_))

      (tryL orElse tryR) match {
        case Some(f) => \/-(f)
        case None    => fail(s"Unknown field: '$name'")
      }
    }

    def byField(fieldName: String, criteria: FieldCriteria[String, Valid]): R = {
      import FieldAttr._
      import data._

      def parseAsAttr(field: ParsedField, attrText: String): R =
        lookupFieldAttr(attrText) { attr =>
          import SpecialBuiltInField._

          def blankOnly(f: Valid.Field, name: String): ErrorMsg \/ Valid =
            attr match {
              case Blank
                 | NotBlank      => \/-(Valid.fieldProp(f, FieldCriteria.Attr(attr)))
              case DefaultInUse  => fail(s"$name doesn't have defaults")
              case NotApplicable => fail(s"$name is always applicable")
            }

          def noDefault(f: Valid.Field, name: String): ErrorMsg \/ Valid =
            attr match {
              case Blank
                 | NotBlank
                 | NotApplicable => \/-(Valid.fieldProp(f, FieldCriteria.Attr(attr)))
              case DefaultInUse  => fail(s"$name doesn't have defaults")
            }

          // Keep FilterEditor pxAutoComplete in sync with below
          (field, attr) match {
            case (\/-(f: CustomField)                       , Blank
                                                            | NotBlank
                                                            | NotApplicable) => \/-(Valid.fieldProp(\/-(f.id), FieldCriteria.Attr(attr)))
            case (\/-(f: CustomField.Tag)                   , DefaultInUse ) => \/-(Valid.fieldProp(\/-(f.id), FieldCriteria.Attr(attr)))
            case (\/-(_: CustomField.Text)                  , DefaultInUse ) => fail("Text fields don't have defaults.")
            case (\/-(_: CustomField.Implication)           , DefaultInUse ) => fail("Implication fields don't have defaults.")
            case (-\/(f: Title.type)                        , _            ) => blankOnly(-\/(f), f.name)
            case (\/-(f: StaticField.OtherTags.type)        , _            ) => blankOnly(\/-(f), f.name)
            case (\/-(f: StaticField.AllTags.type)          , _            ) => blankOnly(\/-(f), f.name)
            case (\/-(f: StaticField.NormalAltStepTree.type), _            ) => noDefault(\/-(f), f.name)
            case (\/-(f: StaticField.ExceptionStepTree.type), _            ) => noDefault(\/-(f), f.name)
            case (\/-(StaticField.ImplicationGraph)         , _)
               | (\/-(StaticField.StepGraph)                , _            ) => fail(s"$fieldName can't be used here.")
          }
        }

      def valuesNotAllowed: R =
        fail(s"You can't specify values for $fieldName.")

      def parseAsPoses(field: ParsedField, s: FieldCriteria.ReqTypePosSet): R =
        field match {
          case \/-(f: CustomField.Implication) => \/-(Valid.fieldProp(\/-(f.id), s))
          case _                               => valuesNotAllowed
        }

      def parseAsQuery(field: ParsedField, q: FieldCriteria.Query[Valid]): R =
        field match {
          case \/-(f: CustomField.Implication) => \/-(Valid.fieldProp(\/-(f.id), q))
          case _                               => valuesNotAllowed
        }

      parseFieldName(fieldName).flatMap { field =>
        criteria match {
          case FieldCriteria.Attr(a)          => parseAsAttr(field, a)
          case q@ FieldCriteria.Query(_)      => parseAsQuery(field, q)
          case s: FieldCriteria.ReqTypePosSet => parseAsPoses(field, s)
        }
      }
    }

    def impCriteria(criteria: Potential.ImpCriteriaF[Valid], f: Valid.ImpCriteria => ValidF[Valid]): R =
      criteria match {
        case ImpCriteria.Reqs(reqs)  => byReqSet(reqs, x => f(ImpCriteria.Reqs(x)))
        case q@ ImpCriteria.Query(_) => \/-(Valid(f(q)))
      }

    def scoped(main: Boolean, scope: Potential.Scope, clause: Valid): R = {
      val scopeResult: ErrorMsg \/ NonEmptyVector[Scope[data.CustomField.Tag.Id]] =
        scope.traverse1 {
          case Scope.Derivation(None) =>
            \/-(Scope.Derivation(None))

          case Scope.Derivation(Some(fieldName)) =>
            parseFieldName(fieldName).flatMap {
              case \/-(f: data.CustomField.Tag) =>
                // Not you'd be tempted to add a check here to confirm that the tag field actually uses derivative tags
                // but if we did that, it would allow valid saved filters to later become invalid and not work.
                \/-(Scope.Derivation(Some(f.id)))

              case _ =>
                fail(s"$fieldName is not a tag field.")
            }
        }

      for {
        s <- scopeResult
      } yield Fix(Scoped(main, s.toNES, clause))
    }

    {
      case HashRef       (key)             => byHashTag(key)
      case ImpliesAnyOf  (criteria)        => impCriteria(criteria, ImpliesAnyOf.apply)
      case ImpliedByAnyOf(criteria)        => impCriteria(criteria, ImpliedByAnyOf.apply)
      case Reqs          (reqs)            => byReqSet(reqs, Reqs.apply)
      case Presence      (attr)            => byAttr(attr, Presence.apply)
      case HasIssue      (on, c)           => c.traverse1(FilterAst.issueCategoryFromStr).bimap(ErrorMsg.apply, Valid.hasIssue(on, _))
      case Regex         (regex)           => byRegex(regex)
      case ReqType       (mn)              => lookupReqType(mn).map(rt => Valid(ReqType(rt.reqTypeId)))
      case FieldProp     (field, criteria) => byField(field, criteria)
      case Scoped        (m, s, c)         => scoped(m, s, c)
      case c: Text                         => \/-(Valid(c))
      case c: Not  [Valid]                 => \/-(Valid(c))
      case c: AllOf[Valid]                 => \/-(Valid(c))
      case c: AnyOf[Valid]                 => \/-(Valid(c))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def unvalidate(cfg: data.ProjectConfig): FAlgebra[ValidF, Potential] = {
    val convReqType: data.ReqTypeId => data.ReqType.Mnemonic =
      cfg.reqTypes.need(_).mnemonic

    def convReqSet(x: Valid.ReqSet): Potential.ReqSet =
      x.map(Functor[IntensionalReqSet].map(_)(convReqType))

    val fieldName: data.FieldId => String = {
      @inline def default = cfg.fieldName

      {
        case id: data.CustomField.Implication.Id =>
          val f     = cfg.fields.custom(id)
          val alias = cfg.reqTypes.need(f.reqTypeId).mnemonic.value
          cfg.fieldsByNameLowercaseWithFilterAliases.get(alias.toLowerCase) match {
            case Some(f2) if f2.fieldId !=* id => default(id)
            case _                             => alias
          }

        case id =>
          default(id)
      }
    }

    val fieldCriteria: FieldCriteria[FieldAttr, Potential] => Potential.FieldCriteria = {
      case FieldCriteria.Attr(a)             => FieldCriteria.Attr(a.name)
      case x@ FieldCriteria.ReqTypePosSet(_) => x
      case x@ FieldCriteria.Query(_)         => x
    }

    def impCriteria(criteria: Valid.ImpCriteriaF[Potential]): Potential.ImpCriteria =
      criteria match {
        case ImpCriteria.Reqs(reqs) => ImpCriteria.Reqs(convReqSet(reqs))
        case x@ImpCriteria.Query(_) => x
      }

    val convertScope: Scope[data.CustomField.Tag.Id] => Scope[String] = {
      case Scope.Derivation(None)    => Scope.Derivation(None)
      case Scope.Derivation(Some(f)) => Scope.Derivation(Some(cfg.fieldName(f)))
    }

    {
      case HashRef       (\/-(id))  => Potential(HashRef       (cfg.tags.needApplicableTag(id).key))
      case HashRef       (-\/(id))  => Potential(HashRef       (cfg.customIssueTypes.need(id).key))
      case Presence      (attr)     => Potential(Presence      (attr.name))
      case ImpliesAnyOf  (criteria) => Potential(ImpliesAnyOf  (impCriteria(criteria)))
      case ImpliedByAnyOf(criteria) => Potential(ImpliedByAnyOf(impCriteria(criteria)))
      case Reqs          (reqs)     => Potential(Reqs          (convReqSet(reqs)))
      case ReqType       (id)       => Potential(ReqType       (convReqType(id)))
      case HasIssue      (on, c)    => Potential(HasIssue      (on, c.map(FilterAst.issueCategoryToStr)))
      case FieldProp     (f, c)     => Potential(FieldProp     (f.fold(_.name, fieldName), fieldCriteria(c)))
      case Scoped        (m, ss, c) => Potential(Scoped        (m, ss.toNEV.map(convertScope), c))
      case c: Regex                 => Potential(c)
      case c: Text                  => Potential(c)
      case c: Not  [Potential]      => Potential(c)
      case c: AllOf[Potential]      => Potential(c)
      case c: AnyOf[Potential]      => Potential(c)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def makeExtensional(p: data.Project): FAlgebra[ValidF, Extensional] = {

    def lookupSomeReqs(reqTypeId: data.ReqTypeId, nums: NonEmptySet[Int]): Set[data.ReqId] = {
      val vec = p.content.reqs.pubids.value(reqTypeId)
      nums.foldLeft[Set[data.ReqId]](UnivEq.emptySet)((ids, num) =>
        if (num > vec.length) ids else ids + vec(num - 1))
    }

    val lookupReqSubset: Valid.ReqSubset => Extensional.ReqSet = {
      case IntensionalReqSet.SomeOfType(reqTypeId, ns) => lookupSomeReqs(reqTypeId, ns)
      case IntensionalReqSet.WholeType(reqTypeId)      => p.content.reqs.pubids.value(reqTypeId).toSet
    }

    def lookupReqSet(ss: NonEmptyVector[Valid.ReqSubset]): Extensional.ReqSet =
      ss.reduceMapLeft1(lookupReqSubset)(_ ++ _)

    def impCriteria(criteria: Valid.ImpCriteriaF[Extensional]): Extensional.ImpCriteria =
      criteria match {
        case ImpCriteria.Reqs(reqs) => ImpCriteria.Reqs(lookupReqSet(reqs))
        case x@ImpCriteria.Query(_) => x
      }

    {
      case ImpliesAnyOf  (criteria)               => Extensional(ImpliesAnyOf  (impCriteria(criteria)))
      case ImpliedByAnyOf(criteria)               => Extensional(ImpliedByAnyOf(impCriteria(criteria)))
      case Reqs          (reqs)                   => Extensional(Reqs          (lookupReqSet(reqs)))
      case c: Regex                               => Extensional(c)
      case c: Text                                => Extensional(c)
      case c@ FieldProp(_, _)                     => Extensional(c)
      case c: HashRef  [Valid.HashTag]            => Extensional(c)
      case c: Presence [Valid.Attr]               => Extensional(c)
      case c: HasIssue [Valid.IssueCat]           => Extensional(c)
      case c: ReqType  [Valid.ReqType]            => Extensional(c)
      case c: Not      [Extensional]              => Extensional(c)
      case c: AllOf    [Extensional]              => Extensional(c)
      case c: AnyOf    [Extensional]              => Extensional(c)
      case c: Scoped   [Valid.Scope, Extensional] => Extensional(c)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
   * @param filterDead Not used to in consideration of reqs, but config.
   *                   i.e. dead reqs will still be returned (and expected to be further filtered out later),
   *                   where as it's needed in consideration of default tags in field rules.
   *
   *                   This is important to remember because this is an [[FAlgebra]], not a holistic filter.
   *                   It operates on AST layers one at a time. Injecting [[FilterDead]] here would corrupt layers
   *                   such that not(x) would become (live & not(live & x)).
   */
  def compile(p          : data.Project,
              filterDead : FilterDead,
              projectText: PlainText.ForProject.NoCtx,
              textSearch : TextSearch,
              issueLookup: IssueLookup,
              tags       : VirtualProjectTags): FAlgebra[ExtensionalF, CompiledFilter] = {
    var cata: Extensional => CompiledFilter = null

    // Possible optimisations:
    // - overlap between has Tag & Presence(AnyTag)
    // - overlap between has Issue & Presence(AnyIssue)
    // - overlap between WholeType & SomeOfType
    // - cycle in ImpliesAnyOf & ImpliedByAnyOf is impossible to satisfy
    // - AnyOf with 2 contradictions = always pass
    // - AllOf with 2 contradictions = always fail
    // - AnyOf stops when match found, AllOf stops when non-match found. DeMorgan to the faster case.
    // - Remove duplicates
    // - Implications in AnyOf can be merged "{implies:MF-1 implies:MF-2}" = "implies:MF-{1,2}"
    // - Squash Not(Not(x)) => x

    import shipreq.webapp.base.data.{ReqType => _, _}
    lazy val issuesBySource = p.issues.bySource

    def ignore: Any => Boolean = null
    def fail: Any => Boolean = _ => false

    def make(req        : Req         => Boolean,
             codeGroup  : CodeGroup   => Boolean,
             manualIssue: ManualIssue => Boolean): CompiledFilter =
      CompiledFilter(
        req         = OptionalBoolFn(Option(req)),
        codeGroup   = OptionalBoolFn(Option(codeGroup)),
        manualIssue = OptionalBoolFn(Option(manualIssue)),
      )

    def reqOnly(f: Req => Boolean): CompiledFilter =
      make(
        req         = f,
        codeGroup   = ignore,
        manualIssue = fail,
      )

    def ucOnly(f: UseCase => Boolean): CompiledFilter =
      reqOnly {
        case _: GenericReq => false
        case uc: UseCase   => f(uc)
      }

    def runSubQuery(subquery: CompiledFilter): Set[ReqId] =
      subquery.req.iterator(p.reqIterator(filterDead)).map(_.id).toSet

    def fieldApplicableReqOnly(fieldId: FieldId)(f: Req => Boolean): CompiledFilter = {
      val applicability = p.config.applicability.byField(fieldId)
      reqOnly(req => applicability(req.reqTypeId).is(Applicable) && f(req))
    }

    def byTag(f: Set[ApplicableTagId] => Boolean): CompiledFilter =
      make(
        req         = r => f(tags(r.id, filterDead).set(TagFieldId.All)),
        codeGroup   = ignore,
        manualIssue = i => f(i.tags),
      )

    def byIssue(f: Issues.ForSource => Boolean): CompiledFilter =
      make(
        req         = r => f(issuesBySource(r.id)),
        codeGroup   = g => f(issuesBySource(g.id)),
        manualIssue = fail,
      )

    def byCustomIssueType(f: Vector[Atom.AnyIssue] => Boolean): CompiledFilter =
      make(
        req         = r => f(issueLookup.forReq(r.id)),
        codeGroup   = g => f(issueLookup.forReqCodeGroup(g.id)),
        manualIssue = fail,
      )

    def byImplication(criteria: Extensional.ImpCriteriaF[CompiledFilter], tc: TransitiveClosure[ReqId]): CompiledFilter = {
      val sources: Set[ReqId] =
        criteria match {
          case ImpCriteria.Reqs(reqs)      => reqs
          case ImpCriteria.Query(subquery) => runSubQuery(subquery)
        }
      val whitelist: Set[ReqId] =
        sources.foldLeft(UnivEq.emptySet[ReqId])(_ ++ tc(_))
      reqOnly(whitelist contains _.id)
    }

    def byText(substr: String): CompiledFilter = {
      val searchFilter = textSearch.ignoreCaseSingleSpaces.searchFilter(substr)
      CompiledFilter(
        req         = searchFilter.req        .contramap(_.id),
        codeGroup   = searchFilter.codeGroup  .contramap(_.id),
        manualIssue = searchFilter.manualIssue.contramap(_.id),
      )
    }

    def byPubidOrText(str: String): CompiledFilter =
      Grammar.pubid.stringPrism.getOption(str).flatMap(_.lookup(p).toOption) match {
        case Some(matchingReq) =>
          val posPrefix = matchingReq.pubid.pos.value.toString
          reqOnly { r =>
            @inline def reqTypeMatch = r.reqTypeId ==* matchingReq.reqTypeId
            @inline def idMatch      = r.id ==* matchingReq.id
            @inline def posMatch     = r.pubid.pos.value.toString.startsWith(posPrefix)
            reqTypeMatch && (idMatch || posMatch)
          }
        case None =>
          byText(str)
      }

    def byRegex(regex: String): CompiledFilter = {
      val pat = Pattern.compile(regex)
      val m: String => Boolean = pat.matcher(_).matches
      make(
        r => {
          def title  = m(projectText reqTitle r)
          def custom = p.config.liveCustomTextFields.exists(f => projectText.customTextFieldOption(f.id)(r) exists m)
          title || custom
        },
        g => m(projectText codeGroupTitle g),
        i => m(projectText.manualIssue(i.text))
      )
    }

    def byReqType(rt: ReqTypeId): CompiledFilter =
      filterDead match {
        case HideDead =>
          reqOnly(_.reqTypeId ==* rt)

        case ShowDead =>
          val exReqs = p.content.reqs.exReqs(rt)
          reqOnly(r => r.reqTypeId ==* rt || exReqs.contains(r.id))
      }

    def byFieldProp(fieldArg: Filter.Valid.Field, criteria: FieldCriteria[FieldAttr, CompiledFilter]): CompiledFilter = {
      import FieldReqTypeRules.Resolution
      import FieldAttr._

      (criteria, fieldArg) match {

        case (FieldCriteria.ReqTypePosSet(posSet), \/-(id: CustomField.Implication.Id)) =>
          val criteria = posSet.whole
          val lookup = p.dataLogic.customFieldImps(filterDead)(id)
          reqOnly(req => lookup.getPubids(req.id).exists(pubid => criteria.contains(pubid.pos)))

        case (FieldCriteria.Query(subquery), \/-(id: CustomField.Implication.Id)) =>
          val criteria = runSubQuery(subquery)
          val lookup = p.dataLogic.customFieldImps(filterDead)(id)
          reqOnly(req => lookup.getReqIds(req.id).exists(criteria.contains))

        case (FieldCriteria.Attr(Blank), \/-(f: CustomField.Text.Id)) =>
          val text = p.content.reqTextFor(f)
          fieldApplicableReqOnly(f)(req => !text.contains(req.id))

        case (FieldCriteria.Attr(Blank), \/-(f: CustomField.Implication.Id)) =>
          val imps = p.dataLogic.customFieldImps(ShowDead)(f)
          fieldApplicableReqOnly(f)(req => imps.getReqIds(req.id).isEmpty)

        case (FieldCriteria.Attr(Blank), \/-(f: CustomField.Tag.Id)) =>
          val scope = p.config.tagFieldDistribution(filterDead).inField(f)
          fieldApplicableReqOnly(f)(req => tags(req.id, filterDead).set(TagFieldId.All).intersect(scope).isEmpty)

        case (FieldCriteria.Attr(Blank), -\/(SpecialBuiltInField.Title)) =>
          make(
            req         = _.title.isEmpty,
            codeGroup   = _.title.isEmpty,
            manualIssue = fail,
          )

        case (FieldCriteria.Attr(Blank), \/-(StaticField.OtherTags)) =>
          val scope = p.config.tagFieldDistribution(filterDead).notUsedInFields
          reqOnly(req => tags(req.id, filterDead).set(TagFieldId.All).intersect(scope).isEmpty)

        case (FieldCriteria.Attr(Blank), \/-(StaticField.AllTags)) =>
          byTag(_.isEmpty)

        case (FieldCriteria.Attr(Blank), \/-(StaticField.NormalAltStepTree)) =>
          ucOnly { uc =>
            filterDead.filterFn.iteratorBy(uc.stepsNA.tree.valueIterator)(_.liveIgnoringUC(uc.stepsNA))
              .drop(1)
              .isEmpty
          }

        case (FieldCriteria.Attr(Blank), \/-(StaticField.ExceptionStepTree)) =>
          ucOnly { uc =>
            filterDead.filterFn.iteratorBy(uc.stepsE.tree.valueIterator)(_.liveIgnoringUC(uc.stepsE))
              .isEmpty
          }

        case (FieldCriteria.Attr(DefaultInUse), \/-(f: CustomField.Tag.Id)) =>
          fieldApplicableReqOnly(f)(req => tags(req.id, filterDead).defaults.contains(f))

        case (FieldCriteria.Attr(NotApplicable), -\/(SpecialBuiltInField.Title)) =>
          reqOnly(fail)

        case (FieldCriteria.Attr(NotApplicable), \/-(id)) =>
          val field = p.config.fields.need(id)
          val na = p.config.reqTypesWithRes(field.fieldReqTypeRules)(Resolution.NotApplicable).map(_.reqTypeId).toSet
          reqOnly(req => na.contains(req.reqTypeId))

        case (FieldCriteria.Attr(NotBlank), field) =>
          import Extensional._
          import FilterAst.FieldCriteria.Attr
          val rewritten: Extensional =
            not(anyOf(
              fieldProp(field, Attr(Blank)),
              fieldProp(field, Attr(NotApplicable))))
          cata(rewritten)

        case (FieldCriteria.ReqTypePosSet(_) | FieldCriteria.Query(_),
                 -\/(_)
               | \/-(_: CustomField.Tag.Id)
               | \/-(_: CustomField.Text.Id)
               | \/-(_: StaticField)
             ) =>
               reqOnly(fail)

        case (FieldCriteria.Attr(Blank),
                 \/-(StaticField.ImplicationGraph)
               | \/-(StaticField.StepGraph)
             ) =>
               reqOnly(fail)

        case (FieldCriteria.Attr(DefaultInUse),
                 -\/(SpecialBuiltInField.Title)
               | \/-(StaticField.AllTags)
               | \/-(StaticField.OtherTags)
               | \/-(StaticField.ImplicationGraph)
               | \/-(StaticField.NormalAltStepTree)
               | \/-(StaticField.ExceptionStepTree)
               | \/-(StaticField.StepGraph)
               | \/-(_: CustomField.Implication.Id)
               | \/-(_: CustomField.Text.Id)
             ) =>
               reqOnly(fail)
      }
    }

    val self: FAlgebra[ExtensionalF, CompiledFilter] = {
      case Text          (t, None)       => byPubidOrText(t)
      case Text          (t, Some(_))    => byText(t)
      case Reqs          (reqs)          => reqOnly(r => reqs.contains(r.id))
      case ImpliesAnyOf  (criteria)      => byImplication(criteria, p.implicationTgtToSrcTC)
      case ImpliedByAnyOf(criteria)      => byImplication(criteria, p.implicationSrcToTgtTC)
      case ReqType       (rt)            => byReqType(rt)
      case Presence      (Attr.AnyIssue) => byIssue(_.issues.nonEmpty)
      case Presence      (Attr.AnyTag)   => byTag(_.nonEmpty)
      case HashRef       (-\/(issue))    => byCustomIssueType(_.exists(_.typ ==* issue))
      case HashRef       (\/-(tag))      => byTag(_ contains tag)
      case Regex         (regex)         => byRegex(regex)
      case FieldProp     (f, a)          => byFieldProp(f, a)
      case AllOf         (fs)            => fs.reduce(_ && _)
      case AnyOf         (f, fs)         => f || fs.reduce(_ || _)
      case Not           (f)             => !f
      case Scoped        (m, _, c)       => if (m) c else CompiledFilter.empty
      case HasIssue      (On, cs)        => byIssue(i => i.issues.nonEmpty && cs.forall(i.categories.contains))

      case HasIssue      (Off, cs)       =>
        val categories = cs.whole.toSet
        byIssue(i => i.issues.nonEmpty && i.categories.exists(!categories.contains(_)))
    }

    import Filter.Implicits.traverseFilterExtensional
    cata = RecursionFn.cata(self)

    self
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Attempts to remove data from a filter.
   *
   * @param fields Fields to remove.
   * @param reqTypes Req types to remove.
   * @return `\/-(f)` = successful filter
   *         `-\/(b)` = with data removed, the result is constantly `b`
   */
  def remove(fields  : Set[data.FieldId  ],
             reqTypes: Set[data.ReqTypeId],
            ): FAlgebra[ValidF, Boolean \/ Valid] = {

    type Result = Boolean \/ Valid

    def constFalse: Valid = {
      val nope = Valid.text("x")
      Valid.allOf(nope, Valid.not(nope))
    }

    def check1(isBad: Boolean, ok: Valid): Result =
      if (isBad) -\/(false) else \/-(ok)

    def fieldCriteria(x: Valid.FieldCriteriaF[Result]): Boolean \/ Valid.FieldCriteria =
      x match {
        case c@ FieldCriteria.Attr         (_) => \/-(c)
        case c@ FieldCriteria.ReqTypePosSet(_) => \/-(c)
        case FieldCriteria.Query           (d) => d.map(FieldCriteria.Query(_))
      }

    def impCriteria(x: Valid.ImpCriteriaF[Result]): Boolean \/ Valid.ImpCriteria =
      x match {
        case r@ ImpCriteria.Reqs(_) => \/-(r)
        case ImpCriteria.Query(d)   => d.map(ImpCriteria.Query(_))
      }

    {
      case a: Text                                       => \/-(Valid(a))
      case a: Regex                                      => \/-(Valid(a))
      case a: Presence      [Valid.Attr]                 => \/-(Valid(a))
      case a: HasIssue      [Valid.IssueCat]             => \/-(Valid(a))
      case a: HashRef       [Valid.HashTag]              => \/-(Valid(a))
      case a: Reqs          [Valid.ReqSet]               => \/-(Valid(a))
      case a: ReqType       [Valid.ReqType]              => check1(reqTypes.contains(a.reqType), Valid(a))
      case a: Not           [Result]                     => a.clause.fold(b => -\/(!b), c => \/-(Valid(Not(c))))
      case a: ImpliesAnyOf  [Valid.ImpCriteriaF, Result] => impCriteria(a.criteria).map(Valid.impliesAnyOf)
      case a: ImpliedByAnyOf[Valid.ImpCriteriaF, Result] => impCriteria(a.criteria).map(Valid.impliedByAnyOf)

      case p @ FieldProp(_, _) =>
        fieldCriteria(p.criteria).flatMap { criteria =>
          val fieldIsOk = Valid.fieldProp(p.field, criteria)
          p.field match {
            case \/-(f) => check1(fields.contains(f), fieldIsOk)
            case -\/(_) => \/-(fieldIsOk)
          }
        }

      case a: AnyOf[Boolean \/ Valid] =>
        val as = (a.head +: a.tail.whole).iterator.map(_.toOption).filterDefined.toVector
        as.length match {
          case 0 => -\/(false)
          case 1 => \/-(as.head)
          case _ => \/-(Valid(AnyOf(as.head, NonEmptyVector force as.tail)))
        }

      case a: AllOf[Boolean \/ Valid] =>
        @tailrec
        def go(rem: Vector[Boolean \/ Valid], acc: Vector[Valid]): Boolean \/ Valid =
          rem.headOption match {
            case Some(exit @ -\/(false)) => exit
            case Some(-\/(true))         => go(rem.tail, acc)
            case Some(\/-(v))            => go(rem.tail, acc :+ v)
            case None =>
              NonEmptyVector.option(acc) match {
                case Some(clauses) => \/-(Valid(AllOf(clauses)))
                case None          => -\/(true)
              }
          }
        go(a.clauses.whole, Vector.empty)

      case a: Scoped[Valid.Scope, Result] =>

        def filterScope(sub: Valid): Result = {
          val newScopes =
            if (fields.isEmpty)
              a.scope.whole
            else
              a.scope.whole.filterNot {
                case Scope.Derivation(f) => f.exists(fields.contains)
              }

          NonEmptySet.option(newScopes) match {
            case Some(ss) => \/-(Fix(Scoped(a.main, ss, sub)))
            case None     => -\/(true)
          }
        }

        a.clause match {
          case -\/(true)  => -\/(true)
          case -\/(false) => filterScope(constFalse)
          case \/-(sub)   => filterScope(sub)
        }
    }
  }
}