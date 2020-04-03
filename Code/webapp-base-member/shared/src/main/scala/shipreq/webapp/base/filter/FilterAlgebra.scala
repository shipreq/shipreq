package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.recursion._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.ConciseIntSetFormat
import japgolly.univeq._
import java.util.regex.Pattern
import scala.annotation.tailrec
import scalaz.{-\/, Functor, Traverse, \/, \/-}
import scalaz.std.option._
import scalaz.syntax.traverse1._
import shipreq.base.util.{Applicable, OptionalBoolFn, TransitiveClosure}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{FilterDead, On}
import shipreq.webapp.base.data.DataLogic.{IssueLookup, TagLookup}
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

  def quoteFieldName(s: String): String =
    if (s.exists(c => c == ':' || c.isWhitespace))
      "\"" + s + "\""
    else
      s

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

    {
      case Text          (text, None)        => text
      case Text          (text, Some(qChar)) => qChar ~ text ~ qChar
      case Regex         (text)              => '/' ~ text.replace("/", "\\/") ~ '/'
      case ReqType       (value)             => value.value
      case HashRef       (text)              => Grammar.hashRefKey.prefix ~ text.value
      case FieldProp     (field, attr)       => "field:" ~ quoteFieldName(field) ~ ":" ~ attr
      case HasIssue      (on, criteria)      => "has:issue:" ~ (if (on is On) "" else "-") ~ criteria.mkString(",")
      case ImpliesAnyOf  (reqs)              => "implies:" ~ fmtReqs(reqs, ',')
      case ImpliedByAnyOf(reqs)              => "impliedBy:" ~ fmtReqs(reqs, ',')
      case Reqs          (reqs)              => fmtReqs(reqs, ' ')
      case Presence      (attr)              => "has:" ~ attr
      case Not           (clause)            => '-' ~ clause.atom
      case AllOf         (clauses)           => composite("(", clauses.iterator.map(_.atom).mkString(" "),  ")")
      case AnyOf         (c, cs)             => composite("(", (c +: cs).iterator.map(_.atom).mkString(" | "),  ")")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def validate(cfg: data.ProjectConfig): FAlgebraM[String \/ *, PotentialF, Valid] = {
    type R = String \/ Valid

    def byAttr(attrStr: String, f: Attr => ValidF[Nothing]): R =
      Attr(attrStr) match {
        case Some(a) => \/-(Valid(f(a)))
        case None    => -\/(s"Unknown attribute: '$attrStr'. Known: ${Attr.availableText}.") // English
      }

    def lookupFieldAttr(attrStr: String)(f: FieldAttr => R): R =
      FieldAttr(attrStr) match {
        case Some(a) => f(a)
        case None    => -\/(s"Unknown attribute: '$attrStr'. Known: ${FieldAttr.availableText}.") // English
      }

    def byHashTag(k: data.HashRefKey): R =
      cfg.hashRefLookup(k.value) match {
        case Some(-\/(t)) => \/-(Valid(HashRef(\/-(t.id))))
        case Some(\/-(i)) => \/-(Valid(HashRef(-\/(i.id))))
        case None         => -\/(s"Unknown tag or issue: '${k.value}'") // English
      }

    val reqTypesByMnemonic = cfg.reqTypes.allByMnemonic

    def lookupReqType(mn: data.ReqType.Mnemonic): String \/ data.ReqType =
      reqTypesByMnemonic.get(mn) match {
        case Some(rt) => \/-(rt)
        case None     => -\/(s"Unknown type: '${mn.value}'") // English
      }

    val lookupReqSubset: Potential.ReqSubset => String \/ Valid.ReqSubset =
      Traverse[IntensionalReqSet].traverse(_)(lookupReqType(_).map(_.reqTypeId))

    def byReqSet(reqs: Potential.ReqSet, f: Valid.ReqSet => ValidF[Nothing]): R =
      reqs.traverse1(lookupReqSubset).map(nev => Valid(f(nev)))

    def byRegex(regex: String): R =
      try {
        Pattern compile regex
        \/-(Valid(Regex(regex)))
      } catch {
        // PatternSyntaxException not available in Scala.JS
        // case e: PatternSyntaxException => error(e.getDescription)
        case _: Throwable => -\/(s"Invalid regex: /$regex/")
      }

    // explicit types here because IntelliJ is a piece of shit
    val alg: PotentialF[Valid] => String \/ Valid = {
      case HashRef       (key)   => byHashTag(key)
      case ImpliesAnyOf  (reqs)  => byReqSet(reqs, ImpliesAnyOf.apply)
      case ImpliedByAnyOf(reqs)  => byReqSet(reqs, ImpliedByAnyOf.apply)
      case Reqs          (reqs)  => byReqSet(reqs, Reqs.apply)
      case Presence      (attr)  => byAttr(attr, Presence.apply)
      case HasIssue      (on, c) => c.traverse1(FilterAst.issueCategoryFromStr).map(Valid.hasIssue(on, _))
      case Regex         (regex) => byRegex(regex)
      case ReqType       (mn)    => lookupReqType(mn).map(rt => Valid(ReqType(rt.reqTypeId)))
      case c: Text               => \/-(Valid(c))
      case c: Not  [Valid]       => \/-(Valid(c))
      case c: AllOf[Valid]       => \/-(Valid(c))
      case c: AnyOf[Valid]       => \/-(Valid(c))

      case FieldProp(fieldName, attrStr) =>
        import FieldAttr._
        import data._
        lookupFieldAttr(attrStr) { attr =>
          import SpecialBuiltInField._

          val nameLower = fieldName.toLowerCase
          def tryL      = SpecialBuiltInField.filterOkByNameLowercase.get(nameLower).map(-\/(_))
          def tryR      = cfg.fieldsByNameLowercase.get(nameLower).map(\/-(_))

          def blankOnly(f: Valid.Field, name: String): String \/ Valid =
            attr match {
              case Blank         => \/-(Valid(FieldProp(f, attr)))
              case DefaultInUse  => -\/(s"$name doesn't have defaults")
              case NotApplicable => -\/(s"$name is always applicable")
            }

          (tryL orElse tryR, attr) match {
            case (Some(\/-(f: CustomField))               , Blank | NotApplicable) => \/-(Valid(FieldProp(\/-(f.id), attr)))
            case (Some(\/-(f: CustomField.Tag))           , DefaultInUse         ) => \/-(Valid(FieldProp(\/-(f.id), attr)))
            case (Some(\/-(_: CustomField.Text))          , DefaultInUse         ) => -\/("Text fields don't have defaults.")
            case (Some(\/-(_: CustomField.Implication))   , DefaultInUse         ) => -\/("Implication fields don't have defaults.")
            case (Some(-\/(f@ Title))                     , _                    ) => blankOnly(-\/(f), f.name)
            case (Some(\/-(f@ StaticField.OtherTags))     , _                    ) => blankOnly(\/-(f), f.name)
            case (Some(\/-(StaticField.AllTags))          , _                    )
               | (Some(\/-(StaticField.ImplicationGraph)) , _                    )
               | (Some(\/-(StaticField.NormalAltStepTree)), _                    )
               | (Some(\/-(StaticField.ExceptionStepTree)), _                    )
               | (Some(\/-(StaticField.StepGraph))        , _                    ) => -\/(s"$fieldName can't be used here.")
            case (None                                    , _                    ) => -\/(s"Unknown field: '$fieldName'")
          }
        }
    }

    alg
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def unvalidate(cfg: data.ProjectConfig): FAlgebra[ValidF, Potential] = {
    val convReqType: data.ReqTypeId => data.ReqType.Mnemonic =
      cfg.reqTypes.need(_).mnemonic

    def convReqSet(x: Valid.ReqSet): Potential.ReqSet =
      x.map(Functor[IntensionalReqSet].map(_)(convReqType))

    {
      case HashRef       (\/-(id)) => Potential(HashRef       (cfg.tags.needApplicableTag(id).key))
      case HashRef       (-\/(id)) => Potential(HashRef       (cfg.customIssueTypes.need(id).key))
      case Presence      (attr)    => Potential(Presence      (attr.name))
      case ImpliesAnyOf  (reqs)    => Potential(ImpliesAnyOf  (convReqSet(reqs)))
      case ImpliedByAnyOf(reqs)    => Potential(ImpliedByAnyOf(convReqSet(reqs)))
      case Reqs          (reqs)    => Potential(Reqs          (convReqSet(reqs)))
      case ReqType       (id)      => Potential(ReqType       (convReqType(id)))
      case HasIssue      (on, c)   => Potential(HasIssue      (on, c.map(FilterAst.issueCategoryToStr)))
      case FieldProp     (f, a)    => Potential(FieldProp     (f.fold(_.name, cfg.fieldName), a.name))
      case c: Regex                => Potential(c)
      case c: Text                 => Potential(c)
      case c: Not  [Potential]     => Potential(c)
      case c: AllOf[Potential]     => Potential(c)
      case c: AnyOf[Potential]     => Potential(c)
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

    {
      case ImpliesAnyOf  (reqs)                       => Extensional(ImpliesAnyOf  (lookupReqSet(reqs)))
      case ImpliedByAnyOf(reqs)                       => Extensional(ImpliedByAnyOf(lookupReqSet(reqs)))
      case Reqs          (reqs)                       => Extensional(Reqs          (lookupReqSet(reqs)))
      case c: Regex                                   => Extensional(c)
      case c: Text                                    => Extensional(c)
      case c: FieldProp[Valid.Field, Valid.FieldAttr] => Extensional(c)
      case c: HashRef  [Valid.HashTag]                => Extensional(c)
      case c: Presence [Valid.Attr]                   => Extensional(c)
      case c: HasIssue [Valid.IssueCat]               => Extensional(c)
      case c: ReqType  [Valid.ReqType]                => Extensional(c)
      case c: Not      [Extensional]                  => Extensional(c)
      case c: AllOf    [Extensional]                  => Extensional(c)
      case c: AnyOf    [Extensional]                  => Extensional(c)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
   *
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
              tagLookup  : TagLookup): FAlgebra[ExtensionalF, CompiledFilter] = {

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

    def fieldApplicableReqOnly(fieldId: FieldId)(f: Req => Boolean): CompiledFilter = {
      val applicability = p.config.applicability.byField(fieldId)
      reqOnly(req => applicability(req.reqTypeId).is(Applicable) && f(req))
    }

    def byTag(f: Set[ApplicableTagId] => Boolean): CompiledFilter =
      make(
        req         = r => tagLookup(r.id) exists f,
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

    def byImplication(reqs: Extensional.ReqSet, tc: TransitiveClosure[ReqId]): CompiledFilter = {
      val whitelist = reqs.foldLeft(UnivEq.emptySet[ReqId])(_ ++ tc(_))
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

    def byFieldProp(fieldArg: Filter.Valid.Field, attr: FieldAttr): CompiledFilter = {
      import FieldReqTypeRules.Resolution
      (attr, fieldArg) match {

        case (FieldAttr.NotApplicable, \/-(id)) =>
          val field = p.config.fields.need(id)
          val na = p.config.reqTypesWithRes(field.fieldReqTypeRules)(Resolution.NotApplicable).map(_.reqTypeId).toSet
          reqOnly(req => na.contains(req.reqTypeId))

        case (FieldAttr.Blank, \/-(f: CustomField.Text.Id)) =>
          val text = p.content.reqTextFor(f)
          fieldApplicableReqOnly(f)(req => !text.contains(req.id))

        case (FieldAttr.Blank, \/-(f: CustomField.Implication.Id)) =>
          val imps = p.dataLogic.customFieldImps(ShowDead)(f)
          fieldApplicableReqOnly(f)(req => imps(req.id).isEmpty)

        case (FieldAttr.Blank, \/-(f: CustomField.Tag.Id)) =>
          val scope = p.config.tagFieldDistribution(filterDead).inField(f)
          fieldApplicableReqOnly(f)(req => tagLookup(req.id).all.intersect(scope).isEmpty)

        case (FieldAttr.DefaultInUse, \/-(f: CustomField.Tag.Id)) =>
          val fieldDefaultApplied = p.fieldDefaultApplied(f, filterDead)
          fieldApplicableReqOnly(f)(req => fieldDefaultApplied(req.id))

        case (FieldAttr.Blank, -\/(SpecialBuiltInField.Title)) =>
          make(
            req         = _.title.isEmpty,
            codeGroup   = _.title.isEmpty,
            manualIssue = fail,
          )

        case (FieldAttr.Blank, \/-(StaticField.OtherTags)) =>
          val scope = p.config.tagFieldDistribution(filterDead).notUsedInFields
          reqOnly(req => tagLookup(req.id).all.intersect(scope).isEmpty)

        case (FieldAttr.NotApplicable, -\/(SpecialBuiltInField.Title)) =>
          reqOnly(fail)

        case (FieldAttr.Blank,
                 \/-(StaticField.AllTags)
               | \/-(StaticField.ImplicationGraph)
               | \/-(StaticField.NormalAltStepTree)
               | \/-(StaticField.ExceptionStepTree)
               | \/-(StaticField.StepGraph)
             ) =>
               reqOnly(fail)

        case (FieldAttr.DefaultInUse,
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

    var alg: ExtensionalF[CompiledFilter] => CompiledFilter = null
    alg = {
      case Text          (substr, _)     => byText(substr)
      case Reqs          (reqs)          => reqOnly(r => reqs.contains(r.id))
      case ImpliesAnyOf  (reqs)          => byImplication(reqs, p.implicationTgtToSrcTC)
      case ImpliedByAnyOf(reqs)          => byImplication(reqs, p.implicationSrcToTgtTC)
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
      case HasIssue      (On, cs)        => byIssue(i => i.issues.nonEmpty && cs.forall(i.categories.contains))
      case HasIssue      (Off, cs)       =>
        val categories = cs.whole.toSet
        byIssue(i => i.issues.nonEmpty && i.categories.exists(!categories.contains(_)))
    }
    alg
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def remove(fields  : Set[data.FieldId  ],
             reqTypes: Set[data.ReqTypeId],
            ): FAlgebra[ValidF, Boolean \/ Valid] = {

    def check1(isBad: Boolean, ok: Valid): Boolean \/ Valid =
      if (isBad) -\/(false) else \/-(ok)

    {
      case a: Text                                   => \/-(Valid(a))
      case a: Regex                                  => \/-(Valid(a))
      case a: Presence      [Valid.Attr]             => \/-(Valid(a))
      case a: HasIssue      [Valid.IssueCat]         => \/-(Valid(a))
      case a: HashRef       [Valid.HashTag]          => \/-(Valid(a))
      case a: ImpliesAnyOf  [Valid.ReqSet]           => \/-(Valid(a))
      case a: ImpliedByAnyOf[Valid.ReqSet]           => \/-(Valid(a))
      case a: Reqs          [Valid.ReqSet]           => \/-(Valid(a))
      case a: ReqType       [Valid.ReqType]          => check1(reqTypes.contains(a.reqType), Valid(a))
      case a: FieldProp     [Valid.Field, FieldAttr] => a.field.fold(_ => \/-(Valid(a)), f => check1(fields.contains(f), Valid(a)))
      case a: Not           [Boolean \/ Valid]       => a.clause.fold(b => -\/(!b), c => \/-(Valid(Not(c))))

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
    }
  }
}