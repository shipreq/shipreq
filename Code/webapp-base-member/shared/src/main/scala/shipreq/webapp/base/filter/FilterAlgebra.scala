package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.recursion._
import japgolly.microlibs.utils.ConciseIntSetFormat
import japgolly.univeq._
import java.util.regex.Pattern
import scalaz.{-\/, Functor, Traverse, \/, \/-}
import scalaz.syntax.traverse1._
import shipreq.base.util.{OptionalBoolFn, TransitiveClosure}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.On
import shipreq.webapp.base.data.DataLogic.{IssueLookup, TagLookup}
import shipreq.webapp.base.issue.Issues
import shipreq.webapp.base.text.{Atom, Grammar, PlainText, TextSearch}

/** Algebras:
  *
  * unparse        : FAlgebra [             PotentialF,   AtomOrComposite[String]]
  * validate       : FAlgebraM[String \/ ?, PotentialF,   Valid                  ]
  * unvalidate     : FAlgebra [             ValidF,       Potential              ]
  * makeExtensional: FAlgebra [             ValidF,       Extensional            ]
  * compile        : FAlgebra [             ExtensionalF, CompiledFilter         ]
  */
object FilterAlgebra {
  import Filter._
  import FilterAst._

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

  def validate(cfg: data.ProjectConfig): FAlgebraM[String \/ ?, PotentialF, Valid] = {
    type R = String \/ Valid

    def byAttr(attrStr: String, f: Attr => ValidF[Nothing]): R =
      Attr(attrStr) match {
        case Some(a) => \/-(Valid(f(a)))
        case None    => -\/(s"Unknown attribute: '$attrStr'. Known: ${Attr.availableText}.") // English
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
        case e: Throwable => -\/(s"Invalid regex: /$regex/")
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
      case HashRef       (\/-(id)) => Potential(HashRef       (cfg.tags.atag(id).key))
      case HashRef       (-\/(id)) => Potential(HashRef       (cfg.customIssueType(id).key))
      case Presence      (attr)    => Potential(Presence      (attr.name))
      case ImpliesAnyOf  (reqs)    => Potential(ImpliesAnyOf  (convReqSet(reqs)))
      case ImpliedByAnyOf(reqs)    => Potential(ImpliedByAnyOf(convReqSet(reqs)))
      case Reqs          (reqs)    => Potential(Reqs          (convReqSet(reqs)))
      case ReqType       (id)      => Potential(ReqType       (convReqType(id)))
      case HasIssue      (on, c)   => Potential(HasIssue      (on, c.map(FilterAst.issueCategoryToStr)))
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
      case ImpliesAnyOf  (reqs)        => Extensional(ImpliesAnyOf  (lookupReqSet(reqs)))
      case ImpliedByAnyOf(reqs)        => Extensional(ImpliedByAnyOf(lookupReqSet(reqs)))
      case Reqs          (reqs)        => Extensional(Reqs          (lookupReqSet(reqs)))
      case c: Regex                    => Extensional(c)
      case c: Text                     => Extensional(c)
      case c: HashRef [Valid.HashTag]  => Extensional(c)
      case c: Presence[Valid.Attr]     => Extensional(c)
      case c: HasIssue[Valid.IssueCat] => Extensional(c)
      case c: ReqType [Valid.ReqType]  => Extensional(c)
      case c: Not     [Extensional]    => Extensional(c)
      case c: AllOf   [Extensional]    => Extensional(c)
      case c: AnyOf   [Extensional]    => Extensional(c)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def compile(p          : data.Project,
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

    def make(req      : Req       => Boolean = null,
             codeGroup: CodeGroup => Boolean = null) =
      CompiledFilter(
        req       = OptionalBoolFn(Option(req)),
        codeGroup = OptionalBoolFn(Option(codeGroup)))

    def byTag(f: Set[ApplicableTagId] => Boolean) =
      make(req = r => tagLookup(r.id) exists f)

    def byIssue(f: Issues.ForSource => Boolean) =
      make(
        r => f(issuesBySource(r.id)),
        g => f(issuesBySource(g.id)))

    def byCustomIssueType(f: Vector[Atom.AnyIssue] => Boolean) =
      make(
        r => f(issueLookup.forReq(r.id)),
        g => f(issueLookup.forReqCode(g.id)))

    def byImplication(reqs: Extensional.ReqSet, tc: TransitiveClosure[ReqId]) = {
      val whitelist = reqs.foldLeft(UnivEq.emptySet[ReqId])(_ ++ tc(_))
      make(req = whitelist contains _.id)
    }

    def byText(substr: String) = {
      val (fa, fb) = textSearch.ignoreCaseSingleSpaces.searchFilter(substr)
      CompiledFilter(
        req       = fa.contramap((_: Req).id),
        codeGroup = fb.contramap((_: CodeGroup).id))
    }

    def byRegex(regex: String) = {
      val pat = Pattern.compile(regex)
      val m: String => Boolean = pat.matcher(_).matches
      make(
        r => {
          def title  = m(projectText reqTitle r)
          def custom = p.config.liveCustomTextFields.exists(f => projectText.customTextField(f.id)(r) exists m)
          title || custom
        },
        g => m(projectText codeGroupTitle g))
    }

    var alg: ExtensionalF[CompiledFilter] => CompiledFilter = null
    alg = {
      case Text          (substr, _)     => byText(substr)
      case Reqs          (reqs)          => make(req = r => reqs.contains(r.id))
      case ImpliesAnyOf  (reqs)          => byImplication(reqs, p.implicationTgtToSrcTC)
      case ImpliedByAnyOf(reqs)          => byImplication(reqs, p.implicationSrcToTgtTC)
      case ReqType       (rt)            => make(req = _.reqTypeId ==* rt)
      case Presence      (Attr.AnyIssue) => byIssue(_.issues.nonEmpty)
      case Presence      (Attr.AnyTag)   => byTag(_.nonEmpty)
      case HashRef       (-\/(issue))    => byCustomIssueType(_.exists(_.typ ==* issue))
      case HashRef       (\/-(tag))      => byTag(_ contains tag)
      case Regex         (regex)         => byRegex(regex)
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
  // TODO also auto-complete
}