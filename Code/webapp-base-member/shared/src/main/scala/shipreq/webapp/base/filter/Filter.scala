package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.recursion._
import scalaz.Traverse
import shipreq.base.util.Identity
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{FilterDead, HideDead, Req, ReqTypePos}
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.text.{PlainText, TextSearch}

object Filter {
  import Implicits._

  type Validator = FAlgebraM[String \/ *, PotentialF, Valid]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Potential = Fix[PotentialF]

  type PotentialF[+F] = FilterAst[
    Potential.Attr,
    Potential.Field,
    Potential.FieldCriteria,
    Potential.IssueCat,
    Potential.HashTag,
    Potential.ReqSet,
    Potential.ReqType,
    F]

  object Potential extends FilterAst.Dsl {
    type Attr          = String
    type Field         = String
    type FieldCriteria = String
    type IssueCat      = String
    type HashTag       = data.HashRefKey
    type ReqSubset     = IntensionalReqSet[data.ReqType.Mnemonic]
    type ReqSet        = NonEmptyVector[ReqSubset]
    type ReqType       = data.ReqType.Mnemonic

    def reqSet(i1: ReqSubset, in: ReqSubset*): ReqSet =
      NonEmptyVector(i1, in.toVector)

    def validate(pf: Potential, validator: Validator): String \/ Filter.Valid =
      Recursion.cataM[String \/ *, PotentialF, Valid](validator)(pf)

    def toText(f: Potential): String =
      AtomOrComposite.cata(FilterAlgebra.unparse)(f)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Valid = Fix[ValidF]

  type ValidF[+F] = FilterAst[
    Valid.Attr,
    Valid.Field,
    Valid.FieldCriteria,
    Valid.IssueCat,
    Valid.HashTag,
    Valid.ReqSet,
    Valid.ReqType,
    F]

  object Valid extends FilterAst.Dsl {
    type Attr          = FilterAst.Attr
    type Field         = data.SpecialBuiltInField.FilterOk \/ data.FieldId
    type IssueCat      = IssueCategory
    type HashTag       = data.CustomIssueTypeId \/ data.ApplicableTagId
    type ReqSubset     = IntensionalReqSet[data.ReqTypeId]
    type ReqSet        = NonEmptyVector[ReqSubset]
    type ReqType       = data.ReqTypeId

    sealed trait FieldCriteria

    object FieldCriteria {
      final case class Attr         (attr: FilterAst.FieldAttr)       extends FieldCriteria
      final case class ReqTypePosSet(values: NonEmptySet[ReqTypePos]) extends FieldCriteria

      implicit def univEq: UnivEq[FieldCriteria] = UnivEq.derive
    }

    def reqSet(i1: ReqSubset, in: ReqSubset*): ReqSet =
      NonEmptyVector(i1, in.toVector)

    def tag  (id: data.ApplicableTagId)  : Valid = apply(FilterAst.HashRef(\/-(id)))
    def issue(id: data.CustomIssueTypeId): Valid = apply(FilterAst.HashRef(-\/(id)))

    def exists(text   : FilterAst.Text  => Boolean,
               regex  : FilterAst.Regex => Boolean,
               hashRef: HashTag         => Boolean,
               attr   : Attr            => Boolean,
               field  : Field           => Boolean,
               reqSet : ReqSet          => Boolean,
               reqType: ReqType         => Boolean): Valid => Boolean =
      RecursionFn.cata[ValidF, Boolean] {
        case c: FilterAst.Text                                 => text (c)
        case c: FilterAst.Regex                                => regex(c)
        case c: FilterAst.HashRef       [HashTag]              => hashRef(c.value)
        case c: FilterAst.Presence      [Attr]                 => attr(c.attr)
        case c: FilterAst.FieldProp     [Field, FieldCriteria] => field(c.field)
        case _: FilterAst.HasIssue      [IssueCat]             => false
        case c: FilterAst.ReqType       [ReqType]              => reqType(c.reqType)
        case c: FilterAst.ImpliesAnyOf  [ReqSet]               => reqSet(c.reqs)
        case c: FilterAst.ImpliedByAnyOf[ReqSet]               => reqSet(c.reqs)
        case c: FilterAst.Reqs          [ReqSet]               => reqSet(c.reqs)
        case c: FilterAst.Not           [Boolean]              => c.clause
        case c: FilterAst.AllOf         [Boolean]              => c.clauses.exists(Identity.apply)
        case c: FilterAst.AnyOf         [Boolean]              => c.head || c.tail.exists(Identity.apply)
      }

    def toText(cfg: data.ProjectConfig, f: Valid): String =
      Potential.toText(
        Recursion.cata(FilterAlgebra.unvalidate(cfg))(f))

    type Compiler = Valid => CompiledFilter

    def compiler(p                    : data.Project,
                 projectText          : PlainText.ForProject.NoCtx,
                 textSearch           : TextSearch,
                 filterDead           : FilterDead,
                 applyFilterDeadToReqs: Boolean): Compiler = {
      val extensional = FilterAlgebra.makeExtensional(p)
      val compile = FilterAlgebra.compile(
        p,
        filterDead,
        projectText,
        textSearch,
        p.dataLogic.issueLookup(filterDead),
        p.dataLogic.tagLookup(filterDead))

      val compiler: Compiler =
        v => Recursion.cata(compile)(Recursion.cata(extensional)(v))

      if (applyFilterDeadToReqs && filterDead.is(HideDead))
        v => {
          val filter   = compiler(v)
          val reqTypes = p.config.reqTypes
          val live     = filterDead.filterFn.contramap((_: Req).live(reqTypes))
          filter.copy(req = live && filter.req)
        }
      else
        compiler
    }

    def remove(fields  : Set[data.FieldId  ] = Set.empty,
               reqTypes: Set[data.ReqTypeId] = Set.empty,
              ): Valid => Boolean \/ Valid =
      RecursionFn.cata(FilterAlgebra.remove(fields, reqTypes))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Extensional = Fix[ExtensionalF]

  type ExtensionalF[+F] = FilterAst[
    Extensional.Attr,
    Extensional.Field,
    Extensional.FieldCriteria,
    Extensional.IssueCat,
    Extensional.HashTag,
    Extensional.ReqSet,
    Extensional.ReqType,
    F]

  object Extensional extends FilterAst.Dsl {
    type Attr          = Valid.Attr
    type Field         = Valid.Field
    type FieldCriteria = Valid.FieldCriteria
    type IssueCat      = Valid.IssueCat
    type HashTag       = Valid.HashTag
    type ReqSet        = Set[data.ReqId]
    type ReqType       = Valid.ReqType
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Implicits {

    implicit val traverseFilterPotential: Traverse[PotentialF] =
      FilterAst.traverse

    implicit val traverseFilterValid: Traverse[ValidF] =
      FilterAst.traverse

    implicit val traverseFilterExtensional: Traverse[ExtensionalF] =
      FilterAst.traverse

    implicit val univEqFilterPotential: UnivEq[Potential] = {
      import Potential._
      FilterAst.univEqFix[Attr, Field, FieldCriteria, IssueCat, HashTag, ReqSet, ReqType]
    }

    implicit val univEqFilterValid: UnivEq[Valid] = {
      import Valid._
      FilterAst.univEqFix[Attr, Field, FieldCriteria, IssueCat, HashTag, ReqSet, ReqType]
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def parseAndValidate(input: String, validator: Validator): (FilterParser.Failure \/ String) \/ Option[Filter.Valid] =
    FilterParser.parse(input) match {
      case \/-(Some(f)) => Potential.validate(f, validator).bimap(\/-(_), Some(_))
      case \/-(None)    => \/-(None)
      case f@ -\/(_)    => -\/(f)
    }

}
