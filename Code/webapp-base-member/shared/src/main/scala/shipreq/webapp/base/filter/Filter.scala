package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.recursion._
import scalaz.{-\/, Traverse, \/, \/-}
import shipreq.base.util.Identity
import shipreq.base.util.univeq._
import shipreq.webapp.base.data
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.text.{PlainText, TextSearch}

object Filter {
  import Implicits._

  type Validator = FAlgebraM[String \/ ?, PotentialF, Valid]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Potential = Fix[PotentialF]

  type PotentialF[+F] = FilterAst[
    Potential.Attr,
    Potential.IssueCat,
    Potential.HashTag,
    Potential.ReqSet,
    Potential.ReqType,
    F]

  object Potential extends FilterAst.Dsl {
    type Attr      = String
    type IssueCat  = String
    type HashTag   = data.HashRefKey
    type ReqSubset = IntensionalReqSet[data.ReqType.Mnemonic]
    type ReqSet    = NonEmptyVector[ReqSubset]
    type ReqType   = data.ReqType.Mnemonic

    def reqSet(i1: ReqSubset, in: ReqSubset*): ReqSet =
      NonEmptyVector(i1, in.toVector)

    def validate(pf: Potential, validator: Validator): String \/ Filter.Valid =
      Recursion.cataM[String \/ ?, PotentialF, Valid](validator)(pf)

    def toText(f: Potential): String =
      AtomOrComposite.cata(FilterAlgebra.unparse)(f)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Valid = Fix[ValidF]

  type ValidF[+F] = FilterAst[
    Valid.Attr,
    Valid.IssueCat,
    Valid.HashTag,
    Valid.ReqSet,
    Valid.ReqType,
    F]

  object Valid extends FilterAst.Dsl {
    type Attr      = FilterAst.Attr
    type IssueCat  = IssueCategory
    type HashTag   = data.CustomIssueTypeId \/ data.ApplicableTagId
    type ReqSubset = IntensionalReqSet[data.ReqTypeId]
    type ReqSet    = NonEmptyVector[ReqSubset]
    type ReqType   = data.ReqTypeId

    def reqSet(i1: ReqSubset, in: ReqSubset*): ReqSet =
      NonEmptyVector(i1, in.toVector)

    def tag  (id: data.ApplicableTagId)  : Valid = apply(FilterAst.HashRef(\/-(id)))
    def issue(id: data.CustomIssueTypeId): Valid = apply(FilterAst.HashRef(-\/(id)))

    def exists(text   : FilterAst.Text  => Boolean,
               regex  : FilterAst.Regex => Boolean,
               hashRef: HashTag         => Boolean,
               attr   : Attr            => Boolean,
               reqSet : ReqSet          => Boolean,
               reqType: ReqType         => Boolean): Valid => Boolean =
      RecursionFn.cata[ValidF, Boolean] {
        case c: FilterAst.Text                     => text (c)
        case c: FilterAst.Regex                    => regex(c)
        case c: FilterAst.HashRef       [HashTag]  => hashRef(c.value)
        case c: FilterAst.Presence      [Attr]     => attr(c.attr)
        case _: FilterAst.HasIssue      [IssueCat] => false
        case c: FilterAst.ReqType       [ReqType]  => reqType(c.reqType)
        case c: FilterAst.ImpliesAnyOf  [ReqSet]   => reqSet(c.reqs)
        case c: FilterAst.ImpliedByAnyOf[ReqSet]   => reqSet(c.reqs)
        case c: FilterAst.Reqs          [ReqSet]   => reqSet(c.reqs)
        case c: FilterAst.Not           [Boolean]  => c.clause
        case c: FilterAst.AllOf         [Boolean]  => c.clauses.exists(Identity.apply)
        case c: FilterAst.AnyOf         [Boolean]  => c.head || c.tail.exists(Identity.apply)
      }

    def toText(cfg: data.ProjectConfig, f: Valid): String =
      Potential.toText(
        Recursion.cata(FilterAlgebra.unvalidate(cfg))(f))

    type Compiler = Valid => CompiledFilter

    def compiler(p          : data.Project,
                 projectText: PlainText.ForProject.NoCtx,
                 textSearch : TextSearch,
                 filterDead : FilterDead): Compiler = {
      val extensional = FilterAlgebra.makeExtensional(p)
      val compile = FilterAlgebra.compile(
        p,
        projectText,
        textSearch,
        p.dataLogic.issueLookup(filterDead),
        p.dataLogic.tagLookup(filterDead))
      v => Recursion.cata(compile)(Recursion.cata(extensional)(v))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Extensional = Fix[ExtensionalF]

  type ExtensionalF[+F] = FilterAst[
    Extensional.Attr,
    Extensional.IssueCat,
    Extensional.HashTag,
    Extensional.ReqSet,
    Extensional.ReqType,
    F]

  object Extensional extends FilterAst.Dsl {
    type Attr     = Valid.Attr
    type IssueCat = Valid.IssueCat
    type HashTag  = Valid.HashTag
    type ReqSet   = Set[data.ReqId]
    type ReqType  = Valid.ReqType
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
      FilterAst.univEqFix[Attr, IssueCat, HashTag, ReqSet, ReqType]
    }

    implicit val univEqFilterValid: UnivEq[Valid] = {
      import Valid._
      FilterAst.univEqFix[Attr, IssueCat, HashTag, ReqSet, ReqType]
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
