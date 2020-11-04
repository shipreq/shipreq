package shipreq.webapp.member.filter

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.{OptionalBoolFn, Util}
import shipreq.webapp.member.data._

/**
  * A filter that's been compiled into executable functions, ready to be applied to a project.
  */
final case class CompiledFilter(req        : OptionalBoolFn[Req],
                                codeGroup  : OptionalBoolFn[CodeGroup],
                                manualIssue: OptionalBoolFn[ManualIssue],
                                derivation : CompiledFilter.Derivation[Req],
                               ) {

  def isEmpty: Boolean =
    req.isEmpty && codeGroup.isEmpty && manualIssue.isEmpty

  @inline def nonEmpty: Boolean =
    !isEmpty

  def unary_! : CompiledFilter =
    CompiledFilter(!req, !codeGroup, !manualIssue, !derivation)

  def &&(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req && that.req,
      codeGroup && that.codeGroup,
      manualIssue && that.manualIssue,
      derivation && that.derivation,
    )

  def ||(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req || that.req,
      codeGroup || that.codeGroup,
      manualIssue || that.manualIssue,
      derivation || that.derivation,
    )
}

object CompiledFilter {

  def empty: CompiledFilter =
    apply(
      OptionalBoolFn.empty,
      OptionalBoolFn.empty,
      OptionalBoolFn.empty,
      Derivation.emptyReq,
    )

  def fail: CompiledFilter =
    apply(
      OptionalBoolFn.fail,
      OptionalBoolFn.fail,
      OptionalBoolFn.fail,
      Derivation.emptyReq,
    )

  /** Filters for derivative tag factors.
    *
    * When these filters are in use and a req contains derived tags, these filters are applied to all the sources of the
    * derived tags such that if a derived tag doesn't have a filter-approved source, it is moved into the non-primary
    * portion of tag expansions. What the means is that 1) said tag can never have it's own expansion row, 2) it is
    * still presented to users but in a way that doesn't draw attention (i.e. lower opacity, no colouring).
    *
    * @see https://shipreq.com/project/d6My#/reqs/FR-47
    */
  final class Derivation[A](private[Derivation] val all     : OptionalBoolFn[A],
                            private[Derivation] val perField: Map[CustomField.Tag.Id, A => Boolean]) {

    def isEmpty: Boolean =
      all.isEmpty && perField.isEmpty

    @inline def nonEmpty: Boolean =
      !isEmpty

    def unary_! : Derivation[A] =
      new Derivation(!all, perField.mapValuesNow(!_))

    def &&(that: Derivation[A]): Derivation[A] =
      new Derivation(
        all && that.all,
        Util.mergeMaps(perField, that.perField)(_ && _))

    def ||(that: Derivation[A]): Derivation[A] =
      new Derivation(
        all || that.all,
        Util.mergeMaps(perField, that.perField)(_ || _))

    def forAllTags: OptionalBoolFn[A] =
      all

    def forTagField(f: CustomField.Tag.Id): OptionalBoolFn[A] =
      perField.get(f) match {
        case Some(g) => all && OptionalBoolFn(g)
        case None    => all
      }
  }

  object Derivation {
    def empty[A]: Derivation[A] =
      new Derivation(OptionalBoolFn.empty, Map.empty)

    val emptyReq: Derivation[Req] =
      empty
  }
}
