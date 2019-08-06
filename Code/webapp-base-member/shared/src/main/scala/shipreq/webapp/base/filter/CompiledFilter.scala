package shipreq.webapp.base.filter

import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.base.data._

/**
  * A filter that's been compiled into executable functions, ready to be applied to a project.
  */
final case class CompiledFilter(req        : OptionalBoolFn[Req],
                                codeGroup  : OptionalBoolFn[CodeGroup],
                                manualIssue: OptionalBoolFn[ManualIssue],
                               ) {

  def unary_! : CompiledFilter =
    CompiledFilter(!req, !codeGroup, !manualIssue)

  def &&(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req && that.req,
      codeGroup && that.codeGroup,
      manualIssue && that.manualIssue,
    )

  def ||(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req || that.req,
      codeGroup || that.codeGroup,
      manualIssue || that.manualIssue,
    )

  def isEmpty: Boolean =
    req.isEmpty && codeGroup.isEmpty && manualIssue.isEmpty

  def nonEmpty = !isEmpty
}

object CompiledFilter {
  def empty: CompiledFilter =
    apply(
      OptionalBoolFn.empty,
      OptionalBoolFn.empty,
      OptionalBoolFn.empty,
    )
}