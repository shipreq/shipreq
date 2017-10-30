package shipreq.webapp.base.filter

import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.base.data._

/**
  * A filter that's been compiled into executable functions, ready to be applied to a project.
  */
final case class CompiledFilter(req      : OptionalBoolFn[Req],
                                codeGroup: OptionalBoolFn[CodeGroup]) {

  def unary_! : CompiledFilter =
    CompiledFilter(!req, !codeGroup)

  def &&(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req && that.req,
      codeGroup && that.codeGroup)

  def ||(that: CompiledFilter): CompiledFilter =
    CompiledFilter(
      req || that.req,
      codeGroup || that.codeGroup)

  def isEmpty: Boolean =
    req.isEmpty && codeGroup.isEmpty

  def nonEmpty = !isEmpty
}

object CompiledFilter {
  def empty: CompiledFilter =
    apply(OptionalBoolFn.empty, OptionalBoolFn.empty)
}