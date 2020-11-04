package shipreq.webapp.member.util

import shipreq.webapp.member.data.ReqCode

/**
 * Representation of a ReqCode when viewed like a tree.
 */
case class ReqCodeTreeItem(indent: Vector[ReqCodeTreeItem.Indent], suffix: ReqCode.Value)

object ReqCodeTreeItem {
  sealed trait Indent

  /**
   * Unit of indentation for when a ReqCode is a direct child of the one above.
   *
   * `a.b.c.d` after `a.b.c` would result in 3 of these with `.d` as the suffix.
   */
  case object IndentChild extends Indent

  /**
   * Unit of indentation that consumes a fixed number of spaces.
   *
   * @param length ≥ 1 The length of the common node (excluding the ".").
   */
  case class IndentSpace(length: Int) extends Indent

  implicit def indentSpaceEquality: UnivEq[IndentSpace]     = UnivEq.derive
  implicit def indentEquality     : UnivEq[Indent]          = UnivEq.derive
  implicit def itemEquality       : UnivEq[ReqCodeTreeItem] = UnivEq.derive
}
