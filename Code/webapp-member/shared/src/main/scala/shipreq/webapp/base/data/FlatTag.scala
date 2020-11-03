package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
final case class FlatTag(tag: Tag, depth: Int, parentPath: Vector[TagId], status: FlatTag.Status) {
  @inline def id: TagId = tag.id

  def key: String =
    if (depth == 0)
      id.value.toString
    else {
      val sb = new StringBuilder
      parentPath.foreach { p =>
        sb append p.value
        sb append '.'
      }
      sb append id.value
      sb.toString()
    }

  def indentedName =
    s"${FlatTag.indentation(depth)}${tag.name}"
}

object FlatTag {
  sealed trait Status
  object Status {
    case object Good              extends Status
    case object Bad               extends Status
    case object BadParentGoodKids extends Status
    implicit def equality: UnivEq[Status] = UnivEq.derive
  }

  sealed trait FilterPolicy
  object FilterPolicy {

    case object OmitNothing extends FilterPolicy

    /** Omit subtrees consisting solely of bad nodes.
      *
      * Bad nodes with good children are retained.
      */
    case object OmitBadBranches extends FilterPolicy

    /** Omit subtrees with bad roots, even if the subtree contains good nodes. */
    case object OmitAnythingWithBadParent extends FilterPolicy

    implicit def equality: UnivEq[FilterPolicy] = UnivEq.derive
  }

  implicit def equality: UnivEq[FlatTag] = UnivEq.derive

  val indentation =
    Memo.int("\u00A0\u00A0" * _)

  def flatRows(topLvlIds: Set[TagId], lookup: TagId => TagInTree)
              (isGood: Tag => Boolean, policy: FilterPolicy): Vector[FlatTag] = {
    import Status._
    import FilterPolicy._

    val omitAnythingWithBadParent = policy ==* OmitAnythingWithBadParent
    val omitNothing               = policy ==* OmitNothing

    def go(r: Vector[FlatTag], t: TagInTree, depth: Int, parentPath: Vector[TagId]): Vector[FlatTag] =
      if (isGood(t.tag)) {
        var result = r :+ FlatTag(t.tag, depth, parentPath, Good)
        // Append children directly
        if (t.children.nonEmpty) {
          val nextDepth = depth + 1
          val nextPP = parentPath :+ t.id
          t.children.foreach(id => result = go(result, lookup(id), nextDepth, nextPP))
        }
        result
      } else if (omitAnythingWithBadParent)
        r
      else {
        // Process children separately
        var cs = Vector.empty[FlatTag]
        if (t.children.nonEmpty) {
          val nextDepth = depth + 1
          val nextPP = parentPath :+ t.id
          cs = t.children.foldLeft(cs)((q, id) => go(q, lookup(id), nextDepth, nextPP))
        }
        val goodKids = cs.exists(_.status ==* Good)

        def result(s: Status) = (r :+ FlatTag(t.tag, depth, parentPath, s)) ++ cs
        if (goodKids)
          result(BadParentGoodKids)
        else if (omitNothing)
          result(Bad)
        else
          r
      }

    val topLvl = MutableArray(topLvlIds.iterator.map(lookup)).sortBy(_.tag.name)
    topLvl.array.foldLeft(Vector.empty[FlatTag])(go(_, _, 0, Vector.empty))
  }
}
