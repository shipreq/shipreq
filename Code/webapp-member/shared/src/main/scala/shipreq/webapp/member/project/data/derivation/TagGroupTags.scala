package shipreq.webapp.member.project.data.derivation

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.member.project.data._

/** This is a the full set of [[ApplicableTag]]s for a given [[TagGroup]].
  *
  * Tags are ordered.
  */
final case class TagGroupTags(tags: Vector[ApplicableTag]) {
  def isEmpty = tags.isEmpty

  private val idSet = tags.iterator.map(_.id).toSet

  val contains: ApplicableTagId => Boolean =
    idSet.contains

  lazy val abbreviations =
    new TagGroupTags.Abbreviations(tags)

}

object TagGroupTags {

  val empty =
    apply(Vector.empty)

  private[data] def derive(tags: Tags, tagGroupId: TagGroupId, fd: FilterDead): TagGroupTags = {
    val flatTags = tags.flatRowsWithRoots(Set1(tagGroupId), fd)
    val atags = flatTags.iterator.map(_.tag).filterSubType[ApplicableTag].toVector
    TagGroupTags(atags)
  }

  private final val MinAbbreviationLen = 3

  final class Abbreviations private[TagGroupTags](tags: Vector[ApplicableTag]) {

    private def go(len: Int,
                   rem: Vector[ApplicableTag],
                   res: List[(ApplicableTag, String)]): List[(ApplicableTag, String)] =
      if (rem.isEmpty)
        res
      else {
        var seen   = Set.empty[String]
        var newRem = Vector.empty[ApplicableTag]
        var newRes = List.empty[(ApplicableTag, String)]
        for (tag <- rem) {
          val name             = tag.name
          val abb              = name.take(len)
          def hasMore          = abb.length < name.length
          def suffixIsAlphaNum = abb.takeRight(MinAbbreviationLen).forall(_.isLetterOrDigit)
          def nextIsAlphaNum   = name.charAt(abb.length).isLetterOrDigit

          if (hasMore && !suffixIsAlphaNum && nextIsAlphaNum) {
            // Tag is multi word and we've captured less than MinAbbreviationLen chars of the last word
            newRem :+= tag
          } else if (seen.contains(abb)) {
            // Duplicate abbreviation detected
            newRem :+= tag
            newRes = newRes.filter {
              case (t, a) => if (a == abb) {
                newRem :+= t
                false // remove
              } else
                true // retain
            }
          } else {
            // Successful abbreviation
            seen += abb
            newRes ::= ((tag, abb))
          }
        }
        go(len + 1, newRem, if (res.isEmpty) newRes else newRes ::: res)
      }

    private var rawResults = {
      val (a, b) = tags.partition(_.name.forall(_.isLetterOrDigit))
      val x = go(MinAbbreviationLen, a, Nil) // single words first
      go(MinAbbreviationLen, b, x)
    }

    private val byId =
      rawResults.iterator.map(_.map1(_.id)).toMap

    private val byName =
      rawResults.iterator
        .flatMap { case (tag, abb) =>
          val full = tag.name.toLowerCase
          @tailrec
          def go(len: Int, res: List[(String, ApplicableTag)]): List[(String, ApplicableTag)] =
            if (len == full.length)
              (full, tag) :: res
            else
              go(len + 1, (full.take(len), tag) :: res)
          go(abb.length, Nil)
        }
        .toMap

    rawResults = null // free memory

    def apply(tag: ApplicableTag): String =
      byId.getOrElse(tag.id, tag.name)

    def getByName(s: String): Option[ApplicableTag] =
      byName.get(s.toLowerCase)
  }

}