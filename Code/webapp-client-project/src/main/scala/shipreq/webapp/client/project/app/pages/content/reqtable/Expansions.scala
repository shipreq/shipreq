package shipreq.webapp.client.project.app.pages.content.reqtable

import monocle.macros.Lenses
import scalaz.Monoid
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ReqCodeTreeItem

/**
 * Replacement values for a requirement at a specific row.
 *
 * Due to sorting criteria, the same requirement may appear multiple times with certain composite values
 * split across rows. This process is dubbed expansion and this class houses the different values its corresponding row
 * will display.
 *
 * Example: if a row is implied by two sources and the table is sorted by implication-source, then the row will
 * appear twice - once for each implicatee.
 */
@Lenses
final case class Expansions(implications: Direction.Values[Expansion[Pubid]],
                            reqCodes    : Expansion[ReqCode.Value],
                            reqCodeTree : Expansion[ReqCodeTreeItem],
                            cfImps      : Map[CustomField.Implication.Id, Expansion[Pubid]],
                            cfTags      : Map[CustomField.Tag.Id, Expansion[ApplicableTagId]],
                            otherTags   : Expansion[ApplicableTagId],
                            allTags     : Expansion[ApplicableTagId],
                          ) {

  val focusedTags: TagFieldId => Vector[ApplicableTagId] = {
    case f: CustomField.Tag.Id => cfTags.get(f).fold(Vector.empty[ApplicableTagId])(_.values)
    case TagFieldId.All        => allTags.values
    case TagFieldId.Other      => otherTags.values
  }

  val unfocusedTags: TagFieldId => Vector[ApplicableTagId] = {
    case f: CustomField.Tag.Id => cfTags.get(f).fold(Vector.empty[ApplicableTagId])(_.nonPrimary)
    case TagFieldId.All        => allTags.nonPrimary
    case TagFieldId.Other      => otherTags.nonPrimary
  }

  // Workaround for stupid https://issues.scala-lang.org/browse/SI-6391
  def copyReqCodes   (nv: Vector[ReqCode.Value]  ): Expansions = copy(reqCodes = reqCodes.copy(values = nv))
  def copyReqCodeTree(nv: Vector[ReqCodeTreeItem]): Expansions = copy(reqCodeTree = reqCodeTree.copy(values = nv))

  def impsForCF(id: CustomField.Implication.Id): Expansion[Pubid] =
    cfImps.getOrElse(id, Expansion.empty)

  def tagsForCF(id: CustomField.Tag.Id): Expansion[ApplicableTagId] =
    cfTags.getOrElse(id, Expansion.empty)
}

object Expansions {
  val empty: Expansions =
    apply(
      Direction.Values both Expansion.empty,
      Expansion.empty,
      Expansion.empty,
      UnivEq.emptyMap,
      UnivEq.emptyMap,
      Expansion.empty,
      Expansion.empty,
    )

  implicit def equality: UnivEq[Expansions] = UnivEq.derive

  implicit val reqCodeTreeM: Monoid[Vector[ReqCodeTreeItem]] =
    new Monoid[Vector[ReqCodeTreeItem]] {
      override def zero =  Vector.empty
      override def append(f1: Vector[ReqCodeTreeItem], f2: => Vector[ReqCodeTreeItem]) = f1 ++ f2
    }

  implicit val monoid: Monoid[Expansions] =
    new Monoid[Expansions] {
      override def zero = empty
      override def append(a: Expansions, _b: => Expansions) = {
        val b = _b
        Expansions(
          a.implications |+| b.implications,
          a.reqCodes     |+| b.reqCodes,
          a.reqCodeTree  |+| b.reqCodeTree,
          a.cfImps       |+| b.cfImps,
          a.cfTags       |+| b.cfTags,
          a.otherTags    |+| b.otherTags,
          a.allTags      |+| b.allTags,
        )
      }
    }
}
