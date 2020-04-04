package shipreq.webapp.client.project.app.pages.content.reqtable

import monocle.macros.Lenses
import scalaz.{Equal, Monoid, Semigroup}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util._
import shipreq.base.util.univeq._
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
final case class Expansions(implications: Direction.Values[Vector[Pubid]],
                            reqCodes    : Vector[ReqCode.Value],
                            reqCodeTree : Vector[ReqCodeTreeItem],
                            cfImps      : Map[CustomField.Implication.Id, Vector[Pubid]],
                            cfTags      : Map[CustomField.Tag.Id, Vector[ApplicableTagId]],
                            otherTags   : Vector[ApplicableTagId],
                            allTags     : Vector[ApplicableTagId],
                          ) {

  // Workaround for stupid https://issues.scala-lang.org/browse/SI-6391
  def copyReqCodes   (nv: Vector[ReqCode.Value]  ): Expansions = copy(reqCodes = nv)
  def copyReqCodeTree(nv: Vector[ReqCodeTreeItem]): Expansions = copy(reqCodeTree = nv)

  def impsForCF(id: CustomField.Implication.Id): Vector[Pubid] =
    cfImps.getOrElse(id, Vector.empty)

  def tagsForCF(id: CustomField.Tag.Id): Vector[ApplicableTagId] =
    cfTags.getOrElse(id, Vector.empty)
}

object Expansions {
  val empty: Expansions =
    apply(
      Direction.Values both Vector.empty,
      Vector.empty,
      Vector.empty,
      UnivEq.emptyMap,
      UnivEq.emptyMap,
      Vector.empty,
      Vector.empty,
    )

  implicit def equality: UnivEq[Expansions] = UnivEq.derive

  implicit val reqCodeTreeM: Monoid[Vector[ReqCodeTreeItem]] =
    new Monoid[Vector[ReqCodeTreeItem]] {
      override def zero =  Vector.empty
      override def append(f1: Vector[ReqCodeTreeItem], f2: => Vector[ReqCodeTreeItem]) = f1 ++ f2
    }

  implicit def vectorUniqSemigroup[A](implicit e: Equal[A]): Semigroup[Vector[A]] =
    new Semigroup[Vector[A]] {
      override def append(x: Vector[A], y: => Vector[A]) =
        y.foldLeft(x)((q, a) =>
          if (x.exists(e.equal(_, a))) q else q :+ a)
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
