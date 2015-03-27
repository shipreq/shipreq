package shipreq.webapp.client.app.ui.reqtable

import monocle._
import monocle.macros.Lenser
import scalaz.{Equal, Semigroup, Monoid, Maybe}
import scalaz.std.map._
import scalaz.syntax.semigroup._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.TypeclassDerivation._

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
// TODO Make imp naming consistent
case class Expansion(implicationSrc: Vector[Pubid],
                     implicationTgt: Vector[Pubid],
                     reqCodes      : Vector[ReqCode],
                     cfImps        : Map[CustomField.Implication.Id, Vector[Pubid]],
                     cfTags        : Map[CustomField.Tag.Id,         Vector[ApplicableTag.Id]]) {

  def impsForCF(id: CustomField.Implication.Id): Vector[Pubid] =
    cfImps.getOrElse(id, Vector.empty)

  def tagsForCF(id: CustomField.Tag.Id): Vector[ApplicableTag.Id] =
    cfTags.getOrElse(id, Vector.empty)
}

object Expansion {
  implicit val equality: UnivEq[Expansion] = deriveUnivEq

  implicit def vectorUniqSemigroup[A: Equal]: Semigroup[Vector[A]] =
    new Semigroup[Vector[A]] {
      override def append(x: Vector[A], y: => Vector[A]) =
        y.foldLeft(x)((q, a) =>
          if (x.exists(Equal[A].equal(_, a))) q else q :+ a)
    }

  implicit val monoid: Monoid[Expansion] =
    new Monoid[Expansion] {
      override def zero = none
      override def append(a: Expansion, _b: => Expansion) = {
        val b = _b
        Expansion(
          a.implicationSrc |+| b.implicationSrc,
          a.implicationTgt |+| b.implicationTgt,
          a.reqCodes       |+| b.reqCodes,
          a.cfImps         |+| b.cfImps,
          a.cfTags         |+| b.cfTags)
      }
    }

  private[this] def l = Lenser[Expansion]
  val _implicationSrc = l(_.implicationSrc)
  val _implicationTgt = l(_.implicationTgt)
  val _reqCodes       = l(_.reqCodes)
  val _cfImps         = l(_.cfImps)
  val _cfTags         = l(_.cfTags)

  val none = Expansion(Vector.empty, Vector.empty, Vector.empty, UnivEq.emptyMap, UnivEq.emptyMap)
}

// =====================================================================================================================

/**
 * Sortable data (ie. lists) that are never expanded.
 */
case class MultiValues(tags: Vector[ApplicableTag.Id])
object MultiValues {
  implicit val equality: UnivEq[MultiValues] = deriveUnivEq

  import Expansion.vectorUniqSemigroup

  implicit val monoid: Monoid[MultiValues] =
    new Monoid[MultiValues] {
      override def zero = MultiValues(Vector.empty)
      override def append(a: MultiValues, _b: => MultiValues) = {
        val b = _b
        MultiValues(a.tags |+| b.tags)
      }
    }

  private[this] def l = Lenser[MultiValues]
  val _tags           = l(_.tags)
}

// =====================================================================================================================

sealed trait Row {
  def fold[A](g: GenericReqRow => A): A
}

case class GenericReqRow(req: GenericReq, exp: Expansion, mv: MultiValues) extends Row {
  override def fold[A](g: GenericReqRow => A): A = g(this)
  override def toString = s"\n$req\n$exp\n$mv\n"
}

//case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row

object Row {
  implicit val equalityG: UnivEq[GenericReqRow]   = deriveUnivEq
//  implicit val equalityC: UnivEq[ReqCodeGroupRow] = deriveUnivEq
  implicit val equality : UnivEq[Row]             = deriveUnivEq

  val _expansion = Optional[Row, Expansion] {
    case r: GenericReqRow => Maybe just r.exp
  }(e => {
    case GenericReqRow(r, _, m) => GenericReqRow(r, e, m)
  })

  val _multiValues = Optional[Row, MultiValues] {
    case r: GenericReqRow => Maybe just r.mv
  }(mv => {
    case GenericReqRow(r, e, _) => GenericReqRow(r, e, mv)
  })

  // TODO I am now officially fucking over the "_" prefix on optics.
  val _implicationSrc = Row._expansion   ^|-> Expansion._implicationSrc
  val _implicationTgt = Row._expansion   ^|-> Expansion._implicationTgt
  val _reqCodes       = Row._expansion   ^|-> Expansion._reqCodes
  val _cfImps         = Row._expansion   ^|-> Expansion._cfImps
  val _cfTags         = Row._expansion   ^|-> Expansion._cfTags
  val _tags           = Row._multiValues ^|-> MultiValues._tags

}
