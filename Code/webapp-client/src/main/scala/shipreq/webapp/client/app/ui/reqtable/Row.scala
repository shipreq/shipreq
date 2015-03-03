package shipreq.webapp.client.app.ui.reqtable

import monocle._
import monocle.macros.Lenser
import scalaz.Maybe
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
case class Expansion(implicationSrc: List[Pubid],
                     implicationTgt: List[Pubid],
                     reqCodes      : List[ReqCode])
object Expansion {
  implicit val equality: UnivEq[Expansion] = deriveUnivEq

  private[this] def l = Lenser[Expansion]
  val _reqCodes       = l(_.reqCodes)
  val _implicationSrc = l(_.implicationSrc)
  val _implicationTgt = l(_.implicationTgt)

  val none = Expansion(Nil, Nil, Nil)
}

// =====================================================================================================================

case class MultiValues(tags  : List[ApplicableTag.Id],
                       cfTags: Map[CustomField.Tag.Id,         List[ApplicableTag.Id]],
                       cfImps: Map[CustomField.Implication.Id, List[Pubid]])
object MultiValues {
  implicit val equality: UnivEq[MultiValues] = deriveUnivEq

  private[this] def l = Lenser[MultiValues]
  val _tags           = l(_.tags)
  val _cfTags         = l(_.cfTags)
  val _cfImps         = l(_.cfImps)
}

// =====================================================================================================================

sealed trait Row

case class GenericReqRow(req: GenericReq, exp: Expansion, mv: MultiValues) extends Row

//case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row

object Row {
  implicit val equalityG: UnivEq[GenericReqRow]   = deriveUnivEq
//  implicit val equalityC: UnivEq[ReqCodeGroupRow] = deriveUnivEq
  implicit val equality : UnivEq[Row]             = deriveUnivEq

  val desc: Row => String = {
    case r: GenericReqRow => r.req.desc
//    case ReqCodeGroupRow(g, _) => g.desc
  }

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
  val _reqCodes       = Row._expansion   ^|-> Expansion._reqCodes
  val _implicationSrc = Row._expansion   ^|-> Expansion._implicationSrc
  val _implicationTgt = Row._expansion   ^|-> Expansion._implicationTgt
  val _tags           = Row._multiValues ^|-> MultiValues._tags
  val _cfTags         = Row._multiValues ^|-> MultiValues._cfTags
  val _cfImps         = Row._multiValues ^|-> MultiValues._cfImps

}