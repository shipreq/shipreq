package shipreq.webapp.client.app.ui.reqtable

import shipreq.webapp.base.data._

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
case class Expansion(implicationSrc: List[Req.Id],
                     implicationTgt: List[Req.Id],
                     reqCodes      : List[ReqCode])
object Expansion {
  val none = Expansion(Nil, Nil, Nil)
}

// =====================================================================================================================

sealed trait Row

case class GenericReqRow(req: GenericReq, exp: Expansion) extends Row

case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row
