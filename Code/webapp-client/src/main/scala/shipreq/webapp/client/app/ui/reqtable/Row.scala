package shipreq.webapp.client.app.ui.reqtable

import shipreq.webapp.base.data._

/**
 * Requirements aren't always rendered in their entirety.
 * The same requirement may appear multiple times in the table with some of its values replaces by values in this class.
 *
 * For example, if a row is implied by two sources and the table is sorted by impliciation-source, then the row will
 * appear twice - once for each implied row. This process is dubbed expansion and this class houses the different
 * behaviour each row will exhibit.
 */
case class Expansion(implicationSrc: Option[Req.Id],
                     implicationTgt: Option[Req.Id],
                     reqCodes      : List[ReqCode])
object Expansion {
  val none = Expansion(None, None, Nil)
}

// =====================================================================================================================

sealed trait Row

case class GenericReqRow(req: GenericReq, exp: Expansion) extends Row

case class ReqCodeGroupRow(grp: ReqCodeGroup, code: ReqCode) extends Row
