package shipreq.webapp.client.app.ui.reqtable

import shipreq.webapp.base.data._
import SCRATCH._

object Logic {

  type ExpandedCodes = Stream[List[ReqCode]]

  def expandReqCodes(expand: Boolean): Set[ReqCode] => ExpandedCodes = {
    val noReqCodesToExpand: ExpandedCodes = Stream(Nil)

    val expandFn: Set[ReqCode] => ExpandedCodes =
      if (expand)
        _.toStream.map(_ :: Nil)
      else
        codes => Stream(codes.toList) // TODO sort?

    codes => if (codes.isEmpty) noReqCodesToExpand else expandFn(codes)
  }

  def gatherReqs(vs: ViewSettings, p: Project): Stream[Row] = {
    // Init
    val codeExpander = expandReqCodes(vs.order.includesCode)

    // Traverse reqs
    p.reqs.vstreamf {
      case r: GenericReq =>

        // Remove deleted

        // Expansion
        val expandedReqCodes = codeExpander(p.reqCodesPerTarget(r.id))

        // Done
        expandedReqCodes.map(codes =>
          GenericReqRow(r, Expansion(None, None, codes)))
    }
  }

}
