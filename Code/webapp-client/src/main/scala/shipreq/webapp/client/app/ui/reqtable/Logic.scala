package shipreq.webapp.client.app.ui.reqtable

import scalaz.{Apply, NonEmptyList}
import shipreq.webapp.base.data._

private[reqtable] object Logic {

  private type Expanded[+A] = NonEmptyList[List[A]]

  private final val emptyExpansions: NonEmptyList[Expansion] =
    NonEmptyList(Expansion.none)

  private final val emptyExpanded: Expanded[Nothing] =
    NonEmptyList(Nil)

  @inline private def isEmptyExp[A](e: Expanded[A]): Boolean =
    e eq emptyExpanded

  /**
   * Nothing to expand = [ [] ]
   * Don't expand      = [ [a,b,c] ]
   * Expanded          = [ [a], [b], [c] ]
   *
   * @param visible Is the column visible? If not, just pretend everything is empty.
   * @param expand  When visible, does the data need to be expanded?
   */
  private def expander[A](visible: Boolean, expand: => Boolean): Set[A] => Expanded[A] = {
    type F = Set[A] => Expanded[A]

    @inline def nonEmpty(f: (A, List[A]) => Expanded[A]): F =
      _.toList match {
        case Nil    => emptyExpanded
        case h :: t => f(h, t)
      }

    def doExpand: F =
      nonEmpty((h, t) =>
        NonEmptyList.nel(h :: Nil, t.map(_ :: Nil)))

    def dontExpand: F =
      nonEmpty((h, t) => NonEmptyList(h :: t))

    if (visible) {
      if (expand) doExpand else dontExpand
    } else
      _ => emptyExpanded
  }

  private def expanderC[A](vs: ViewSettings, c: Column.SortInconclusive): Set[A] => Expanded[A] =
    expander(vs isVisible c, vs isOrdered c)

  private def expansions(impSrcs: Expanded[Req.Id], impTgts: Expanded[Req.Id], codes: Expanded[ReqCode]): NonEmptyList[Expansion] =
    if (isEmptyExp(codes) && isEmptyExp(impSrcs) && isEmptyExp(impTgts))
      emptyExpansions
    else
      Apply[NonEmptyList].apply3(impSrcs, impTgts, codes)(Expansion.apply)

  /**
   * Gathers [[Row]]s for display in [[ReqTable]].
   * Performs expansion.
   * Does not perform any sorting.
   */
  def gather(vs: ViewSettings, p: Project): Stream[Row] = {

    // NOTE:
    // * Column.ImplicationSrc isn't transitive; custom implication columns are.
    //   There can potentially be overlap but culling this could be misleading.

    // Init
    val expandImpSrcs = expanderC[Req.Id](vs, Column.ImplicationSrc)
    val expandImpTgts = expanderC[Req.Id](vs, Column.ImplicationTgt)
    val expandCodes   = expanderC[ReqCode](vs, Column.Code)
    val pReqCodes     = p.reqCodes.data
    val pImplications = p.reqFieldData.data.implications

    // Traverse reqs
    p.reqs.data.reqs.vstreamf {
      case r: GenericReq =>
        val id = r.id

        // Remove deleted

        // Filter

        // Expansion
        val impSrcs = expandImpSrcs(pImplications.tgtToSrc(id))
        val impTgts = expandImpTgts(pImplications.srcToTgt(id))
        val codes   = expandCodes(pReqCodes.byTarget(id))
        val exps    = expansions(impSrcs, impTgts, codes)

        // Done
        exps.list.toStream.map(GenericReqRow(r, _))
    }

    // Add SHRs

  }

}
