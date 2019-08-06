package shipreq.webapp.client.project.app.issues

import monocle.Optional
import shipreq.base.util.{Util, Vector1}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.sort.{Sorter => SorterBase}

object Sorter {
  import SorterBase._

  val Types = new WithTypes[Setup, Row]
  import Types._

  final class Setup(val p: Project)

  private val pubidSorter = sorter[(Int, Int)](
    prep =
      setup => {
        val n = setup.p.dataLogic.pubidSortKeyFn
        val `n/a` = (-1, -1)
        ;{
          case r: Row.ForReq         => n(r.req.pubid)
          case _: Row.ForRcg
             | _: Row.ForConfig
             | _: Row.ForManualIssue => `n/a`
        }
      },
    sort = SortFn.intPair
  )

  private val reqCodeIdSorter: Sorter = {
    val o = Optional[Row, Vector[ReqCode.Value]]({
      case r: Row.ForRcg         => Some(Vector1(r.code))
      case _: Row.ForReq
         | _: Row.ForConfig
         | _: Row.ForManualIssue => None
    })(c => {
      case r: Row.ForRcg if c.length == 1 => r.copy(code = c.head)
      case r                              => r
    })
    SorterBase.reqCodeSorter(o, BlanksFirst)
  }

  val idSorter: Sorter =
    pubidSorter.overrideWith(reqCodeIdSorter) {
      case _: Row.ForRcg         => true
      case _: Row.ForReq
         | _: Row.ForConfig
         | _: Row.ForManualIssue => false
    }

  val issueCategorySorter: Sorter = {
    val ordering = Util.enumOrdering(IssueCategory.values.whole)(UiText.Issues.category)
    val sortFn = SortFn.fromOrdering(ordering)
    sorter(_ => _.issue.category, sortFn)
  }

  val issueClassSorter: Sorter =
    sorter(_ => _.issueClassDesc, SortFn.stringNonEmpty)
}
