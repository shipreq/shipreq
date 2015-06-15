package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya.test._
import scalaz.std.vector._
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.client.lib.{ShowDead, FilterDead}

object RandomReqTableData {

  lazy val filterDead: Gen[FilterDead] =
    Gen.boolean.map(ShowDead.to)

  def visibleColumns(p: Project, fd: FilterDead): Gen[NonEmptyVector[Column]] = {
    val customFields = fd(p.fields.data.customFields.values)(_.live)
    val allPossibleColumns    = Column all customFields whole
    val (mandatory, optional) = allPossibleColumns partition Column.mandatory
    Gen.subset(optional).map(_ ++ mandatory).shuffle.map(cs => NonEmptyVector(cs.head, cs.tail))
  }

  def sortMethodI: Gen[SortMethod.IgnoreBlanks] =
    RandomData.oneofV(SortMethod.ignoreBlanks)

  def sortMethodB: Gen[SortMethod.ConsiderBlanks] =
    RandomData.oneofV(SortMethod.considerBlanks)

  def sortCriteriaC: Gen[SortCriterion.Conclusive] =
    sortMethodI.map(SortCriterion.Conclusive(Column.Pubid, _))

  private def `change ↖sortCriteriaC↖ if more conclusive criteria added`: Column.SortConclusive => Unit = {
    case Column.Pubid => ()
  }

  def sortCriteriaI(legalCols: Vector[Column.SortInconclusive]): Gen[Vector[SortCriterion.Inconclusive]] =
    Gen.subset(legalCols).shuffle.flatMap(cs =>
      Gen.sequence(cs.map(c =>
        RandomData.oneofV(SortCriterion possibilitiesI c))))

  def sortCriteria(gi: Gen[Vector[SortCriterion.Inconclusive]]): Gen[SortCriteria] =
    Gen.apply2(SortCriteria.apply)(gi, sortCriteriaC)

  val noFilter: Gen[Option[FilterAst]] =
    Gen insert None

  def viewSettings(p: Project, allowFilter: Boolean): Gen[ViewSettings] =
    for {
      fd     ← filterDead
      cols   ← visibleColumns(p, fd)
      icols  = cols.whole.filterT[Column.SortInconclusive].toVector
      order  ← sortCriteria(sortCriteriaI(icols))
      filter ← if (allowFilter) RandomData.filter.ast.forProject(p).option else noFilter
    } yield ViewSettings(cols, order, filter, fd)

}
