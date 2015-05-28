package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya.test._
import scalaz.std.vector._
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.client.lib.{ShowDead, FilterDead}

object RandomReqTableData {

  lazy val filterDead: Gen[FilterDead] =
    Gen.boolean.map(ShowDead.to)

  def columns(p: Project): Gen[NonEmptyVector[Column]] = {
    val allPossibleColumns    = Column.all(p.fields.data.customFields.values).whole
    val (mandatory, optional) = allPossibleColumns partition Column.mandatory
    Gen.subset(optional).map(_ ++ mandatory).shuffle.map(cs => NonEmptyVector(cs.head, cs.tail))
  }

  def sortMethodI: Gen[SortMethod.IgnoreBlanks] =
    RandomData.oneofV(SortMethod.ignoreBlanks)

  def sortMethodB: Gen[SortMethod.ConsiderBlanks] =
    RandomData.oneofV(SortMethod.considerBlanks)

  def sortCriteriaC: Gen[SortCriterion.Conclusive] =
    sortMethodI.map(SortCriterion.Conclusive(Column.Pubid, _))

  private def __change_rndSortCriteriaC_if_more_conclusive_criteria_added: Column.SortConclusive => Unit = {
    case Column.Pubid => ()
  }

  def sortCriteriaI(legalCols: Vector[Column.SortInconclusive]): Gen[Vector[SortCriterion.Inconclusive]] =
    Gen.subset(legalCols).shuffle.flatMap(cs =>
      Gen.sequence(cs.map(c =>
        RandomData.oneofV(SortCriterion possibilitiesI c))))

  def sortCriteria(gi: Gen[Vector[SortCriterion.Inconclusive]]): Gen[SortCriteria] =
    Gen.apply2(SortCriteria.apply)(gi, sortCriteriaC)

  def viewSettings(p: Project): Gen[ViewSettings] =
    for {
      cols  ← columns(p)
      icols = cols.whole.filterT[Column.SortInconclusive].toVector
      order ← sortCriteria(sortCriteriaI(icols))
      fdead ← filterDead
    } yield ViewSettings(cols, order, fdead)

}
