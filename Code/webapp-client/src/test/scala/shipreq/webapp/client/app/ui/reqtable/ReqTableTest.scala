package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya.test._
import scalaz.std.vector._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._

object ReqTableTest {

  def rndColumns(p: Project): Gen[Vector[Column]] = {
    val allPossibleColumns    = Column.all(p.fields.data.customFields.keys)
    val (mandatory, optional) = allPossibleColumns partition Column.mandatory
    Gen.subset(optional).map(_ ++ mandatory).shuffle
  }

  def rndSortMethodI: Gen[SortMethod.IgnoreBlanks] =
    Gen.oneofL(SortMethod.ignoreBlanks)

  def rndSortMethodB: Gen[SortMethod.ConsiderBlanks] =
    Gen.oneofL(SortMethod.considerBlanks)

  def rndSortCriteriaC: Gen[SortCriterion.Conclusive] =
    rndSortMethodI.map(SortCriterion.Conclusive(Column.PubId, _))

  private def __change_rndSortCriteriaC_if_more_conclusive_criteria_added: Column.SortConclusive => Unit = {
    case Column.PubId => ()
  }

  def rndSortCriteriaI(legalCols: Vector[Column.SortInconclusive]): Gen[Vector[SortCriterion.Inconclusive]] =
    Gen.subset(legalCols).shuffle.flatMap(cs =>
      Gen.sequence(cs.map(c =>
        Gen.oneofL(SortMethod valuesAllowed c).map(m =>
          SortCriterion.Inconclusive(c, m)))))

  def rndSortCriteria(gi: Gen[Vector[SortCriterion.Inconclusive]]): Gen[SortCriteria] =
    Gen.apply2(SortCriteria.apply)(gi, rndSortCriteriaC)

  def rndViewSettings(p: Project): Gen[ViewSettings] =
    for {
      cols  ← rndColumns(p)
      icols = cols.filterT[Column.SortInconclusive].toVector
      order ← rndSortCriteria(rndSortCriteriaI(icols))
    } yield ViewSettings(cols, order)

}
