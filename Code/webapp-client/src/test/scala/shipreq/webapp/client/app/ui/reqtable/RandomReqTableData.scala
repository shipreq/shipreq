package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya.test._
import scalaz.std.vector._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.client.lib.{ShowDead, FilterDead}
import shipreq.webapp.client.util.On

object RandomReqTableData {

  lazy val filterDead: Gen[FilterDead] =
    Gen.boolean.map(ShowDead.to)

  def columnState(p: Project): Gen[ColumnsEditor.State] =
    for {
      long ← Gen.long
      all  = Column allInProject p
      cols ← Gen shuffle all.whole
    } yield {
      var i = long
      ColumnsEditor.State.init(cols)(c =>
        if (Column mandatory c)
          On
        else {
          val j = i
          i = i >>> 1
          if (i == 0) i = System.currentTimeMillis()
          On <~ ((j & 1) == 1)
        }
      )
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
      cs     ← columnState(p)
      icols  = cs.on.filterT[Column.SortInconclusive].toVector
      order  ← sortCriteria(sortCriteriaI(icols))
      filter ← if (allowFilter) RandomData.filter.ast.forProject(p).option else noFilter
      fd     ← filterDead
    } yield ViewSettings(cs, order, filter, fd)

}
