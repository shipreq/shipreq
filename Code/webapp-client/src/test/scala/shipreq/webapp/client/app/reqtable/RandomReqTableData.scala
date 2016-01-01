package shipreq.webapp.client.app.reqtable

import nyaya.gen._
import nyaya.test._
import scalaz.std.vector._
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.client.data.{ShowDead, FilterDead}
import RandomData.NevToNonEmptySeq

object RandomReqTableData {

  lazy val filterDead: Gen[FilterDead] =
    Gen.boolean.map(ShowDead.to)

  def visibleColumns(p: Project): Gen[NonEmptyVector[Column]] =
    for {
      long ← Gen.long
      all  = Column all p.config
      cols ← Gen shuffle all.whole
    } yield {
      var i = long
      val vs = cols.filter(c =>
        if (Column isMandatory c)
          true
        else {
          val j = i
          i = i >>> 1
          if (i == 0) i = System.currentTimeMillis()
          (j & 1) == 1
        }
      )
      NonEmptyVector force vs
  }

  def sortMethodI: Gen[SortMethod.IgnoreBlanks] =
    Gen.chooseNE(SortMethod.ignoreBlanks)

  def sortMethodB: Gen[SortMethod.ConsiderBlanks] =
    Gen.chooseNE(SortMethod.considerBlanks)

  def columnC: Gen[Column.SortConclusive] =
    Gen pure Column.Pubid

  private def `change ↖columnCon↖ if more conclusive criteria added`: Column.SortConclusive => Unit = {
    case Column.Pubid => ()
  }

  def sortCriteriaC: Gen[SortCriterion.Conclusive] =
    Gen.apply2(SortCriterion.Conclusive)(columnC, sortMethodI)

  val builtInColumnIs: NonEmptyVector[Column.SortInconclusive] =
    NonEmptyVector force (Column.builtInValues.whole: Vector[Column]).filterT[Column.SortInconclusive].toVector

  val builtInColumnIsG: Gen[Column.SortInconclusive] =
    Gen.chooseNE(builtInColumnIs)

  case class ColumnIGen(legalCustomFieldColumns: Vector[Column.CustomField]) {
    val legalCustomFieldColumnNEV = NonEmptyVector option legalCustomFieldColumns

    val legalColumnIs: NonEmptyVector[Column.SortInconclusive] =
      builtInColumnIs ++ legalCustomFieldColumns

    def columnI: Gen[Column.SortInconclusive] =
      Gen.chooseNE(legalColumnIs)

    def colIs: Gen[Vector[Column.SortInconclusive]] =
      Gen.subset(legalColumnIs.whole).shuffle

    def sortCriIs: Gen[Vector[SortCriterion.Inconclusive]] =
      colIs flatMap RandomReqTableData.sortCriIs
  }

  def customFieldColumn: Gen[Column.CustomField] =
    Gen.apply2(Column.CustomField)(RandomData.customFieldId, RandomData.live)

  def sortCriI(colI: Column.SortInconclusive): Gen[SortCriterion.Inconclusive] =
    Gen.chooseNE(SortCriterion possibilitiesI colI)

  def sortCriIs(colIs: Vector[Column.SortInconclusive]): Gen[Vector[SortCriterion.Inconclusive]] =
    Gen.sequence(colIs map sortCriI)

  def sortCriteria(scIs: Vector[SortCriterion.Inconclusive]): Gen[SortCriteria] =
    sortCriteriaC.map(SortCriteria(scIs, _))

  val noFilter: Gen[Option[FilterAst]] =
    Gen pure None

  def viewSettings(p: Project, allowFilter: Boolean): Gen[ViewSettings] =
    for {
      cs     ← visibleColumns(p)
      icols  = cs.toStream.filterT[Column.SortInconclusive].toVector
      scis   ← Gen.subset(icols).shuffle flatMap sortCriIs
      order  ← sortCriteria(scis)
      filter ← if (allowFilter) RandomData.filter.ast.forProject(p).option else noFilter
      fd     ← filterDead
    } yield ViewSettings(cs, order, filter, fd)

}
