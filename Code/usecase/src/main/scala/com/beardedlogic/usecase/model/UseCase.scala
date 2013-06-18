package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib._
import db.DBHelpers._

case class UseCase(
  value: PlainValue[DataType.UseCase],
  title: String,
  number: Short,
  fieldListId: Long) extends Value[DataType.UseCase] {

  final def valueId = value.valueId

  def stateEquals(that: UseCase): Boolean =
    this.title == that.title &&
    this.number == that.number &&
    this.fieldListId == that.fieldListId

  def toSummary(updatedAt: String) = UseCaseSummary(valueId, number, title, updatedAt)
}

// These fields names need to match the attributes in list.html
case class UseCaseSummary(
  valueId: Long,
  number: Short,
  title: String,
  updatedAt: String
)

// ---------------------------------------------------------------------------------------------------------------------

object UseCaseAccessor {
  implicit val GetResultPlainValue = ValueAccessor.GetValueResult[DataType.UseCase]
  implicit val GetResultUseCase = GetResult(r => UseCase(GetResultPlainValue(r), r.<<, r.<<, r.<<))
  implicit val GetResultUseCaseSummary = GetResult(r => UseCaseSummary(r.<<, r.<<, r.<<, r.<<))

  implicit object SetParameterUseCase extends SetParameter[UseCase] {
    def apply(v: UseCase, pp: PositionedParameters) {
      pp.setLong(v.valueId)
      pp.setString(v.title)
      pp.setShort(v.number)
      pp.setLong(v.fieldListId)
    }
  }

  val Insert = Q.update[UseCase]("INSERT INTO usecase VALUES(?,?,?,?)")

  val NextNumber = "select coalesce(max(number),0)+1 from usecase"
  val InsertNext = Q.query[(Long, String, Long), Short](s"INSERT INTO usecase VALUES(?,?,($NextNumber),?) RETURNING number")

  private val fieldSelection = s"v.${ValueAccessor.*}, title, number, field_list_id"

  val Select = Q.query[Long, UseCase]( s"""
    SELECT $fieldSelection
    FROM value v, usecase u
    WHERE u.id=? AND v.id = u.id
    """.sql)

  private def SelectLatestSql(dataIdSql: String) = s"""
    with history as (
      select id, rev, data_id, row_number() over (order by rev desc) rn
      from value
      where data_id = $dataIdSql
    )
    select $fieldSelection
    from history v, usecase u
    where v.id = u.id
      and v.rn = 1
    """.sql

  val SelectLatestByDataId = Q.query[Long, UseCase](SelectLatestSql("?"))
  val SelectLatestByValueId = Q.query[Long, UseCase](SelectLatestSql("(select data_id from value where id = ?)"))

  private def SelectSummariesSql(innerCond: String) = {
    val latestRevs = new LatestRevSubquery().where(innerCond).withTableAlias("t")
    s"""
      ${latestRevs.toWithClause}
      select v.id, number, title, to_iso8601_str(updated_at)
      from usecase u, value v
      where ${latestRevs.applyWithTableAsValueIdFilter("u.id")}
      and u.id=v.id
      order by number
      """.sql
  }
  val SelectSummaries = Q.queryNA[UseCaseSummary](SelectSummariesSql(
    s"data_id in (select id from data where type_id = ${DataType.UseCase.ordinal})"))
  val SelectSummary = Q.query[Long, UseCaseSummary](SelectSummariesSql(s"data_id=?"))

  val UpdateTitleDirectly = Q.update[(String, Long)]("UPDATE usecase SET title=? WHERE id=?")

  // TODO Model correction & validation: move, rename, do something
  def normaliseWhitespaceInSingleLineString(str: String) = str.replaceAll("\\s+", " ").trim
  def correctUseCaseTitle(title: String) = {
    var t = normaliseWhitespaceInSingleLineString(title)
    if (t.isEmpty) t = Defaults.Title
    t
  }
  def correct(uc: UseCase) = uc.copy(title = correctUseCaseTitle(uc.title))
}

trait UseCaseAccessor extends DatabaseAccessor {
  self: DAO =>

  import UseCaseAccessor._

  /** Creates a single `usecase` row. Doesn't create a new `value`. */
  def createUseCase(value: PlainValue[DataType.UseCase],
    title: String,
    number: Short,
    fieldList: FieldList): UseCase = {

    val uc = correct(UseCase(value, title, number, fieldList.valueId))
    createCorrectedUseCase(uc)
    uc
  }

  /** Creates a single `usecase` row. Doesn't create a new `value`. */
  private def createCorrectedUseCase(uc: UseCase): Unit = Insert.execute(uc)

  // TODO New-UC has GLOBAL scope.
  // TODO New-UC: Use table locking for mutex?
  // TODO New-UC: Lacking appropriate number uniqueness constraint
  def createInitialUseCase(title: String, fieldList: FieldList): UseCase = withTransaction {
    // TODO need a usecase state so we can call correct() instead of correctUseCaseTitle(). Would also make stateEquals() redundant
    val correctedTitle = correctUseCaseTitle(title)
    val v = createInitialValue(DataType.UseCase)
    val number = InsertNext.first(v.valueId, correctedTitle, fieldList.valueId)
    UseCase(v, correctedTitle, number, fieldList.valueId)
  }

  def findUseCase(valueId: Long): Option[UseCase] = Select.firstOption(valueId)

  def findLatestUseCase(uc: UseCase): Option[UseCase] = findLatestUseCaseByDataId(uc.value.dataId)
  def findLatestUseCaseByDataId(dataId: Long): Option[UseCase] = SelectLatestByDataId.firstOption(dataId)
  def findLatestUseCaseByValueId(valueId: Long): Option[UseCase] = SelectLatestByValueId.firstOption(valueId)

  def findUseCaseSummary(uc: UseCase): Option[UseCaseSummary] = SelectSummary.firstOption(uc.value.dataId)
  def findAllUseCaseSummaries(): List[UseCaseSummary] = SelectSummaries.list

  /**
   * Updates the header of an existing use case (ie. just the contents of the `usecase` table ignoring its relations).
   *
   * When updating just the title of an Untitled rev #1 UC, the update is direct. In all other cases requiring an
   * update, a new revision is created.
   *
   * @param tgtUseCase An existing use case with updated values.
   * @return A result indicator, and a resulting `UseCase` if successful. Possible results are:
   *         AlreadyUpToDate, DirectUpdate, NewRevision, StaleRevision.
   */
  def updateUseCaseHeader(tgtUseCase: UseCase): (DbOpResult, Option[UseCase]) = withTransaction {
    // TODO locking? race conditions here? ensure DB mutex

    val tgt = correct(tgtUseCase)
    findLatestUseCase(tgt) match {
      // NOP
      case Some(latest) if tgt.stateEquals(latest) =>
        (DbOpResult.AlreadyUpToDate, Some(latest))

      // Rev #1 title update
      case Some(latest) if latest.value.rev == 1 && tgt.copy(title = Defaults.Title).stateEquals(latest) => {
        UpdateTitleDirectly.execute(tgt.title, tgt.valueId)
        (DbOpResult.DirectUpdate, Some(tgt))
      }

      // Audited Update (ensuring not stale)
      case Some(latest) if latest.valueId == tgt.valueId => {
        val newValue = createValue(tgt.value, LatestRev)
        val newUc = tgt.copy(value = newValue)
        createCorrectedUseCase(newUc)
        propagateRelations(latest, newUc)
        (DbOpResult.NewRevision, Some(newUc))
      }

      case _ => (DbOpResult.StaleRevision, None)
    }
  }
}
