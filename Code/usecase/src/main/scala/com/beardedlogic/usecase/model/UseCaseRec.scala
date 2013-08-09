package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib._
import db.DBHelpers._
import db.DbOpResult
import DbOpResult._
import ExternalId.{toExternal, toInternal}
import Types._

case class UseCaseRev(identId: UseCaseIdentId, rev: Short, id: UseCaseRevId, header: UseCaseHeader)

// These fields names need to match the attributes in list.html
// TODO Update UseCaseSummary field names & list.html
case class UseCaseSummary(
  dataEid: String,
  valueEid: String,
  number: Short,
  title: String,
  updatedAt: String) {

  def this(dataId: Long, valueId: Long, number: Short, title: String, updatedAt: String) =
    this(toExternal(dataId), toExternal(valueId), number, title, updatedAt)

  def this(uc: UseCaseRev, updatedAt: String) =
    this(uc.identId, uc.id, uc.header.number, uc.header.title, updatedAt)

  def dataId = toInternal(dataEid)
  def valueId = toInternal(valueEid)
}

// ---------------------------------------------------------------------------------------------------------------------

object UseCaseAccessor {

  implicit val GRUseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<, r.<<)))

  implicit val GRUseCaseSummary = GetResult(r =>
    new UseCaseSummary(r.nextLong, r.nextLong, r.nextShort, r.nextString, r.nextString))

  val CopyUcFieldsBetweenRevs = Q.update[(UseCaseRevId, UseCaseRevId)]( s"""
    insert into uc_field
    select ?, label, parent_rev_id, index, text_rev_id
    from uc_field where uc_rev_id = ?
  """.sql)

  val InsertIdent = Q.queryNA[UseCaseIdentId]("INSERT INTO usecase DEFAULT VALUES RETURNING id")

  val InsertRev = Q.query[(UseCaseIdentId, Short, Short, String), UseCaseRevId](
    "INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,?,?,?) RETURNING id")

  private val NextNumberSql = "select coalesce(max(number),0)+1 from usecase_rev where id in (select latest_rev_id from usecase)"

  val InsertRevWithNextNumber = Q.query[(UseCaseIdentId, Short, String), (UseCaseRevId, Short)](
    s"INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,?,($NextNumberSql),?) RETURNING id, number")

  private val RevFields = s"ident_id, rev, id, title, number"

  val SelectLatestRevId = Q.query[UseCaseIdentId, UseCaseRevId](s"SELECT latest_rev_id FROM usecase WHERE id=?")

  val SelectRevById = Q.query[UseCaseRevId, UseCaseRev](s"SELECT $RevFields FROM usecase_rev WHERE id=?")

  val SelectLatestRev = Q.query[UseCaseIdentId, UseCaseRev](
    s"SELECT $RevFields FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

  val SelectSummaries = Q.queryNA[UseCaseSummary]( s"""
    select ident_id, r.id, number, title, to_iso8601_str(created_at)
    from usecase u, usecase_rev r
    where r.id = latest_rev_id
    order by number """.sql)

  val UpdateTitleDirectly = Q.update[(String, UseCaseRevId)]("UPDATE usecase_rev SET title=? WHERE id=?")
}

trait UseCaseAccessor extends DatabaseAccessor {
  self: DAO =>

  import UseCaseAccessor._

  /** Creates a single `usecase_rev` row. Doesn't create a new `usecase`. */
  def createUseCase(ucId: UseCaseIdentId, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val uch = InputCorrection.correct(header)
    createUseCaseRevWithoutCorrection(ucId, rev, uch)
  }

  /** Creates a single `usecase_rev` row. Doesn't create a new `usecase`. */
  private def createUseCaseRevWithoutCorrection(ucId: UseCaseIdentId, rev: Short, h: UseCaseHeader): UseCaseRev = {
    val id = InsertRev.first(ucId, rev, h.number, h.title)
    UseCaseRev(ucId, rev, id, h)
  }

  /**
   * Creates a `usecase` and a rev-#1 `usecase_rev`. The UC number is determined automatically.
   *
   * @param title The new UC title.
   */
  def createInitialUseCase(title: String): UseCaseRev = withTransaction {
    // TODO New-UC has GLOBAL scope.
    // TODO New-UC: Use table locking for mutex?
    // TODO New-UC: Lacking appropriate number uniqueness constraint
    // TODO need a usecase state so we can call correct() instead of correctUseCaseTitle(). Would also make stateEquals() redundant
    val identId = InsertIdent.first()
    val correctedTitle = InputCorrection.useCaseTitle(title)
    val rev = 1: Short
    val (id, number) = InsertRevWithNextNumber.first(identId, rev, correctedTitle)
    UseCaseRev(identId, rev, id, UseCaseHeader(correctedTitle, number))
  }

  def findLatestUseCaseRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestRevId.firstOption(ucId)

  def findUseCase(revId: UseCaseRevId): Option[UseCaseRev] = SelectRevById.firstOption(revId)

  def findLatestUseCase(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestRev.firstOption(ucId)

  def findAllUseCaseSummaries(): List[UseCaseSummary] = SelectSummaries.list

  /**
   * Updates the header of an existing use case (ie. just the contents of the `usecase` table ignoring its relations).
   *
   * When updating just the title of an Untitled rev #1 UC, the update is direct. In all other cases requiring an
   * update, a new revision is created.
   *
   * @param ucId The `usecase` id to update. (Note: not the `usecase_rev` id.)
   * @return A result indicator, and a resulting `UseCaseRev` if successful. Possible results are:
   *         AlreadyUpToDate, DirectUpdate, NewRevision, NothingUpdated.
   */
  def updateUseCaseHeader(ucId: UseCaseIdentId, modFn: UseCaseHeader => UseCaseHeader): DbOpResult[UseCaseRev] = withTransaction {
    // TODO locking? race conditions here? ensure DB mutex

    findLatestUseCase(ucId) match {
      case None => NothingUpdated
      case Some(latest) =>
        val newHeader = InputCorrection.correct(modFn(latest.header))

        // NOP
        if (latest.header == newHeader)
          Success(AlreadyUpToDate, latest)

        // Rev #1 title update
        else if (latest.rev == 1 && latest.header == newHeader.copy(title = Defaults.Title)) {
          UpdateTitleDirectly.execute(newHeader.title, latest.id)
          Success(DirectUpdate, latest.copy(header = newHeader))
        }

        // Audited update
        else {
          val newRev = createUseCaseRevWithoutCorrection(ucId, latest.rev + 1, newHeader)
          copyUcFieldsBetweenRevs(latest, newRev)
          Success(NewRevision, newRev)
        }
    }
  }

  def copyUcFieldsBetweenRevs(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs.execute(to, from)
}
