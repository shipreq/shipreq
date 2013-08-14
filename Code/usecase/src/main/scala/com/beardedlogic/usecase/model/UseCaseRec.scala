package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
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

  def dataId: UseCaseIdentId = toInternal(dataEid).tag[UseCaseIdentIdTag]
  def valueId: UseCaseRevId = toInternal(valueEid).tag[UseCaseRevIdTag]
}

// ---------------------------------------------------------------------------------------------------------------------

object UseCaseAccessor {

  implicit val GRUseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<, r.<<)))

  implicit val GRUseCaseSummary = GetResult(r =>
    new UseCaseSummary(r.nextLong, r.nextLong, r.nextShort, r.nextString, r.nextString))

  val InsertIdent = Q.queryNA[UseCaseIdentId]("INSERT INTO usecase DEFAULT VALUES RETURNING id")

  val InsertRev = Q.query[(UseCaseIdentId, Short, Short, String), UseCaseRevId](
    "INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,?,?,?) RETURNING id")

  private val NextNumberSql = "select coalesce(max(number),0)+1 from usecase_rev where id in (select latest_rev_id from usecase)"

  val InsertInitialRevWithNextNumber = Q.query[(UseCaseIdentId, String), (UseCaseRevId, Short)](
    s"INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,1,($NextNumberSql),?) RETURNING id, number")


  val SelectLatestRevId = Q.query[UseCaseIdentId, UseCaseRevId](s"SELECT latest_rev_id FROM usecase WHERE id=?")

  private val r_* = s"r.ident_id, r.rev, r.id, r.title, r.number"

  val SelectRevById = Q.query[UseCaseRevId, UseCaseRev](s"SELECT ${r_*} FROM usecase_rev r WHERE r.id=?")

  val SelectLatestRev = Q.query[UseCaseIdentId, UseCaseRev](
    s"SELECT ${r_*} FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

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
   * Creates a new `usecase` row. If a `usecase_rev` row is not inserted before the end of the transaction, then the
   * transaction will fail because `usecase.latest_rev_id` will be `NULL`.
   */
  def createUseCaseIdent(): UseCaseIdentId = InsertIdent.first()

  // TODO Remove createInitialUseCase(header) after anonymous UC editing is removed
  def createInitialUseCase(header: UseCaseHeader): UseCaseRev = withTransaction {
    val identId = createUseCaseIdent()
    val h = InputCorrection.correct(header)
    val rev = 1: Short
    createUseCaseRevWithoutCorrection(identId, rev, h)
  }

  // TODO New-UC has GLOBAL scope.
  // TODO New-UC: Use table locking for mutex?
  // TODO New-UC: Lacking appropriate number uniqueness constraint
  // TODO need a usecase state so we can call correct() instead of correctUseCaseTitle(). Would also make stateEquals() redundant
  /**
   * Creates a `usecase` and a rev-#1 `usecase_rev`. The UC number is determined automatically.
   *
   * @param title The new UC title.
   */
  def createInitialUseCase(title: String): UseCaseRev = withTransaction {
    val identId = createUseCaseIdent()
    val correctedTitle = InputCorrection.useCaseTitle(title)
    val (id, number) = InsertInitialRevWithNextNumber.first(identId, correctedTitle)
    UseCaseRev(identId, 1, id, UseCaseHeader(correctedTitle, number))
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
}
