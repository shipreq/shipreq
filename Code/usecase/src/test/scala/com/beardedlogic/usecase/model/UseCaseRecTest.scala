package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import test.TestDatabaseSupport
import lib.{Defaults, UseCaseHeader}
import lib.db.DbOpResult
import lib.Types._
import DbOpResult._

class UseCaseRevTest extends FunSpec with TestDatabaseSupport {

  describe("findUseCase") {
    it("should load when found") {
      val uch = UseCaseHeader(17, "ah")
      val saved = db.createInitialUseCase(uch)
      db.findUseCase(saved).get ==== saved
    }
  }

  describe("updateUseCaseHeader") {
    def assertUC(revId: UseCaseRevId, expected: UseCaseRev, revOffset: Int) {
      val uc = db.findUseCase(revId).get
      uc.identId ==== expected.identId
      uc.rev.toInt ==== expected.rev + revOffset
      uc.header ==== expected.header
    }

    def assertAuditedUpdate(src: UseCaseRev, relationRows: Int = 0): UseCaseRev = {
      import Tables._
      assertTableDiffs(UsecaseRev -> 1, UcField -> relationRows) {
        val r = db.updateUseCaseHeader(src, _.copy(title = "omg"))
        r.successCodeOpt ==== Some(NewRevision)
        val newUc = r.dataOpt.get
        assertUC(newUc, src.withTitle("omg"), 1)
        newUc
      }
    }

    def assertNOP(uc: UseCaseRev, expected: UseCaseRev) {
      val r = assertTableDiffs() {db.updateUseCaseHeader(uc, h => h)}
      r should be(Success(AlreadyUpToDate, expected))
    }

    def createTwoRevs = {
      val rev1 = db.createInitialUseCase("Haha")
      val rev2s = db.updateUseCaseHeader(rev1, _.copy(title = "wow")).dataOpt.get
      val rev2 = db.findUseCase(rev2s).get
      (rev1, rev2)
    }

    it("should do a direct update when rev #1 and title default") {
      val rev1 = db.createInitialUseCase(Defaults.Title)
      val tgt = rev1.withTitle("omg")
      val r = assertTableDiffs() {db.updateUseCaseHeader(rev1, _ => tgt.header)}
      r ==== Success(DirectUpdate, tgt)
      assertUC(r.dataOpt.get, tgt, 0)
    }

    it("should do an audited update when rev #1 and non-default title changes") {
      assertAuditedUpdate(db.createInitialUseCase("Haha"))
    }

    it("should do an audited update when rev #2+") {
      val (_, rev2) = createTwoRevs
      assertAuditedUpdate(rev2)
    }

    it("should copy relationships when performing an audited update") {
      val fk1 = db.createFieldKey(FieldKeyType.Text, Some("THE OCEAN"))
      val fk2 = db.createFieldKey(FieldKeyType.Text, Some("PELAGIAL"))
      val uc1 = db.createInitialUseCase("Haha")
      val txt1 = db.createTextRev(db.createInitialText(uc1, fk1), 1, "mesopelagic".hasNormalisedRefs)
      val txt2 = db.createTextRev(db.createInitialText(uc1, fk2), 1, "bathyalpelagic".hasNormalisedRefs)
      db.linkUcToText(uc1, txt1)
      db.linkUcToText(uc1, txt2)

      val uc2 = assertAuditedUpdate(uc1, 2)
      db.findAllUcFieldData(uc2).map(_.textRev).map(_.toString).sorted ==== List(txt1, txt2).map(_.toString).sorted
    }

    it("should do nothing when rev #1 and no change") {
      val rev1 = db.createInitialUseCase(Defaults.Title)
      assertNOP(rev1, rev1)
      assertNOP(rev1.withTitle(""), rev1)
    }

    it("should do nothing when rev #2 and no change") {
      val (_, rev2) = createTwoRevs
      assertNOP(rev2, rev2)
      assertNOP(rev2.withTitle(rev2.header.title + "  "), rev2)
    }

    // it("should stop when target UC is not the latest revision available") {
    //   val (rev1, _) = createTwoRevs
    //   val r = assertTableDiffs() {db.updateUseCaseHeader(rev1, _.copy(title = "aahh"))}
    //   r ==== StaleRevision
    // }
  }
}