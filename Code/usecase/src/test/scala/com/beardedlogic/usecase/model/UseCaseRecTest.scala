package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import test.{TestHelpers, TestDatabaseSupport}
import lib.{Defaults, UseCaseHeader}
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import lib.db.DbOpResult
import DbOpResult._

class UseCaseRecTest extends FunSpec with TestDatabaseSupport with TestHelpers {

  lazy val FL = Defaults.FieldList.get

  describe("findUseCase") {
    it("should load when found") {
      val value = db.createInitialValue(DataType.UseCase)
      val vid = value.valueId
      sqlu"INSERT INTO usecase VALUES(${vid}, 'ah', 7, ${FL.valueId})".execute

      val uc = db.findUseCase(vid).get
      uc should be(UseCaseRec(value, UseCaseHeader("ah", 7.toShort), Defaults.FieldList.get.valueId))
    }
  }

  describe("updateUseCaseHeader") {
    def assertUC(id: Long, expected: UseCaseRec, revOffset: Int) {
      val uc = db.findUseCase(id).get
      uc.dataId should be(expected.dataId)
      uc.value.rev should be(expected.value.rev + revOffset)
      uc.header should be(expected.header)
      uc.fieldListId should be(expected.fieldListId)
    }

    def assertAuditedUpdate(src: UseCaseRec, relationRows: Int = 0): UseCaseRec = {
      assertTableDiffs('value -> 1, 'usecase -> 1, 'relation -> relationRows) {
        val tgt = src.withTitle("omg")
        val r = db.updateUseCaseHeader(tgt)
        r.successCodeOpt should be(Some(NewRevision))
        val newUc = r.dataOpt.get
        assertUC(newUc.valueId, tgt, 1)
        newUc
      }
    }

    def assertNOP(uc: UseCaseRec, expected: UseCaseRec) {
      val r = assertTableDiffs() {db.updateUseCaseHeader(uc)}
      r should be (Success(AlreadyUpToDate, expected))
    }

    def createTwoRevs = {
      val rev1 = db.createInitialUseCase("Haha", FL)
      val rev2s = db.updateUseCaseHeader(rev1.withTitle("wow")).dataOpt.get
      val rev2 = db.findUseCase(rev2s.valueId).get
      (rev1, rev2)
    }

    it("should do a direct update when rev #1 and title default") {
      val rev1 = db.createInitialUseCase(Defaults.Title, FL)
      val tgt = rev1.withTitle("omg")
      val r = assertTableDiffs() {db.updateUseCaseHeader(tgt)}
      r should be (Success(DirectUpdate, tgt))
      assertUC(r.dataOpt.get.valueId, tgt, 0)
    }

    it("should do an audited update when rev #1 and non-default title changes") {
      assertAuditedUpdate(db.createInitialUseCase("Haha", FL))
    }

    it("should do an audited update when rev #2+") {
      val (rev1, rev2) = createTwoRevs
      assertAuditedUpdate(rev2)
    }

    it("should copy relationships when performing an audited update") {
      val rev1 = db.createInitialUseCase("Haha", FL)
      db.createRelationUnchecked(rev1, RelationType.Has, 7, FL)
      val rev2 = assertAuditedUpdate(rev1, 1)
      val r = sql"select type_id,index,to_id from relation where from_id=${rev2.valueId}".as[(Short, Short, Long)].list
      r should be(List((RelationType.Has.ordinal, 7: Short, FL.valueId)))
    }

    it("should do nothing when rev #1 and no change") {
      val rev1 = db.createInitialUseCase(Defaults.Title, FL)
      assertNOP(rev1, rev1)
      assertNOP(rev1.withTitle(""), rev1)
    }

    it("should do nothing when rev #2 and no change") {
      val (rev1, rev2) = createTwoRevs
      assertNOP(rev2, rev2)
      assertNOP(rev2.withTitle(rev2.header.title + "  "), rev2)
    }

    it("should stop when target UC is not the latest revision available") {
      val (rev1, rev2) = createTwoRevs
      val r = assertTableDiffs() {db.updateUseCaseHeader(rev1.withTitle("aahh"))}
      r should be(StaleRevision)
    }
  }
}