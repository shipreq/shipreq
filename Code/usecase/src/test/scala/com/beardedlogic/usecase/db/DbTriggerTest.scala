package com.beardedlogic.usecase.db

import com.beardedlogic.usecase.test.TestDatabaseSupport
import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import org.postgresql.util.PSQLException

class DbTriggerTest extends FunSpec with TestDatabaseSupport {

  class SampleFKs {
    val txtFieldTypeId = sql"INSERT INTO field_key_type VALUES(3250,'txt',1) RETURNING id".as[Short].first
    val stepFieldTypeId = sql"INSERT INTO field_key_type VALUES(3251,'stp',NULL) RETURNING id".as[Short].first
    val txtField1 = sql"INSERT INTO field_key(type_id,data) VALUES($txtFieldTypeId,'TF1') RETURNING id".as[Long].first
    val txtField2 = sql"INSERT INTO field_key(type_id,data) VALUES($txtFieldTypeId,'TF1') RETURNING id".as[Long].first
    val ncId = sql"INSERT INTO field_key(type_id,data) VALUES($stepFieldTypeId,'NC') RETURNING id".as[Long].first
    val ecId = sql"INSERT INTO field_key(type_id,data) VALUES($stepFieldTypeId,'EC') RETURNING id".as[Long].first
  }

  case class SampleUC(fks: SampleFKs) {

    import fks._

    val uc = sql"INSERT INTO usecase DEFAULT VALUES RETURNING id".as[Long].first
    def insertText(fkId: Long) = SampleText(this, fkId)
    val txt1 = insertText(txtField1)
    val txt2 = insertText(txtField2)
    val ncStep1, ncStep2, ncStep3, ncStep4 = insertText(ncId)
    val ecStep1 = insertText(ecId)
    def latestRevId = sql"SELECT latest_rev_id FROM usecase WHERE id = $uc".as[Long].first
    def insertRev(rev: Int, number: Short, title: String): Long =
      sql"INSERT INTO usecase_rev(ident_id,rev,number,title) VALUES($uc,$rev,$number,$title) RETURNING id".as[Long].first
  }

  case class SampleText(uc: SampleUC, fkId: Long) {
    def ucId = uc.uc
    val id = sql"INSERT INTO text(uc_id,fk_id) VALUES($ucId,$fkId) RETURNING id".as[Long].first
    def insertRev(rev: Int, text: String): Long =
      sql"INSERT INTO text_rev(ident_id,rev,text) VALUES($id,$rev,$text) RETURNING id".as[Long].first
  }

  it("Only 1 value per-UC per-text-field allowed") {
    // TODO remove manual field_key stuff
    val fk = new SampleFKs
    val smp = new SampleUC(fk)
    new SampleUC(fk) // Ensure other UCs = no prob

    // UC already has a value for text-field #1
    intercept[PSQLException] {smp.insertText(fk.txtField1)}
  }

  it("usecase.latest_rev_id") {
    val TMP = -1
    val fk = new SampleFKs
    val smp = new SampleUC(fk)
    val smp2 = new SampleUC(fk)

    // INSERT
    smp.latestRevId should be(TMP)
    val rev2 = smp.insertRev(2, 9, "ah")
    smp.latestRevId should be(rev2)
    val rev1 = smp.insertRev(1, 9, "ah")
    smp.latestRevId should be(rev2)
    val rev3 = smp.insertRev(3, 9, "ah")
    smp.latestRevId should be(rev3)

    // UC-SCOPE
    smp2.latestRevId should be(TMP)
    val revB1 = smp2.insertRev(2, 8, "qwe")
    smp2.latestRevId should be(revB1)

    // UPDATE rev
    sqlu"UPDATE usecase_rev set rev=rev+5000 WHERE ident_id = ${smp.uc} AND id = $rev2".execute
    smp.latestRevId should be(rev2)
    sqlu"UPDATE usecase_rev set rev=rev-5000 WHERE ident_id = ${smp.uc} AND id = $rev2".execute
    smp.latestRevId should be(rev3)

    // UPDATE ident_id
    sqlu"UPDATE usecase_rev set ident_id = ${smp2.uc} WHERE ident_id = ${smp.uc} AND id = $rev3".execute
    smp.latestRevId should be(rev2)
    smp2.latestRevId should be(rev3)

    // DELETE
    sqlu"DELETE FROM usecase_rev where id = $rev3".execute
    smp2.latestRevId should be(revB1)
    sqlu"DELETE FROM usecase_rev where id = $revB1".execute
    smp2.latestRevId should be(TMP)

    // UPDATE ident_id of latest rev
    val revB2 = smp2.insertRev(8, 8, "qwe")
    smp2.latestRevId should be(revB2)
    sqlu"UPDATE usecase_rev set ident_id = ${smp.uc} WHERE ident_id = ${smp2.uc} AND id = $revB2".execute
    smp2.latestRevId should be(TMP)
  }

  def linkText(ucRevId: Long, txtRevId: Long) =
    sqlu"INSERT INTO uc_field(uc_rev_id,text_rev_id) VALUES($ucRevId, $txtRevId)".execute

  def linkStep(ucRevId: Long, index: Int, txtRevId: Long) =
    sqlu"INSERT INTO uc_field VALUES($ucRevId, ${s"R.$index.$txtRevId"}, NULL, $index, $txtRevId)".execute

  def linkStep(ucRevId: Long, index: Int, txtRevId: Long, parent: Long) =
    sqlu"INSERT INTO uc_field VALUES($ucRevId, ${s"$parent.$index.$txtRevId"}, $parent, $index, $txtRevId)".execute

  describe(Tables.UcField.name) {
    class Data(fk: SampleFKs) {
      val uc = new SampleUC(fk)

      val ucr = uc.insertRev(1, 1, "My UC")
      val t1r = uc.txt1.insertRev(1, "text 1")
      val t2r = uc.txt2.insertRev(1, "text 2")
      linkText(ucr, t1r)

      val s1 = uc.ncStep1.insertRev(1, "step 1")
      val s2 = uc.ncStep2.insertRev(1, "step 2")
      val s3 = uc.ncStep3.insertRev(1, "step 3")
      val s4 = uc.ncStep4.insertRev(1, "step 4")
      val e1 = uc.ecStep1.insertRev(1, "EC 1")
      linkStep(ucr, 0, s1)
      linkStep(ucr, 1, s2)
      linkStep(ucr, 0, s3, s1)
    }

    def testErr(fn: (Data, Data) => Any): Unit = withNewTransaction {
      val fk = new SampleFKs
      val d1, d2 = new Data(fk)
      intercept[PSQLException] {fn(d1, d2)}
    }

    it("UC must match Text UC") {
      testErr((d1, d2) => linkText(d1.ucr, d2.t2r))
      testErr((d1, d2) => linkStep(d1.ucr, 2, d2.s1))
    }

    it("Parent-text UC must match that of Text") {
      testErr((d1, d2) => linkStep(d1.ucr, 0, d1.s4, d2.s1))
    }

    it("Parent-text FK must match that of Text") {
      testErr((d1, d2) => linkStep(d1.ucr, 0, d1.s4, d1.e1)) // EC(e1) doesn't match NC(s4)
    }

    it("Text cannot be used as a step") {
      testErr((d1, d2) => linkStep(d1.ucr, 0, d1.t2r))
    }

    it("Steps cannot be used as text") {
      testErr((d1, d2) => linkText(d1.ucr, d1.s4))
    }
  }
}
