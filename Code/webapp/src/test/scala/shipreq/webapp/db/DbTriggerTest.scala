package shipreq.webapp.db

import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import org.postgresql.util.PSQLException
import shipreq.webapp.db.SqlHelpers.SP_ShareId
import shipreq.webapp.test.TestDatabaseSupport

class DbTriggerTest extends FunSpec with TestDatabaseSupport {

  class SampleFKs {
    val txtFieldTypeId = sql"INSERT INTO field_key_type VALUES(3250,'txt',1) RETURNING id".as[Short].first
    val stepFieldTypeId = sql"INSERT INTO field_key_type VALUES(3251,'stp',NULL) RETURNING id".as[Short].first
    val txtField1 = sql"INSERT INTO field_key(type_id,data) VALUES($txtFieldTypeId,'TF1') RETURNING id".as[Long].first
    val txtField2 = sql"INSERT INTO field_key(type_id,data) VALUES($txtFieldTypeId,'TF1') RETURNING id".as[Long].first
    val ncId = sql"INSERT INTO field_key(type_id,data) VALUES($stepFieldTypeId,'NC') RETURNING id".as[Long].first
    val ecId = sql"INSERT INTO field_key(type_id,data) VALUES($stepFieldTypeId,'EC') RETURNING id".as[Long].first
  }

  case class SampleUC(ucn: Short, fks: SampleFKs) {
    import fks._
    val projectId = newProjectId().longValue
    val ucId: Long = sql"INSERT INTO usecase(project_id,number) VALUES($projectId,$ucn) RETURNING id".as[Long].first
    def insertText(fkId: Long) = SampleText(this, fkId)
    val txt1 = insertText(txtField1)
    val txt2 = insertText(txtField2)
    val ncStep1, ncStep2, ncStep3, ncStep4 = insertText(ncId)
    val ecStep1 = insertText(ecId)
    def latestRevId = sql"SELECT latest_rev_id FROM usecase WHERE id = $ucId".as[Long].first
    def insertRev(rev: Int, title: String): Long =
      sql"INSERT INTO usecase_rev(ident_id,rev,title) VALUES($ucId,$rev,$title) RETURNING id".as[Long].first
  }

  case class SampleText(uc: SampleUC, fkId: Long) {
    def ucId = uc.ucId
    val id = sql"INSERT INTO text(uc_id,fk_id) VALUES($ucId,$fkId) RETURNING id".as[Long].first
    def insertRev(rev: Int, text: String): Long =
      sql"INSERT INTO text_rev(ident_id,rev,text) VALUES($id,$rev,$text) RETURNING id".as[Long].first
  }

  it("Only 1 value per-UC per-text-field allowed") {
    // TODO remove manual field_key stuff
    val fk = new SampleFKs
    val smp = new SampleUC(1, fk)
    new SampleUC(2, fk) // Ensure other UCs = no prob

    // UC already has a value for text-field #1
    intercept[PSQLException] {smp.insertText(fk.txtField1)}
  }

  it("usecase.latest_rev_id") {
    val TMP = -1
    val fk = new SampleFKs
    val smp = new SampleUC(9, fk)
    val smp2 = new SampleUC(8, fk)

    // INSERT
    smp.latestRevId should be(TMP)
    val rev2 = smp.insertRev(2, "ah")
    smp.latestRevId should be(rev2)
    val rev1 = smp.insertRev(1, "ah")
    smp.latestRevId should be(rev2)
    val rev3 = smp.insertRev(3, "ah")
    smp.latestRevId should be(rev3)

    // UC-SCOPE
    smp2.latestRevId should be(TMP)
    val revB1 = smp2.insertRev(2, "qwe")
    smp2.latestRevId should be(revB1)

    // UPDATE rev
    sqlu"UPDATE usecase_rev set rev=rev+5000 WHERE ident_id = ${smp.ucId} AND id = $rev2".execute
    smp.latestRevId should be(rev2)
    sqlu"UPDATE usecase_rev set rev=rev-5000 WHERE ident_id = ${smp.ucId} AND id = $rev2".execute
    smp.latestRevId should be(rev3)

    // UPDATE ident_id
    sqlu"UPDATE usecase_rev set ident_id = ${smp2.ucId} WHERE ident_id = ${smp.ucId} AND id = $rev3".execute
    smp.latestRevId should be(rev2)
    smp2.latestRevId should be(rev3)

    // DELETE
    sqlu"DELETE FROM usecase_rev where id = $rev3".execute
    smp2.latestRevId should be(revB1)
    sqlu"DELETE FROM usecase_rev where id = $revB1".execute
    smp2.latestRevId should be(TMP)

    // UPDATE ident_id of latest rev
    val revB2 = smp2.insertRev(8, "qwe")
    smp2.latestRevId should be(revB2)
    sqlu"UPDATE usecase_rev set ident_id = ${smp.ucId} WHERE ident_id = ${smp2.ucId} AND id = $revB2".execute
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
      val uc = new SampleUC(1, fk)

      val ucr = uc.insertRev(1, "My UC")
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

  describe(Tables.ShareViewLog.name) {
    def shareViewCount(shareId: Long): Long = sql"SELECT view_count FROM share WHERE id = $shareId".as[Long].first

    it("should update agg view stats by trigger") {
      val a, b = newShare()
      def viewCounts = (shareViewCount(a), shareViewCount(b))
      viewCounts shouldBe (0,0)
      dao.logShareView(a, None); viewCounts shouldBe (1,0)
      dao.logShareView(a, None); viewCounts shouldBe (2,0)
      dao.logShareView(b, None); viewCounts shouldBe (2,1)
    }
  }

  describe(Tables.UsrLoginLog.name) {
    def loginCount(userId: Long): Long = sql"SELECT login_count FROM usr WHERE id = $userId".as[Long].first

    it("should update agg view stats by trigger") {
      val a, b = newUserId()
      def viewCounts = (loginCount(a), loginCount(b))
      viewCounts shouldBe (0,0)
      dao.logUserLogin(a, None); viewCounts shouldBe (1,0)
      dao.logUserLogin(a, None); viewCounts shouldBe (2,0)
      dao.logUserLogin(b, None); viewCounts shouldBe (2,1)
    }
  }

  describe(Tables.Usrd.name) {
    def nameHistory(userId: Long) =
      sql"select name from usrh_name where usr_id=$userId order by updated_at".as[String].list()
    def insert(userId: Long, name: String, newsletter: Boolean) =
      sqlu"insert into usrd values($userId,$name,$newsletter)".execute()
    def update(userId: Long, name: String, newsletter: Boolean) =
      sqlu"update usrd set name=$name, newsletter=$newsletter where usr_id=$userId".execute()
    def read(userId: Long) =
      sql"select name, newsletter from usrd where usr_id=$userId".as[(String,Boolean)].first()

    it("should record name changes") {
      val u = newUserId()
      val (a,b,c) = ("Alice","Bob","Yay")
      insert(u, a, true)
      nameHistory(u) shouldBe Nil
      List(b,b,b,c).foreach(update(u, _, true))
      nameHistory(u) shouldBe List(a, b)
    }

    it("should updates without altercation by triggers") {
      val u = newUserId()
      insert(u, "A", true)
      update(u, "B", false)
      read(u) shouldBe ("B", false)
    }
  }
}
