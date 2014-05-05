package shipreq.webapp.db

import org.scalatest.FunSpec
import slick.jdbc.{StaticQuery => Q}
import Q.interpolation

import shipreq.taskman.api.Types.IsUserId
import shipreq.webapp.feature.UcFilters
import shipreq.webapp.feature.uc.field.{TextFieldDefinition, NormalCourseFieldDefinition, ExceptionCourseFieldDefinition}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.snippet.ResetPassword
import shipreq.webapp.test.TestDatabaseSupport

class DaoTest extends FunSpec with TestDatabaseSupport {
  implicit def str2uch(title: String @@ Validated): UseCaseHeader = UseCaseHeader(title)

  describe("FieldList") {
    lazy val fl1 =
      NormalCourseFieldDefinition ::
        TextFieldDefinition("Opeth") ::
        TextFieldDefinition("Heritage") ::
        TextFieldDefinition("Haxprocess") ::
        ExceptionCourseFieldDefinition ::
        Nil

    lazy val fl2 =
      TextFieldDefinition("Opeth") ::
        TextFieldDefinition("Heritage") ::
        TextFieldDefinition("CHANGED") ::
        NormalCourseFieldDefinition ::
        ExceptionCourseFieldDefinition ::
        Nil

    describe("syncFieldList") {
      it("should save when never saved before") {
        sqlu" UPDATE field_key SET data='hehe' WHERE data IS NULL ".execute
        val fl = assertTableDiffs(Tables.FieldKey -> fl1.size) {dao.syncFieldList(fl1)}
        fl.fieldDefns ==== fl1
      }

      it("should use existing when already saved") {
        val save1 = dao.syncFieldList(fl1)
        assertTableDiffs() {
          val save2 = dao.syncFieldList(fl1)
          save2 ==== save1
        }
      }

      it("should save differences when it differs") {
        val save1 = dao.syncFieldList(fl1)
        val save2 = assertTableDiffs(Tables.FieldKey -> 1) {dao.syncFieldList(fl2)}
        save2.fieldDefns ==== fl2
      }
    }
  }

  // ===================================================================================================================

  describe("to_iso8601_str") {

    def test(in: String, out: String): Unit = {
      val q = Q.queryNA[String](s"select to_iso8601_str(timestamptz '$in')")
      val r: String = q.first
      r shouldBe out
    }

    it("should work with typical precision") {
      test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")
    }

    it("should work with more precision") {
      test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")
    }

    it("should work with lesser precision") {
      test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")
    }

    it("should work with NULLs") {
      val q = Q.queryNA[String](s"select to_iso8601_str(NULL)")
      q.first shouldBe null
    }
  }

  // ===================================================================================================================

  describe("UseCase") {

    describe("findUseCase") {
      it("should load when found") {
        val saved = createUseCaseIdentAndRev1(newProjectId(), "ah".validated)
        dao.findUseCaseRev(saved).get ==== saved
      }
    }

    describe("updateUseCaseHeader") {
      import UseCaseHeaderUpdateResult._

      def assertUC(revId: UseCaseRevId, expected: UseCaseRev, revOffset: Int) {
        val uc = dao.findUseCaseRev(revId).get
        uc.identId ==== expected.identId
        uc.rev.toInt ==== expected.rev + revOffset
        uc.header ==== expected.header
      }

      def assertAuditedUpdate(src: UseCaseRev, relationRows: Int = 0)(implicit projectId: ProjectId): UseCaseRev = {
        import Tables._
        assertTableDiffs(UsecaseRev -> 1, UcField -> relationRows) {
          val r = updateUseCaseHeader(src, _.copy(title = "omg".validated))
          r match {
            case DbSuccess(n) => assertUC(n, src.withTitle("omg".validated), 1); n
            case _ => fail("Expected Success. Got " + r)
          }
        }
      }

      def assertNOP(uc: UseCaseRev, expected: UseCaseRev)(implicit projectId: ProjectId) {
        assertTableDiffs() {
          val r = updateUseCaseHeader(uc, identity)
          r ==== AlreadyUpToDate(expected)
        }
      }

      def createTwoRevs(implicit projectId: ProjectId) = {
        val rev1 = createUseCaseIdentAndRev1(projectId, "Haha".validated)
        val rev2s = updateUseCaseHeader(rev1, _.copy(title = "wow".validated)) match {
          case DbSuccess(x) => x
          case _ => fail("Expected Success.")
        }
        val rev2 = dao.findUseCaseRev(rev2s).get
        (rev1, rev2)
      }

      it("should do an audited update when rev #1 and non-default title changes") {
        implicit val pid = newProjectId()
        assertAuditedUpdate(createUseCaseIdentAndRev1(pid, "Haha".validated))
      }

      it("should do an audited update when rev #2+") {
        implicit val pid = newProjectId()
        val (_, rev2) = createTwoRevs
        assertAuditedUpdate(rev2)
      }

      it("should copy relationships when performing an audited update") {
        val fk1 = dao.createFieldKey(FieldKeyType.Text, Some("THE OCEAN"))
        val fk2 = dao.createFieldKey(FieldKeyType.Text, Some("PELAGIAL"))
        val uc1 = createUseCaseIdentAndRev1(newProjectId(), "Haha".validated)
        val txt1 = dao.createTextRev(dao.createTextIdent(uc1, fk1), 1, "mesopelagic".tag[IsNormalised])
        val txt2 = dao.createTextRev(dao.createTextIdent(uc1, fk2), 1, "bathyalpelagic".tag[IsNormalised])
        dao.linkUcToText(uc1, txt1)
        dao.linkUcToText(uc1, txt2)

        implicit val pid = newProjectId()
        val uc2 = assertAuditedUpdate(uc1, 2)
        dao.findAllUcFieldData(uc2).map(_.textRev).map(_.toString).sorted ==== List(txt1, txt2).map(_.toString).sorted
      }

      it("should do nothing when rev #1 and no change") {
        implicit val pid = newProjectId()
        val rev1 = createUseCaseIdentAndRev1(pid, "YAY".validated)
        assertNOP(rev1, rev1)
      }

      it("should do nothing when rev #2 and no change") {
        implicit val pid = newProjectId()
        val (_, rev2) = createTwoRevs
        assertNOP(rev2, rev2)
      }
    }
  }

  // ===================================================================================================================

  describe("Project") {
    import Tables.{Project => TProject}

    def newUserAndProject(projectName: String @@ Validated) = {
      val u = newUserId
      val p = dao.createProject(u, projectName).gimme
      (u, p)
    }

    def newUserProjectAndUseCase(projectName: String @@ Validated, ucName: String @@ Validated) = {
      val (u, p) = newUserAndProject(projectName)
      val uc = createUseCaseIdentAndRev1(p, ucName)
      (u, p, uc)
    }

    describe("create") {
      import CreateProjectResult._

      it("should create a new project") {
        val u = newUserId
        assertTableDiffs(TProject -> 1) {dao.createProject(u, "Blah".validated)}
      }

      it("should reject duplicate project names") {
        val t = "Yay".validated
        val (u, _) = newUserAndProject(t)
        val (_, _) = newUserAndProject(t)
        dao.createProject(u, t) ==== NameAlreadyInUse
      }
    }

    describe("update") {
      import UpdateProjectResult._

      it("should update the project name") {
        val (u, p) = newUserAndProject("A".validated)
        assertTableDiffs()(dao.updateProject(p, u, "B".validated)) ==== DbSuccess
        dao.findProject(p).get.name ==== "B"
      }

      it("should reject duplicate names") {
        val (u, p1) = newUserAndProject("A".validated)
        val p2 = dao.createProject(u, "B".validated).gimme
        dao.updateProject(p2, u, "A".validated) ==== NameAlreadyInUse
      }

      it("should fail when project not found") {
        dao.updateProject(0.tag[IsProjectId], 0.tag[IsUserId], "A".validated) ==== ProjectNotFound
      }

      it("should fail when project doesnt belong to user") {
        val (u, p) = newUserAndProject("A".validated)
        dao.updateProject(p, newUserId, "B".validated) ==== ProjectNotFound
      }
    }

    describe("summariseProjects") {

      def summariseWithNoise(userId: UserId) = {
        newProjectId(newUserId) // Give some other user a project
        dao.summariseProjects(userId)
      }

      it("should return [] when user has no projects") {
        summariseWithNoise(newUserId) shouldBe empty
      }

      it("should return a summary for each project the user has") {
        val u = newUserId
        val p1 = dao.createProject(u, "Bereft".validated).gimme
        val s1 = ProjectSummary(p1, "Bereft", 0, None, 0, 0, None)
        summariseWithNoise(u) ==== List(s1)
        val p2 = dao.createProject(u, "Apple".validated).gimme
        dao.summariseProjects(u) ==== List(ProjectSummary(p2, "Apple", 0, None, 0, 0, None), s1)

        // + use case
        createUseCaseIdentAndRev1(p2, "yo".validated)
        val r = dao.summariseProjects(u)
        r.map(_.copy(ucUpdatedAt = None)) ==== List(ProjectSummary(p2, "Apple", 1, None, 0, 0, None), s1)
        r(0).ucUpdatedAt shouldBe defined

        // TODO summariseProjects doesn't test shares (or even UCs properly)
      }
    }

    it("findAllLatestUseCaseRevsByProject") {
      newUserProjectAndUseCase("IGNORED".validated, "IGNORED".validated)
      val (_,p,_) = newUserProjectAndUseCase("P1".validated, "YAY".validated)
      dao.findAllLatestUseCaseRevsByProject(p).map(_.title) shouldBe List("YAY")
      createUseCaseIdentAndRev1(p, "yo".validated)
      dao.findAllLatestUseCaseRevsByProject(p).map(_.title) shouldBe List("YAY", "yo")
    }

    it("findAllLatestUseCaseRevs(pid,ids)") {
      newUserProjectAndUseCase("IGNORED".validated, "IGNORED".validated)
      val (_,p,u1) = newUserProjectAndUseCase("P1".validated, "U1".validated)
      val u2 = createUseCaseIdentAndRev1(p, "U2".validated)
      val u3 = createUseCaseIdentAndRev1(p, "U3".validated)
      val u4 = createUseCaseIdentAndRev1(p, "U4".validated)

      dao.findAllLatestUseCaseRevs(p, Nil) shouldBe Nil
      dao.findAllLatestUseCaseRevs(p, List(u2)) shouldBe List(u2)
      dao.findAllLatestUseCaseRevs(p, List(u1, u3, u4)) shouldBe List(u1, u3, u4)
    }

    def afterDeletion: (UserId, ProjectId, ProjectId) = {
      val (uid, p1) = newUserAndProject("wow".tag)
      assertTableDiffs()(dao deleteProjectSoft p1)
      val p2 = dao.createProject(uid, "wow".tag).gimme
      assertTableDiffs()(dao deleteProjectSoft p2)
      (uid, p1, p2)
    }

    it("deletion should be soft and hard") {
      val (_, p1, p2) = afterDeletion
      val a = new AsyncDao
      assertTableDiffs(Tables.Project -> -1)(a deleteProject p1)
      assertTableDiffs(Tables.Project -> -1)(a deleteProject p2)
    }

    it("soft deletion should hide the project from view") {
      val (u, p, _) = afterDeletion
      dao.findProject(p) shouldBe None
      dao.summariseProjects(u).map(_.id) should not contain(p)
    }
  }

  // ===================================================================================================================

  describe("User") {

    it("reset password fns") {
      val u = newUserId
      val username = sql"select username from usr where id=${u: Long}".as[String].first()
      val token = dao.performInstallNewResetPasswordToken(u, () => s"token.$u")

      val date = dao.findResetPasswordTokenIssuedDate(token).get
      ResetPassword.isTokenExpired(date) shouldBe false

      dao.performReuseResetPasswordToken(u)
      val date2 = dao.findResetPasswordTokenIssuedDate(token).get
      ResetPassword.isTokenExpired(date2) shouldBe false

      val p = "hehegreat100"
      val ps = PasswordAndSalt.createWithRandomSalt(p)
      dao.performPasswordReset(ps, token)

      dao.findResetPasswordTokenIssuedDate(token) shouldBe None
      val ps2 = dao.findUserDescAndCredentials(username).get._2
      ps2.matches(p) shouldBe true
    }
  }

  // ===================================================================================================================

  describe("Share") {
    val FilterAllJson = UcFilters.All.json

    def createShare(pid: ProjectId = newProjectId()): Share = {
      val s = dao.createShare(pid, PasswordAndSalt.createWithRandomSalt("volition"), "NAME", Some("pref"), FilterAllJson)
      s.name shouldBe "NAME"
      s.preface shouldBe Some("pref")
      s.ucFilterJson shouldBe FilterAllJson
      s.projectId shouldBe pid
      s
    }

    it("find . create = id") {
      val n, s, m = createShare()
      dao.findShare(s.id) shouldBe Some(s)
    }

    it("create should retry when token taken") {
      val firstToken: ShareUrlToken = "abcdefgh".tag
      val pid = newProjectId()
      val a = dao.createShare(pid, PasswordAndSalt.createWithRandomSalt("v"), "n", None, FilterAllJson, () => firstToken)
      a.urlToken shouldBe firstToken

      var nextToken = firstToken
      val secondToken: ShareUrlToken = "987654321".tag
      val fn = () => {
        val use = nextToken
        nextToken = secondToken
        use
      }
      val b = dao.createShare(pid, PasswordAndSalt.createWithRandomSalt("v"), "n", None, FilterAllJson, fn)
      b.urlToken shouldBe secondToken
    }

    it("findShareAndPassword") {
      val n, s, m = createShare()
      val r = dao.findShareAndPassword(s.urlToken)
      r.map(_._1) shouldBe Some(s)
      r.get._2.matches("volition") shouldBe true
    }

    it("findShareAndProject") {
      val pid = newProjectId()
      val s = createShare(pid)
      val r = dao.findShareAndProject(s.urlToken)
      r.map(_._1) shouldBe Some(s)
      r.get._2 shouldBe dao.findProject(pid).get
    }
  }
}