package com.beardedlogic.usecase
package db

import org.scalatest.FunSpec
import test.TestDatabaseSupport
import slick.jdbc.StaticQuery.interpolation
import lib.field.{TextFieldDefinition, NormalCourseFieldDefinition, ExceptionCourseFieldDefinition}
import lib.Defaults
import lib.Types._
import AutoExternaliseIds._

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
            case Success(n) => assertUC(n, src.withTitle("omg".validated), 1); n
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
          case Success(x) => x
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
        val txt1 = dao.createTextRev(dao.createTextIdent(uc1, fk1), 1, "mesopelagic".hasNormalisedRefs)
        val txt2 = dao.createTextRev(dao.createTextIdent(uc1, fk2), 1, "bathyalpelagic".hasNormalisedRefs)
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
        assertTableDiffs()(dao.updateProject(p, u, "B".validated)) ==== Success
        dao.findProject(p).get.name ==== "B"
      }

      it("should reject duplicate names") {
        val (u, p1) = newUserAndProject("A".validated)
        val p2 = dao.createProject(u, "B".validated).gimme
        dao.updateProject(p2, u, "A".validated) ==== NameAlreadyInUse
      }

      it("should fail when project not found") {
        dao.updateProject(0.tag[ProjectIdTag], 0.tag[UserIdTag], "A".validated) ==== ProjectNotFound
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
        val s1 = ProjectSummary(p1, "Bereft", 0, None)
        summariseWithNoise(u) ==== List(s1)
        val p2 = dao.createProject(u, "Apple".validated).gimme
        dao.summariseProjects(u) ==== List(ProjectSummary(p2, "Apple", 0, None), s1)
        createUseCaseIdentAndRev1(p2, "yo".validated)
        val r = dao.summariseProjects(u)
        r.map(_.copy(ucUpdatedAt = None)) ==== List(ProjectSummary(p2, "Apple", 1, None), s1)
        r(0).ucUpdatedAt shouldBe defined
      }
    }
  }
}