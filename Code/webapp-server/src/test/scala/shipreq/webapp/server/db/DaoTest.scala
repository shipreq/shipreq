package shipreq.webapp.server.db

import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import org.scalatest.FunSpec
import slick.jdbc.StaticQuery.{queryNA, update, updateNA}
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.UserId
import shipreq.webapp.server.data._
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.server.snippet.ResetPassword
import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import utest._
import shipreq.base.util._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.base.text.Text
import shipreq.webapp.server.test.{DbUtil, TestDb}
import shipreq.webapp.server.test.WebappServerTestUtil._
import EventDao.EventSeq

object DaoTest extends TestSuite {

  private val q3 = "\"\"\""
  def demo[E <: ActiveEvent](gen: Gen[E]) = {
    import EventDbCodecs.eventCodecRegistry
    val es = gen.samplesSized(3).take(10)
    println("_"*120)
    for (e <- es) {
      val c = eventCodecRegistry.writer(e)
      val d = c._2.write(e)
      val json = Option(d._3).fold("null")(q3 + _ + q3)
      println(
        s"""
           |testRW(${e.toString.replaceAll(",(?! )", ", ")},
           |  ${c._1}, ${d._2}, '${d._1.getValue}', $json)
         """.stripMargin)
    }
    println()
  }

  def testRW(e: ActiveEvent, typeId: Short, dataId: Integer, dataTypeId: Char, data: String): Unit = {
    import EventDbCodecs.eventCodecRegistry

    val c = eventCodecRegistry.writer(e)
    val (aDataTypeId, aDataId, aData) = c._2.write(e)
    assertEq(s"TypeId for $e", c._1, typeId)
    assertEq(s"DataId for $e", (aDataId, aDataTypeId.getValue), (dataId, dataTypeId.toString))
    assertEq(s"Data for $e", aData, data)

    val aEvent = c._2.read(aDataTypeId.getValue.head.toByte, aDataId, aData)
    assertEq(aEvent, e)
  }

  import shipreq.webapp.base.test.UnsafeTypes._
  import shipreq.webapp.base.test.UnsafeTypes.AutoNES._

  override def tests = TestSuite {

    'to_iso8601_str {
      def test(in: String, out: String): Unit =
        TestDb.Transaction { implicit s =>
          val q = queryNA[String](s"select to_iso8601_str(timestamptz '$in')")
          val r: String = q.first
          assertEq(r, out)
        }

      'typicalPrecision -
        test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")

      'morePrecision -
        test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")

      'lesserPrecision -
        test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")

      'null - TestDb.Transaction { implicit s =>
        val q = queryNA[String](s"select to_iso8601_str(NULL)")
        assertEq(q.first, null)
      }
    }

    'instant {
      def assertApproxEqual(a: Instant, e: Instant): Unit =
        assertEq(Duration.between(a, e).abs.minusSeconds(2).isNegative, true)

      'read - TestDb.Transaction { implicit s =>
        val (dbNow, i) = queryNA[(Instant, Int)](s"select now(), 2").first
        assertEq(i, 2)
        assertApproxEqual(dbNow, Instant.now())
      }

      'readSome - TestDb.Transaction { implicit s =>
        val (dbNow, i) = queryNA[(Option[Instant], Int)](s"select now(), 3").first
        assertEq(i, 3)
        assertEq(dbNow.isDefined, true)
        assertApproxEqual(dbNow.get, Instant.now())
      }

      'readNone - TestDb.Transaction { implicit s =>
        val (dbNow, i) = queryNA[(Option[Instant], Int)](s"select null :: timestamptz, 5").first
        assertEq(i, 5)
        assertEq(dbNow.isDefined, false)
      }

      'write - TestDb.Transaction { implicit s =>
        val u = DbUtil(s).newUserId()
        val l = LocalDateTime.of(1984, 5, 2, 18, 30, 8)
        val i = l.toInstant(ZoneOffset.of("+11:00"))
        val r = "yay"
        update[(Instant, String, Long)]("UPDATE usr SET confirmed_at=?, roles=? WHERE id=?").apply((i, r, u.value)).execute
        val s1 = queryNA[String](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").first
        assertEq(s1, "1984-05-02T07:30:08Z")
        val (ar, ai) = queryNA[(String, Instant)](s"select roles, confirmed_at from usr where id=${u.value: Long}").first
        assertEq(ai, i)
        assertEq(ar, r)
      }

      'writeOption - TestDb.Transaction { implicit s =>
        val u = DbUtil(s).newUserId()
        val l = LocalDateTime.of(1990, 9, 7, 20, 20, 4)
        val i = l.toInstant(ZoneOffset.of("+15:00"))
        update[(Option[Instant], Option[Instant], Long)]("UPDATE usr SET reset_password_sent_at=?, confirmed_at=? WHERE id=?")
          .apply((None, Some(i), u.value)).execute
        val s1 = queryNA[Option[String]](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").first
        assertEq(s1, Some("1990-09-07T05:20:04Z"))
        val s2 = queryNA[Option[String]](s"select to_iso8601_str(reset_password_sent_at) from usr where id=${u.value: Long}").first
        assertEq(s2, None)
      }
    }

    'user {
      'resetPasswordFns - TestDb.Transaction { implicit s =>
        val dbu = DbUtil(s)
        val dao = dbu.dao
        val u = dbu.newUserId()
        val username = queryNA[String](s"select username from usr where id=${u.value: Long}").first
        val token = dao.performInstallNewResetPasswordToken(u, () => s"token.$u")

        val date = dao.findResetPasswordTokenIssuedDate(token).get
        assert(!ResetPassword.isTokenExpired(date))

        dao.performReuseResetPasswordToken(u)
        val date2 = dao.findResetPasswordTokenIssuedDate(token).get
        assert(!ResetPassword.isTokenExpired(date2))

        val p = "hehegreat100"
        val ps = PasswordAndSalt.createWithRandomSalt(p)
        dao.performPasswordReset(ps, token)

        assertEq(dao.findResetPasswordTokenIssuedDate(token), None)
        val ps2 = dao.findUserDescAndCredentials(username).get._2
        assertEq(ps2.matches(p), true)
      }
    }

//  describe("Project") {
//    import Tables.{Project => TProject}
//
////    def newUserAndProject() = {
////      val u = newUserId()
////      val p = dao.createProject(u)
////      (u, p)
////    }
//
//    describe("create") {
//      it("should create a new project") {
//        val u = newUserId()
//        assertTableDiffs(TProject -> 1) {dao.createProject(u)}
//      }
//    }
//
////    describe("rename") {
////      import UpdateProjectResult._
////
////      it("should update the project name") {
////        val (u, p) = newUserAndProject("A")
////        assertTableDiffs()(dao.updateProject(p, u, "B")) ==== DbSuccess
////        dao.findProject(p).get.name ==== "B"
////      }
////
////      it("should reject duplicate names") {
////        val (u, p1) = newUserAndProject("A")
////        val p2 = dao.createProject(u, "B").gimme
////        dao.updateProject(p2, u, "A") ==== NameAlreadyInUse
////      }
////
////      it("should fail when project not found") {
////        dao.updateProject(ProjectId(0), UserId(0), "A") ==== ProjectNotFound
////      }
////
////      it("should fail when project doesnt belong to user") {
////        val (u, p) = newUserAndProject("A")
////        dao.updateProject(p, newUserId(), "B") ==== ProjectNotFound
////      }
////    }
//
////    def afterDeletion: (UserId, ProjectId, ProjectId) = {
////      val (uid, p1) = newUserAndProject("wow")
////      assertTableDiffs()(dao deleteProjectSoft p1)
////      val p2 = dao.createProject(uid, "wow").gimme
////      assertTableDiffs()(dao deleteProjectSoft p2)
////      (uid, p1, p2)
////    }
////
////    it("deletion should be soft and hard") {
////      val (_, p1, p2) = afterDeletion
////      val a = new AsyncDao
////      assertTableDiffs(Tables.Project -> -1)(a deleteProject p1)
////      assertTableDiffs(Tables.Project -> -1)(a deleteProject p2)
////    }
////
////    it("soft deletion should hide the project from view") {
////      val (u, p, _) = afterDeletion
////      dao.findProject(p) shouldBe None
////    }
//  }

    'event {

      'prop - {
        // implicit val settings = DefaultSettings.propSettings.setSampleSize(10).setGenSize(10).setDebug.setSingleThreaded.setSeed(0)
        // implicit val settings = DefaultSettings.propSettings.setSampleSize(20000).setGenSize(4).setDebug
        implicit val settings = DefaultSettings.propSettings.setSampleSize(320).setGenSize(16)

        TestDb.DbUtil.scope(_.map(_.newProjectId())) { dbAndProjectId =>
          val seqCounter = new AtomicInteger()

          val prop = Prop.equal[(ActiveEvent, HashRec.Collection)]("load . save = id")(
            i => dbAndProjectId { case (dbh, projectId) =>
              val seq = EventSeq(seqCounter.incrementAndGet())
              dbh.dao.createEvent(projectId, seq, i._1, i._2)
              val loaded =
                dbh.debugSelectOnError(s"select * from event e, event_hash eh where e.project_id=eh.project_id and e.seq=eh.seq and e.seq = ${seq.value}") {
                  dbh.dao.findAllEvents(projectId).filter(_._1 == seq).map(r => r._2.event match {
                    case ae: ActiveEvent => (ae, r._2.hashRecs)
                    case e               => sys error s"Not an ActiveEvent: $e"
                  })
                }
              loaded
            },
            Vector1(_))

          val rnd = RandomData.events.activeEvent *** RandomData.events.hashRecs

          prop.mustBeSatisfiedBy(rnd)
        }
      }

      // These tests serve two purposes:
      // 1) Ensure data is stored as expected (efficient & human-readable).
      // 2) Ensure each Event's format never changes once established.
      'data {

        'FieldStaticAdd {
          testRW(FieldStaticAdd(StaticField.ExceptionStepTree), 1110, -2, 's', null)
          testRW(FieldStaticAdd(StaticField.NormalAltStepTree), 1110, -1, 's', null)
          testRW(FieldStaticAdd(StaticField.StepGraph        ), 1110, -3, 's', null)
        }

        'ProjectTemplateApply {
          testRW(ProjectTemplateApply(ProjectTemplate.Default), 1000, null, ' ', """1""")
        }

        //'createApplicableTag    - demo(RandomData.events.createApplicableTag   )

        //'createCustomImpField   - demo(RandomData.events.createCustomImpField  )

        //'createCustomIssueType  - demo(RandomData.events.createCustomIssueType )

        //'createCustomReqType    - demo(RandomData.events.createCustomReqType   )

        //'createCustomTagField   - demo(RandomData.events.createCustomTagField  )

        //'createCustomTextField  - demo(RandomData.events.createCustomTextField )

        'GenericReqCreate {
          import GenericReqGD._
          testRW(GenericReqCreate(123, 449099973, Tags(NonEmptySet(ApplicableTagId(166426196)))),
            100, 123, 'g', """{"T":449099973,"#":166426196}""")

          testRW(GenericReqCreate(123, 1469773577, ImpTgts(NonEmptySet(GenericReqId(2074289209)))),
            100, 123, 'g', """{"T":1469773577,"<":2074289209}""")

          testRW(GenericReqCreate(123, 1469773577, ImpSrcs(NonEmptySet(GenericReqId(2074289209)))),
            100, 123, 'g', """{"T":1469773577,">":2074289209}""")

          testRW(GenericReqCreate(123, 1469773577, ReqCodes(ReqCode.IdAndValue(7, "yay"))),
            100, 123, 'g', """{"T":1469773577,"c":{"yay":7}}""")
        }

        //'createReqCodeGroup     - demo(RandomData.events.createReqCodeGroup    )

        //'createTagGroup         - demo(RandomData.events.createTagGroup        )

        //'deleteCustomField      - demo(RandomData.events.deleteCustomField     )

        //'deleteCustomIssueType  - demo(RandomData.events.deleteCustomIssueType )

        //'deleteCustomReqType    - demo(RandomData.events.deleteCustomReqType   )

        //'deleteReqCodeGroup     - demo(RandomData.events.deleteReqCodeGroup    )

        //'deleteReq              - demo(RandomData.events.deleteReq             )

        //'deleteStaticField      - demo(RandomData.events.deleteStaticField     )

        //'deleteTag              - demo(RandomData.events.deleteTag             )

        'ReqImplicationsPatch {
          testRW(ReqImplicationsPatch(1234, Forwards, nesd[ReqId](34, 45)(67, 89)),
            22, 1234, 'g', """{"d":"f","-":[34,45],"+":[67,89]}""")

          testRW(ReqImplicationsPatch(1234, Backwards, nesd[ReqId]()(2128131835)),
            22, 1234, 'g', """{"d":"b","+":2128131835}""")

          testRW(ReqImplicationsPatch(1234, Forwards, nesd[ReqId](1086529477)()),
            22, 1234, 'g', """{"d":"f","-":1086529477}""")
        }

        'ReqCodesPatch {
          val mm = UnivEq.emptySetMultimap[ReqCode.Value, ReqCodeId]
          testRW(ReqCodesPatch(666, Set(3), Set(1095731751, 1055755379), mm),
            20, 666, 'g', """{"-":3,"^":[1095731751,1055755379]}""")

          testRW(ReqCodesPatch(667, Set(), Set(), mm.add("lnls", 1168583026).addvs("m", Set(976426332, 522011847))),
            20, 667, 'g', """{"+":{"m":[522011847,976426332],"lnls":1168583026}}""")
        }

        'ReqTagsPatch {
          testRW(ReqTagsPatch(1234, nesd[ApplicableTagId](34, 45)(67, 89)),
            23, 1234, 'g', """{"-":[34,45],"+":[67,89]}""")

          testRW(ReqTagsPatch(1234, nesd[ApplicableTagId]()(2128131835)),
            23, 1234, 'g', """{"+":2128131835}""")

          testRW(ReqTagsPatch(1234, nesd[ApplicableTagId](1086529477)()),
            23, 1234, 'g', """{"-":1086529477}""")
        }

        //'repositionField        - demo(RandomData.events.repositionField       )

        'ReqFieldCustomTextSet {
          testRW(ReqFieldCustomTextSet(2345, CustomField.Text.Id(123), Text.CustomTextField.demo(9, 8, 5, 7, 6)),
          21, 2345, 'g',
            """
              │{"f":123,"t":[
              │"Atom demonstration.",
              │0,
              │"Here we go:",
              │{"*":[
              │["Req: ",{"r":9}],
              │["UC Step Req: ",{"u":5}],
              │["Code: ",{"c":8}],
              │["Tag: ",{"t":7}],
              │["Issue(∅): ",{"i":6}],
              │["Issue(∃): ",{"i":6,"?":["Need to finish ",{"r":9},", ",{"u":5}," and ",{"c":8}]}],
              │["Issue(∃): ",{"i":6,"?":["Ask ",{"@":"bob@gmail.com"}," about ",{"=":"e=mc^2"}]}],
              │[],
              │["Math: ",{"=":"f(x) = {x+1 \\over x - 1} + 9\\pi^2"}],
              │["Email: ",{"@":"blah@google.com"}],
              │["Web: ",{"/":"https://shipreq.com"}]
              │]}
              │]}
            """.stripMargin('│').replace("\n", "").trim
          )
        }

        //'setGenericReqTitle     - demo(RandomData.events.setGenericReqTitle    )

        //'setGenericReqType      - demo(RandomData.events.setGenericReqType     )

        //'updateApplicableTag    - demo(RandomData.events.updateApplicableTag   )

        //'updateCustomImpField   - demo(RandomData.events.updateCustomImpField  )

        //'updateCustomIssueType  - demo(RandomData.events.updateCustomIssueType )

        //'updateCustomReqType    - demo(RandomData.events.updateCustomReqType   )

        //'updateCustomTagField   - demo(RandomData.events.updateCustomTagField  )

        //'updateCustomTextField  - demo(RandomData.events.updateCustomTextField )

        //'updateReqCodeGroup     - demo(RandomData.events.updateReqCodeGroup    )

        //'updateTagGroup         - demo(RandomData.events.updateTagGroup        )

        //'addUseCaseStep        - demo(RandomData.events.addUseCaseStep       )
        //'shiftUseCaseStepLeft  - demo(RandomData.events.shiftUseCaseStepLeft )
        //'shiftUseCaseStepRight - demo(RandomData.events.shiftUseCaseStepRight)
        //'deleteUseCaseStep     - demo(RandomData.events.deleteUseCaseStep    )
        //'setUseCaseStepText    - demo(RandomData.events.setUseCaseStepText   )
        //'setUseCaseTitle       - demo(RandomData.events.setUseCaseTitle      )
        //'createUseCase         - demo(RandomData.events.createUseCase        )

      }
    }
  }
}
