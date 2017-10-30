package shipreq.webapp.server.db

import doobie.imports._
import japgolly.microlibs.nonempty._
import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import utest._
import scalaz.{-\/, \/-}
import shipreq.base.db.SqlHelpers._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.util._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.user._
import shipreq.webapp.server.security.AppSecurityRealm
import shipreq.webapp.server.test._
import shipreq.webapp.server.test.WebappServerTestUtil._

object DbTest extends TestSuite {

  private val q3 = "\"\"\""

  def db = PrepareEnv.dbAlgebra
  def dbSec = DbInterpreter.ForSecurity

  def demo[E <: ActiveEvent](gen: Gen[E]) = {
    import EventDbCodecs.eventCodecRegistry
    val es = gen.samples(GenSize(3)).take(10)
    println("_"*120)
    for (e <- es) {
      val c = eventCodecRegistry.writer(e)
      val d = c._2.write(e)
      val json = Option(d._3).fold("null")(q3 + _ + q3)
      println(
        s"""
           |testRW(${e.toString.replaceAll(",(?! )", ", ")},
           |  ${c._1}, ${d._2}, '${d._1.value}', $json)
         """.stripMargin)
    }
    println()
  }

  def testRW(e: ActiveEvent, typeId: Short, dataId: Option[Int], dataTypeId: Char, data: String): Unit = {
    import EventDbCodecs.eventCodecRegistry

    val c = eventCodecRegistry.writer(e)
    val (aDataTypeId, aDataId, aData) = c._2.write(e)
    assertEq(s"TypeId for $e", c._1, typeId)
    assertEq(s"DataId for $e", (aDataId, aDataTypeId.value), (dataId, dataTypeId))
    assertEq(s"Data for $e", aData, data)

    val aEvent = c._2.read(aDataTypeId.toByte, aDataId, aData)
    assertEq(aEvent, e)
  }

  import shipreq.webapp.base.test.UnsafeTypes._
  import shipreq.webapp.base.test.UnsafeTypes.AutoNES._

  override def tests = TestSuite {

    'to_iso8601_str {
      def test(in: String, out: String): Unit =
        TestDb().runNow { xa =>
          val q = Query0[String](s"select to_iso8601_str(timestamptz '$in')")
          val r: String = xa ! q.unique
          assertEq(r, out)
        }

      'typicalPrecision -
        test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")

      'morePrecision -
        test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")

      'lesserPrecision -
        test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")

      'null - TestDb().runNow { xa =>
        val q = Query0[Option[String]](s"select to_iso8601_str(NULL)")
        assertEq(xa ! q.unique, None)
      }
    }

    'instant {
      def assertApproxEqual(a: Instant, e: Instant): Unit =
        assertEq(Duration.between(a, e).abs.minusSeconds(2).isNegative, true)

      'read - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Instant, Int)](s"select now(), 2").unique
        assertEq(i, 2)
        assertApproxEqual(dbNow, Instant.now())
      }

      'readSome - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select now(), 3").unique
        assertEq(i, 3)
        assertEq(dbNow.isDefined, true)
        assertApproxEqual(dbNow.get, Instant.now())
      }

      'readNone - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select null :: timestamptz, 5").unique
        assertEq(i, 5)
        assertEq(dbNow.isDefined, false)
      }

      'write - TestDb().runNow { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2084, 5, 2, 18, 30, 8)
        val i = l.toInstant(ZoneOffset.of("+11:00"))
        val r = "yay"
        xa ! Update[(Instant, String, Long)]("UPDATE usr SET confirmed_at=?, roles=? WHERE id=?").toUpdate0(i, r, u.value).run
        val s1 = xa ! Query0[String](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, "2084-05-02T07:30:08Z")
        val (ar, ai) = xa ! Query0[(String, Instant)](s"select roles, confirmed_at from usr where id=${u.value: Long}").unique
        assertEq(ai, i)
        assertEq(ar, r)
      }

      'writeOption - TestDb().runNow { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2030, 9, 7, 20, 20, 4)
        val i = l.toInstant(ZoneOffset.of("+15:00"))
        xa ! Update[(Option[Instant], Option[Instant], Long)]("UPDATE usr SET reset_password_sent_at=?, confirmed_at=? WHERE id=?")
          .toUpdate0(None, Some(i), u.value).run
        val s1 = xa ! Query0[Option[String]](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, Some("2030-09-07T05:20:04Z"))
        val s2 = xa ! Query0[Option[String]](s"select to_iso8601_str(reset_password_sent_at) from usr where id=${u.value: Long}").unique
        assertEq(s2, None)
      }
    }

    'user {
      'resetPasswordFns - TestDb().runNow { xa =>
        val dbu = DbUtil(xa)
        val u = dbu.newUserId()
        val username = xa ! Query0[String](s"select username from usr where id=${u.value: Long}").unique
        val token = xa ! db.createResetPasswordToken(u)

//        val date = xa ! db.getResetPasswordTokenIssueDate(token)
//        assert(!ResetPassword.isTokenExpired(date.get)) TODO

        xa ! db.updateResetPasswordTokenOnReissue(u)
//        val date2 = xa ! db.getResetPasswordTokenIssueDate(token)
//        assert(!ResetPassword.isTokenExpired(date2.get)) TODO

        val p = PlainTextPassword("hehegreat100")
        val ps = AppSecurityRealm.randomHashFn(p)
        xa ! db.updateUserPassword(token, ps)

        assertEq(xa ! db.getResetPasswordTokenIssueDate(token), None)
        val ps2 = (xa ! dbSec.getUserAndPassword(username)).get._2
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

        val ordCounter = new AtomicInteger()

        val prop = Prop.equal[(ActiveEvent, HashRec.Collection)]("load . save = id")(
          i => TestDb().runNow { xa =>
            val dbu = DbUtil(xa)
            val projectId = dbu.newProjectId()
            val org = EventOrd(ordCounter.incrementAndGet())
            xa ! db.saveProjectEvent(projectId)(org, i._1, i._2)
            val loaded =
              dbu.debugSelectOnError(s"select * from event e, event_hash eh where e.project_id=eh.project_id and e.ord=eh.ord and e.ord = ${org.value}") {
                (xa ! db.getAllProjectEvents(projectId)).toVector.filter(_._1 ==* org).map(r => r._2.event match {
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
          testRW(ProjectTemplateApply(ProjectTemplate.Default), 1000, None, ' ', """1""")
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

          testRW(GenericReqCreate(123, 1469773577, Codes(ReqCode.IdAndValue(7, "yay"))),
            100, 123, 'g', """{"T":1469773577,"c":{"yay":7}}""")
        }

        //'createCodeGroup     - demo(RandomData.events.createCodeGroup    )

        //'createTagGroup         - demo(RandomData.events.createTagGroup        )

        //'deleteCustomField      - demo(RandomData.events.deleteCustomField     )

        //'deleteCustomIssueType  - demo(RandomData.events.deleteCustomIssueType )

        //'deleteCustomReqType    - demo(RandomData.events.deleteCustomReqType   )

        //'deleteCodeGroup     - demo(RandomData.events.deleteCodeGroup    )

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

        //'updateCodeGroup     - demo(RandomData.events.updateCodeGroup    )

        //'updateTagGroup         - demo(RandomData.events.updateTagGroup        )

        //'addUseCaseStep        - demo(RandomData.events.addUseCaseStep       )
        //'shiftUseCaseStepLeft  - demo(RandomData.events.shiftUseCaseStepLeft )
        //'shiftUseCaseStepRight - demo(RandomData.events.shiftUseCaseStepRight)
        //'deleteUseCaseStep     - demo(RandomData.events.deleteUseCaseStep    )
        //'setUseCaseStepText    - demo(RandomData.events.setUseCaseStepText   )
        //'setUseCaseTitle       - demo(RandomData.events.setUseCaseTitle      )
        //'createUseCase         - demo(RandomData.events.createUseCase        )

        'savedViewCreate {
          import reqtable._
          import Column.{CustomField => CF, _}
          import SortCriterion._
          import SortMethod._
          import Filter.Valid._

          val e =
            SavedViewCreate(
              SavedView.Id(7593),
              SavedView.Name("7AvWNHb95"),
              NonEmptyVector(
                Implications(Forwards),
                Implications(Backwards),
                CF(CustomField.Tag.Id(27887)),
                DeletionReason),
              SortCriteria(Vector(
                InconclusiveCB(DeletionReason, AscThenBlanks),
                InconclusiveIB(ReqType, Desc),
                InconclusiveCB(Tags, BlanksThenDesc)),
                Conclusive(Pubid, Desc)),
              ShowDead,
              Some(
                not(
                  anyOf(
                    hashRef(\/-(ApplicableTagId(300))),
                    hashRef(-\/(CustomIssueTypeId(400)))))))

          testRW(e, 2100, 7593, ' ',
            """
              |{
              |  "n":"7AvWNHb95",
              |  "c":[{"i":"f"},{"i":"b"},{"f":{"t":27887}},"d"],
              |  "o":[[{"_":["d","a_"]},{"i":["T","d"]},{"_":["#","_d"]}],["I","d"]],
              |  "x":"s",
              |  "f":{
              |    "!":{
              |      "|":[
              |         {"#":{"R":300}},
              |         {"#":{"L":400}}]}}}
            """.stripMargin.replaceAll(" *\n *", "").trim)
        }

      }
    }

  }
}
