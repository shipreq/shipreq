package shipreq.webapp.server.db

import cats.implicits._
import doobie._
import doobie.implicits._
import java.sql.Connection
import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.project.RandomEventStream
import shipreq.webapp.server.config.Global
import shipreq.webapp.server.interpreter.ServerInterpreter
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import shipreq.webapp.server.logic.impl.PublicSpaLogic
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._
import sourcecode.Line
import utest._

object DbTest extends TestSuite {
  import DbUtil.crypto

  override def tests = Tests {
    PrepareEnv.dbOnce()

    "to_iso8601_str" - {
      def test(in: String, out: String): Unit =
        TestDb.withImperativeXA { xa =>
          val q = Query0[String](s"select to_iso8601_str(timestamptz '$in')")
          val r: String = xa ! q.unique
          assertEq(r, out)
        }

      "typicalPrecision" -
        test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")

      "morePrecision" -
        test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")

      "lesserPrecision" -
        test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")

      "null" - TestDb.withImperativeXA { xa =>
        val q = Query0[Option[String]](s"select to_iso8601_str(NULL)")
        assertEq(xa ! q.unique, None)
      }
    }

    "instant" - {
      def assertApproxEqual(a: Instant, e: Instant): Unit =
        assertEq(Duration.between(a, e).abs.minusSeconds(2).isNegative, true)

      "read" - TestDb.withImperativeXA { xa =>
        val (dbNow, i) = xa ! Query0[(Instant, Int)](s"select now(), 2").unique
        assertEq(i, 2)
        assertApproxEqual(dbNow, Instant.now())
      }

      "readSome" - TestDb.withImperativeXA { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select now(), 3").unique
        assertEq(i, 3)
        assertEq(dbNow.isDefined, true)
        assertApproxEqual(dbNow.get, Instant.now())
      }

      "readNone" - TestDb.withImperativeXA { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select null :: timestamptz, 5").unique
        assertEq(i, 5)
        assertEq(dbNow.isDefined, false)
      }

      "write" - TestDb.withImperativeXA { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2084, 5, 2, 18, 30, 8)
        val i = l.toInstant(ZoneOffset.of("+11:00"))
        xa ! Update[(Instant, Long)]("UPDATE usr SET confirmed_at=? WHERE id=?").toUpdate0((i, u.value)).run
        val s1 = xa ! Query0[String](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, "2084-05-02T07:30:08Z")
        val ai = xa ! Query0[Instant](s"select confirmed_at from usr where id=${u.value: Long}").unique
        assertEq(ai, i)
      }

      "writeOption" - TestDb.withImperativeXA { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2030, 9, 7, 20, 20, 4)
        val i = l.toInstant(ZoneOffset.of("+15:00"))
        xa ! Update[(Option[Instant], Option[Instant], Long)]("UPDATE usr SET reset_password_sent_at=?, confirmed_at=? WHERE id=?")
          .toUpdate0((None, Some(i), u.value)).run
        val s1 = xa ! Query0[Option[String]](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, Some("2030-09-07T05:20:04Z"))
        val s2 = xa ! Query0[Option[String]](s"select to_iso8601_str(reset_password_sent_at) from usr where id=${u.value: Long}").unique
        assertEq(s2, None)
      }
    }

    "user" - {
      "resetPasswordFns" - TestDb.withRealXA { xa =>
        implicit val timer = ServerInterpreter
        val dbu = DbUtil(xa)
        val db = dbu.dbAlgebra
        val u = dbu.newUserId()
        val username = dbu.getUsername(u)

        val realisticSql: Fx[VerificationToken] =
          db.withTransactionLevel(xa.trans, Connection.TRANSACTION_SERIALIZABLE)(
            db.getPasswordResetState(-\/(username)) *> db.createResetPasswordToken(u)
          )

        val token = realisticSql.unsafeRunSync()

        val tokenStatus = PublicSpaLogic.passwordResetTokenStatusFn(db, xa.trans, Global.config.server.security)
        assertEq(tokenStatus(token).unsafeRun(), VerificationToken.Status.Valid)

        xa ! db.updateResetPasswordTokenOnReissue(u)
        assertEq(tokenStatus(token).unsafeRun(), VerificationToken.Status.Valid)

        val p = PlainTextPassword("hehegreat100")
        val ps = Global.security.hashPassword(p).unsafeRun()
        xa ! db.updateUserPassword(token, ps)

        assertEq(xa ! db.getResetPasswordTokenIssueDate(token), None)
      }
    }

    "project" - {

      def createProject(u: UserId, name: String, key: ProjectEncryptionKey)(implicit xa: ImperativeXA): ProjectId = {
        val e = Event.ProjectNameSet(name)
        val p = applyEventsSuccessfully(Project.empty, e)
        xa ! DbUtil(xa).dbAlgebra.createProject(u, Vector(e), p, key)
      }

      "create" - TestDb.withImperativeXA { implicit xa =>
        val dbu = DbUtil(xa)
        val db = dbu.dbAlgebra
        val u = dbu.newUserId()
        val k = crypto.generateKey256.unsafeRun()
        val pid = xa.assertRowCountChanges("project" -> 1, "project_event" -> 1, "project_access_per_hour" -> 1) {
          createProject(u, "xxx", ProjectEncryptionKey(k.duplicate))
        }
        val pmd = xa ! db.getProjectMetaData(pid)
        assertEq(pmd.map(_.name), Some("xxx"))

        val k2 = xa ! sql"SELECT encryption_key FROM project WHERE id = $pid".query[ProjectEncryptionKey].unique
        assertEq(k2.value, k)
      }

//      "rename" - {

//        def newUserAndProject(name: String)(implicit xa: ImperativeXA) = {
//          val dbu = DbUtil(xa)
//          val db = dbu.dbAlgebra
//          val u = dbu.newUserId()
//          val pid = createProject(u, name)
//          (dbu, db, u, pid)
//        }

        // We don't bother rejecting duplicate names anymore, doesn't really matter like it does with Github.
        // This wouldn't be so trivial once multi-user support is added.
//        "reject duplicate names" - TestDb.withImperativeXA { implicit xa =>
//          val (dbu, db, u, p1) = newUserAndProject("A")
//          val p2 = createProject(u, "A")
//          dao.updateProject(p2, u, "A") ==== NameAlreadyInUse
//        }

        // This isn't checked at the DB level. It *will* be checked in ProjectSpaLogic once multi-user support is added
//        "fail when project doesnt belong to user" - TestDb.withImperativeXA { implicit xa =>
//          val (dbu, db, _, p) = newUserAndProject("A")
//          val es = (xa ! db.getAllProjectEvents(p)).getOrThrow()
//          val e = Event.ProjectNameSet("qwe")
//          val p2 = applyEventSuccessfully(Project.empty, e)
//          val u2 = dbu.newUserId()
//          val result = xa ! db.saveProjectEvent(p, es.max.ord.next, e, p2, u2)
//          result
//        }
//      }
    }

    "project_event" - {

      "prop" - {
        import IgnoreEqualityOfVerifiedEventTimestamps._
        TestDb.withImperativeXA { xa =>

          val data  = RandomEventStream.activeOnly.sampleEventStreamWithProjects
          val data1 = data.take(RandomEventStream.InitialEventCount)
          val data2 = data.drop(data1.length)
          val dbu   = DbUtil(xa)
          val db    = dbu.dbAlgebra
          val uid   = dbu.newUserId()
          val k     = ProjectEncryptionKey(crypto.generateKey256.unsafeRun())
          val pid   = xa ! db.createProject(uid, data1.map(_._1.event.active), data1.last._2, k)

          def assertPMD(expect: ProjectMetaData => ProjectMetaData)(implicit l: Line): Unit = {
            val a = (xa ! db.getProjectMetaData(pid)).get
            val e = expect(a)
            assertEq(a, e)
          }

          val read1 = (xa ! db.getAllProjectEvents(pid)).getOrThrow()
          assertEq("init event count", read1.size, data1.length)
          assertEq("first ord", read1.head.ord, EventOrd.first)
          assertPMD(a => ProjectMetaData.fromProject(data1.last._2)(
            id            = a.id,
            eventsInit    = data1.length,
            eventsTotal   = data1.length,
            createdAt     = a.createdAt,
            accessedAt    = a.accessedAt,
            lastUpdatedAt = None))

          var ord = read1.last.ord
          for ((e, p) <- data2) {
            ord = EventOrd(ord.value + 1)
            (xa ! db.saveProjectEvent(pid, ord, e.event.active, p, uid)).getOrThrow()
          }
          val readAll = (xa ! db.getAllProjectEvents(pid)).getOrThrow()
          assertSeq(readAll, data.map(_._1))
          assertPMD(a => ProjectMetaData.fromProject(data.last._2)(
            id            = a.id,
            eventsInit    = data1.length,
            eventsTotal   = data.length,
            createdAt     = a.createdAt,
            accessedAt    = a.lastUpdatedAt.get,
            lastUpdatedAt = Some(a.lastUpdatedAt.get)))
        }
      }

    }

    "visitorStats" - {
      def add(uniqueIps: Set[String], requests: Int)(implicit xa: ImperativeXA) =
        xa ! DbInterpreter.logVisitorStats(ResponseType.`1xx`, uniqueIps, requests)

      def test(expectIps: Int, expectRequests: Int)(implicit xa: ImperativeXA, l: Line): Unit = {
        val actualIps      = xa ! sql"select coalesce(#hll_union_agg(ips),0) from visitor_stats_per_hour".query[Int].unique
        val actualRequests = xa ! sql"select coalesce(sum(requests),0) from visitor_stats_per_hour".query[Int].unique
        assertEq((actualIps, actualRequests), (expectIps, expectRequests))
      }

      "startWithoutIps" - TestDb.withImperativeXA { implicit xa =>
//        xa ! DbTable.VisitorStatsPerHour.truncate
        test(0, 0)

        add(Set(), 3)
        test(0, 3)

        add(Set("a", "c"), 10)
        test(2, 13)

        add(Set(), 1)
        test(2, 14)
      }

      "startWithIps" - TestDb.withImperativeXA { implicit xa =>
        test(0, 0)

        add(Set("a", "b"), 5)
        test(2, 5)

        add(Set("a", "c"), 10)
        test(3, 15)

        add(Set(), 5)
        test(3, 20)
      }

      "responseTypes" - {
        import ResponseType._
        val tests = List(
           -1 -> Other,
            0 -> Other,
           99 -> Other,
          100 -> `1xx`,
          150 -> `1xx`,
          199 -> `1xx`,
          200 -> `2xx`,
          250 -> `2xx`,
          299 -> `2xx`,
          300 -> `3xx`,
          350 -> `3xx`,
          399 -> `3xx`,
          400 -> `4xx`,
          450 -> `4xx`,
          499 -> `4xx`,
          500 -> `5xx`,
          550 -> `5xx`,
          599 -> `5xx`,
          600 -> Other,
          650 -> Other,
          699 -> Other,
        )
        for ((i, t) <- tests)
          assertEq(t, ResponseType(i))
      }
    }

  }
}
