package shipreq.webapp.server.logic.protocol

import cats.Eq
import japgolly.microlibs.cats_ext.CatsMacros
import java.time.Instant
import nyaya.gen.Gen
import shipreq.base.util.BinaryData
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._
import shipreq.webapp.member.test.project.{RandomData => R}
import shipreq.webapp.server.logic.algebra.Redis
import sourcecode.Line
import utest._

object RedisProtocolTest extends TestSuite {

  private implicit val equalProjectSnapshot: Eq[Redis.ProjectSnapshot] =
    CatsMacros.deriveEq

  override def tests = Tests {

    // webappServerLogicJVM/testOnly -- shipreq.webapp.server.logic.protocol.RedisProtocolTest.generateTestData
//    "generateTestData" - {
//      shipreq.webapp.base.test.RandomDataSettings.disableUnicode = true
//      RedisProtocolTestData.main(Array.empty)
//    }

    "saved" - {
      def run(ver: Int, assertSnapshot: Boolean = true)(implicit l: Line) = {
        val rows = RedisProtocolTestData.load(ver)

        var prev: Option[Redis.ProjectSnapshot] = None
        for (i <- rows.indices) {
          def prefix = s"[${i+1}/${rows.length}]"
          val row    = rows(i)
          val event  = row.parseEventJson.getOrThrow()
          val ord    = prev.fold(EventOrd.first)(x => EventOrd(x.ord.value) + 1)
          val p1     = prev.fold(Project.empty)(_.project)
          val p2     = applyVerifiedEventSuccessfully(p1, event)
          val ps     = Redis.ProjectSnapshot(p2, ord.asLatest)
          prev       = Some(ps)
          assertEq(s"$prefix ${row.eventJson.noSpaces}", row.parseEventBinary, \/-(event))
          if (assertSnapshot)
            assertEq(prefix, row.parseSnapshotBinary, \/-(ps))
        }
      }

      "v20" - run(0)
    }

    // =================================================================================================================

    "event" - {
      def codec = RedisProtocol.picklerEvent

      "roundTrip" - {
        propTestRoundTrip(codec)(R.events.verifiedEvent)
      }

      "v1.0" - {
        "ManualIssueCreate" - {
          val bin    = BinaryData.fromHex("010081F41C7B016C046F6D6667E0E57B8D5D00E66307")
          val expect = VerifiedEvent(500, Event.ManualIssueCreate(123, "omfg"), Instant.ofEpochSecond(1569553381, 123987456))
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

    // =================================================================================================================

    "projectSnapshot" - {
      import shipreq.webapp.server.logic.algebra.Redis.ProjectSnapshot

      def codec = RedisProtocol.picklerProjectSnapshot

      "roundTrip" - {
        val gen: Gen[ProjectSnapshot] =
          for {
            p <- R.projectNonsenseHistory
          } yield ProjectSnapshot(p, p.history.ord.get)
        propTestRoundTrip(codec)(gen)
      }

      "v2.0" - {
        "empty" - {
          val bin    = BinaryData.fromHex("5C303D710200000000000523494E454700000000000000000000000001010000000000000000000000DEC22AB7")
          val expect = ProjectSnapshot(Project.empty, 0)
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

  }
}
