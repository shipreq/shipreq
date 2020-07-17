package shipreq.webapp.server.logic

import japgolly.microlibs.scalaz_ext.ScalazMacros
import java.time.Instant
import nyaya.gen.Gen
import scalaz.Equal
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.{Project, StaticField}
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.{RandomData => R}
import utest._

object RedisProtocolTest extends TestSuite {

  private implicit val equalProjectSnapshot: Equal[Redis.ProjectSnapshot] =
    ScalazMacros.deriveEqual

  override def tests = Tests {

    // webapp-server-logic-jvm/testOnly -- shipreq.webapp.server.logic.RedisProtocolTest.generateTestData
//    "generateTestData" - {
//      shipreq.webapp.base.RandomDataSettings.disableUnicode = true
//      RedisProtocolTestData.main(Array.empty)
//    }

    "saved" - {
      def run(ver: Int, assertSnapshot: Boolean = true) = {
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

      "v10" - run(0, false) // Snapshot differs now cos OtherTags has been added to Project.empty and thus the result
      "v11" - run(1)
      "v12" - run(2)
      "v13" - run(3)
      "v14" - run(4)
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
      import Redis.ProjectSnapshot

      def codec = RedisProtocol.picklerProjectSnapshot

      "roundTrip" - {
        val gen: Gen[ProjectSnapshot] =
          for {
            p <- R.project
            o <- Gen.chooseInt(10000)
          } yield ProjectSnapshot(p, o)
        propTestRoundTrip(codec)(gen)
      }

      "v1.0" - {
        "empty" - {
          val bin        = BinaryData.fromHex("5C303D7101000000000004494E4547000000000000000000000000010100000000000000007BDEC22AB7")
          val oldProject = applyEventsSuccessfully(Project.empty, Event.FieldStaticRemove(StaticField.OtherTags))
          val expect     = ProjectSnapshot(oldProject, 123)
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "v1.1" - {
        "empty" - {
          val bin    = BinaryData.fromHex("5C303D710101000000000523494E4547000000000000000000000000010100000000000000007BDEC22AB7")
          val expect = ProjectSnapshot(Project.empty, 123)
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

  }
}
