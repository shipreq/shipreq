package shipreq.webapp.server.logic

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.univeq.UnivEqScalaz._
import nyaya.gen.Gen
import scalaz.Equal
import shipreq.webapp.base.{RandomData => R}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.event.EventEquality._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.base.util.BinaryData
import utest._

object RedisProtocolTest extends TestSuite {

  private implicit val equalProjectSnapshot: Equal[Redis.ProjectSnapshot] =
    ScalazMacros.deriveEqual

  override def tests = Tests {

    // =================================================================================================================

    'event - {
      def codec = RedisProtocol.picklerEvent

      'roundTrip - {
        propTestRoundTrip(codec)(R.events.verifiedEvent)
      }

      "v1.0" - {
        'ManualIssueCreate - {
          val bin    = BinaryData.fromHex("010081F41C7B016C046F6D6667")
          val expect = VerifiedEvent(500, Event.ManualIssueCreate(123, "omfg"))
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

    // =================================================================================================================

    'projectSnapshot - {
      import Redis.ProjectSnapshot

      def codec = RedisProtocol.picklerProjectSnapshot

      'roundTrip - {
        val gen: Gen[ProjectSnapshot] =
          for {
            p <- R.project
            o <- Gen.chooseInt(10000)
          } yield ProjectSnapshot(p, o)
        propTestRoundTrip(codec)(gen)
      }

      "v1.0" - {
        'empty - {
          val bin    = BinaryData.fromHex("5C303D7101000000000004494E4547000000000000000000000000010100000000000000007BDEC22AB7")
          val expect = ProjectSnapshot(Project.empty, 123)
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

  }
}
