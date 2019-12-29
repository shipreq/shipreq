package shipreq.webapp.base.protocol

import nyaya.gen.Gen
import scalaz.-\/
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.user.{PlainTextPassword, Username}
import shipreq.webapp.base.{RandomData => R}

object CommonProtocolsTest extends TestSuite {

  override def tests = Tests {

    // =================================================================================================================
    'Login - {
      import CommonProtocols.Login._

      'req - {
        import ajax.req.codec

        'roundTrip - {
          val gen = Gen.apply2(Request.apply)(R.username \/ R.emailAddr, R.plainTextPassword)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("D1DAB08A0100010F637061357134773434646235687969044B6625326119E238")
          val expect = Request(-\/(Username("cpa5q4w44db5hyi")),PlainTextPassword("Kf%2"))
          assertDecodeOk(codec)(bin, expect)
        }
      }

      'res - {
        import ajax.res.codec

        'roundTrip - {
          assertRoundTrip(codec)(Allow)
          assertRoundTrip(codec)(Deny)
        }

        "v1.0" - {
          'allow - {
            val bin    = BinaryData.fromHex("35BED9BA01000171ECACBC")
            val expect = Allow
            assertDecodeOk(codec)(bin, expect)
          }
          'deny - {
            val bin    = BinaryData.fromHex("35BED9BA01000071ECACBC")
            val expect = Deny
            assertDecodeOk(codec)(bin, expect)
          }
        }
      }
    }

  }
}