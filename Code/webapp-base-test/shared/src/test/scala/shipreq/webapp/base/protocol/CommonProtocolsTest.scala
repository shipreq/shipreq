package shipreq.webapp.base.protocol

import nyaya.gen.Gen
import scalaz.-\/
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.data.Obfuscated
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

    // =================================================================================================================
    'SubmitFeedback - {
      import CommonProtocols.SubmitFeedback._

      'req - {
        import ajax.req.codec

        'roundTrip - {
          val genOrd             = Gen.chooseInt(100000)
          val genUserInput       = Gen.string.map(UserInput)
          val genProjectMetadata = Gen.apply3(ProjectMetadata)(R.projectIdPublic, genOrd.option, genOrd.set(0 to 3))
          val genMetadata        = Gen.apply4(Metadata)(genProjectMetadata.option, Gen.string, Gen.string, R.username)
          val genRequest         = Gen.apply2(Request)(genUserInput, genMetadata)
          propTestRoundTrip(codec)(genRequest)
        }

        "v1.0" - {
          "max" - {
            val userInput       = UserInput("asd\nhehe!")
            val projectMetadata = ProjectMetadata(Obfuscated("cUZ0"), Some(9), Set(11, 12, 14))
            val metadata        = Metadata(Some(projectMetadata), "https://shipreq.com/project/cUZ0", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.88 Safari/537.36", Username("omg123"))
            val expect          = Request(userInput, metadata)
            val bin             = BinaryData.fromHex("9E5FEFC8010000096173640A68656865210002000463555A300209030B0C0E2068747470733A2F2F736869707265712E636F6D2F70726F6A6563742F63555A30684D6F7A696C6C612F352E3020285831313B204C696E7578207838365F363429204170706C655765624B69742F3533372E333620284B48544D4C2C206C696B65204765636B6F29204368726F6D652F37392E302E333934352E3838205361666172692F3533372E3336066F6D67313233C3D3FC35")
            assertDecodeOk(codec)(bin, expect)
          }

          "min" - {
            val userInput = UserInput(".")
            val metadata  = Metadata(None, "https://shipreq.com/", "", Username("poop"))
            val expect    = Request(userInput, metadata)
            val bin       = BinaryData.fromHex("9E5FEFC8010000012E00011468747470733A2F2F736869707265712E636F6D2F0004706F6F70C3D3FC35")
            assertDecodeOk(codec)(bin, expect)
          }
        }
      }

      'res - {
        import ajax.res.codec

        'roundTrip - {
          assertRoundTrip(codec)(())
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("82084269010035A0AF48")
          val expect = ()
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

  }
}