package shipreq.webapp.member.protocol.ajax

import java.time.Instant
import nyaya.gen.Gen
import shipreq.base.util.BinaryData
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data.ProjectMetaData
import shipreq.webapp.member.test.project.RandomData
import utest._

object HomeSpaProtocolsTest extends TestSuite {

  override def tests = Tests {

    // =================================================================================================================
    "createProject" - {
      import HomeSpaProtocols.CreateProject._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = Gen.chooseGen(Gen.ascii, Gen.unicode).string(0 to 4)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("363EA6420100036F6D6766251B0C")
          val expect = "omg"
          assertDecodeOk(ajax.req.codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          propTestRoundTrip(codec)(RandomData.projectMetaData)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("C3407BB2010008434B627850647249092432686C557766232102816180F0815FE0D93ADB5B362FC92DE0EB3ADB5BE63E331F01E7704A00")
          val expect = ProjectMetaData(Obfuscated("CKbxPdrI"), "$2hlUwf#!", 2, 353, 240, 351, Instant.ofEpochSecond(1541094105L, 768159542), Instant.ofEpochSecond(1541094123L, 523452134), None)
          assertDecodeOk(codec)(bin, expect)
        }
      }

    }

  }
}
