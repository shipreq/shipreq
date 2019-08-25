package shipreq.webapp.base.protocol

import java.time.Instant
import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.{Obfuscated, ProjectMetaData}
import shipreq.webapp.base.test.BinaryTestUtil._
import utest._

object HomeSpaProtocolsTest extends TestSuite {

  override def tests = Tests {

    // =================================================================================================================
    'createProject - {
      import HomeSpaProtocols.CreateProject._

      'req - {
        import ajax.req.codec

        'roundTrip - {
          val gen = Gen.chooseGen(Gen.ascii, Gen.unicode).string(0 to 4)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("363EA6420100036F6D6766251B0C")
          val expect = "omg"
          assertDecodeOk(ajax.req.codec)(bin, expect)
        }
      }

      'res - {
        import ajax.res.codec

        'roundTrip - {
          propTestRoundTrip(codec)(RandomData.projectMetaData)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("C3407BB2010008434B627850647249092432686C5577662321812C8161815FE188E05DD06601000001E7704A00")
          val expect = ProjectMetaData(Obfuscated("CKbxPdrI"), "$2hlUwf#!", 300, 353, 351, Instant.parse("2018-11-01T17:41:45.224Z"), None)
          assertDecodeOk(codec)(bin, expect)
        }
      }

    }

  }
}
