package shipreq.webapp.base.protocol

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.scalaz_ext.ScalazMacros
import nyaya.gen.Gen
import scalaz.{-\/, Equal}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{BinaryData, ErrorMsg}
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.event.EventEquality._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import utest._

object ProjectSpaProtocolsTest extends TestSuite {
  import ProjectSpaProtocols._

  private val webSocket = WebSocket("fake_project_id")

  private implicit def univEqWsReq: UnivEq[WsReqRes.AndReq] = UnivEq.force
  private implicit val equalProjectAndOrd: Equal[ProjectAndOrd] = ScalazMacros.deriveEqual
  private implicit val equalInitAppData: Equal[InitAppData] = ScalazMacros.deriveEqual

  override def tests = Tests {

    // =================================================================================================================
    "InitApp" - {
      'req - {
        import webSocket.req.codec
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D01000038295653")
          val expect = WsReqRes.InitApp.AndReq(())
          assertDecodeOk(codec)(bin, expect)
        }
      }

      'res - {
        import WsReqRes.InitApp.protocolRes.codec

        "v1.0" - {

        // TODO test success

        "error" - {
          val bin    = BinaryData.fromHex("0100010A6F6D66672066697265213B301988")
          val expect = -\/(ErrorMsg("omfg fire!"))
          assertDecodeOk(codec)(bin, expect)
        }
        }
      }
    }

    // =================================================================================================================
//    "Reconnect" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
    "Sync" - {
      'req - {
        import webSocket.req.codec
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D01000203070B0338295653")
          val expect = WsReqRes.Sync.AndReq(NonEmptySet(3, 7, 11))
          assertDecodeOk(codec)(bin, expect)
        }
      }

      'res - {
        import WsReqRes.Sync.protocolRes.codec
        "v1.0" - {
          val bin    = BinaryData.fromHex("0100")
          val expect = ()
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

    // =================================================================================================================
//    "UpdateConfig" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "CreateContent" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "UpdateContent" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "ProjectNameSet" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "UpdateSavedViews" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "UpdateManualIssues" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "FieldMandatorinessMod" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
//    "ReqTypeImplicationMod" - {
//      'req - {
//      }
//
//      'res - {
//      }
//    }

    // =================================================================================================================
    'push {
      import webSocket.push.codec

      "v1.0" - {

        "GenericReqTitleSet" - {
          val bin    = BinaryData.fromHex("010083E81A0009016C04626C616800060CF606")
          val expect = Event.GenericReqTitleSet(9, "blah").verified(1000)
          assertDecodeOk(codec)(bin, expect)
        }

        "ReqTagsPatch" - {
          val bin    = BinaryData.fromHex("0100816224670016020004004601001300060CF606")
          val expect = Event.ReqTagsPatch(22, nesd[ApplicableTagId](4, 70)(19)).verified(354)
          assertDecodeOk(codec)(bin, expect)
        }

      }
    }

  }
}
