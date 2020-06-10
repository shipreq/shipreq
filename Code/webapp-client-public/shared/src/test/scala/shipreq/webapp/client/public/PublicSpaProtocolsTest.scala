package shipreq.webapp.client.public

import japgolly.microlibs.adt_macros.AdtMacros
import nyaya.gen.Gen
import scalaz.{-\/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.{RandomData => R}
import shipreq.webapp.base.data.VerificationToken
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.user.{EmailAddr, PersonName, PlainTextPassword, Username}
import utest._

object PublicSpaProtocolsTest extends TestSuite {

  override def tests = Tests {

    // =================================================================================================================
    "LandingPage" - {
      import PublicSpaProtocols.LandingPage._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = for {
              a <- R.personName
              b <- R.emailAddr
              c <- R.shortText.option
              d <- Gen.boolean
            } yield Request(a, b, c, d)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("F54CAEB501000A5D7348444F32236F7826036A6B560101F2A28F22")
          val expect = Request(PersonName("]sHDO2#ox&"),EmailAddr("jkV"),None,true)
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          val gen = R.errorMsg \/ Gen.unit
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          "ok" - {
            val bin    = BinaryData.fromHex("D903D77C010000E3D5C6B2")
            val expect = \/-(())
            assertDecodeOk(codec)(bin, expect)
          }
          "ko" - {
            val bin    = BinaryData.fromHex("D903D77C0100010175E3D5C6B2")
            val expect = -\/(ErrorMsg("u"))
            assertDecodeOk(codec)(bin, expect)
          }
        }
      }
    }

    // =================================================================================================================
    "Register1" - {
      import PublicSpaProtocols.Register1._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = R.emailAddr
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("90758289010006354C3367643A58F85888")
          val expect = EmailAddr("5L3gd:")
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          val gen = R.errorMsg \/ Gen.unit
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          "ok" - {
            val bin    = BinaryData.fromHex("3232CE0F01000024423A71")
            val expect = \/-(())
            assertDecodeOk(codec)(bin, expect)
          }
          "ko" - {
            val bin    = BinaryData.fromHex("3232CE0F0100010420685B7124423A71")
            val expect = -\/(ErrorMsg(" h[q"))
            assertDecodeOk(codec)(bin, expect)
          }
        }
      }
    }

    // =================================================================================================================
    "Register2" - {
      import PublicSpaProtocols.Register2._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = for {
              a <- R.verificationToken
              b <- R.personName
              c <- R.username
              d <- R.plainTextPassword
              e <- Gen.boolean
            } yield Request(a, b, c, d, e)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("184A6C450100052473212B76017A067234656A64390454407B6201381B6074")
          val expect = Request(VerificationToken("$s!+v"),PersonName("z"),Username("r4ejd9"),PlainTextPassword("T@{b"),true)
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          val gens: Vector[Gen[Response]] = AdtMacros.adtValues[Result].whole.map(r => Gen.pure(\/-(r))) :+ R.errorMsg.map(-\/(_))
          val gen = Gen.chooseGen_!(gens)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("1259E49F0100000009AEDD6F")
          val expect = \/-(Result.Success)
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "ResetPassword1" - {
      import PublicSpaProtocols.ResetPassword1._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = R.username \/ R.emailAddr
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("AA85FE1E0100011D776470746B5F5F746773633135675F71696A69635F775F676775783031C77C0643")
          val expect = -\/(Username("wdptk__tgsc15g_qijic_w_ggux01"))
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          assertRoundTrip(codec)(())
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("89FBF9FE0100CF148631")
          val expect = ()
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "ResetPassword2" - {
      import PublicSpaProtocols.ResetPassword2._

      "req" - {
        import ajax.req.codec

        "roundTrip" - {
          val gen = Gen.apply2(Request)(R.verificationToken, R.plainTextPassword)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("EE434A020100054D492747400D7A545B68483D75593928587477826CAE63")
          val expect = Request(VerificationToken("MI'G@"),PlainTextPassword("zT[hH=uY9(Xtw"))
          assertDecodeOk(codec)(bin, expect)
        }
      }

      "res" - {
        import ajax.res.codec

        "roundTrip" - {
          val gens: Vector[Gen[Response]] = AdtMacros.adtValues[Result].whole.map(r => Gen.pure(\/-(r))) :+ R.errorMsg.map(-\/(_))
          val gen = Gen.chooseGen_!(gens)
          propTestRoundTrip(codec)(gen)
        }

        "v1.0" - {
          val bin    = BinaryData.fromHex("1282CBB901000000AD0146DA")
          val expect = \/-(Result.Success)
          assertDecodeOk(codec)(bin, expect)
        }
      }
    }

  }
}