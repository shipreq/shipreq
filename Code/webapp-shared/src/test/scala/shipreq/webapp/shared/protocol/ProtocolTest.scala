package shipreq.webapp.shared.protocol

import scalaz.Leibniz.===
import utest._
import upickle._
import shipreq.webapp.shared.data._
import Routine.Remote, Routines._

object ProtocolTest extends TestSuite {

  def kitR[R <: Routine.Desc](r: R) = {
    import r.{ri, wi, ro, wo}
    new KitIO[r.I, r.O]
  }

  def kitEP[I](ep: JsEntryPoint[I, _]) = {
    import ep.{ri, wi}
    new KitIO[I, Unit]
  }

  class KitIO[I: Reader : Writer, O: Reader : Writer] {
    private def c(code: String, m: Any) = s"\033[${code}m$m\033[0m"

    def testI(is: I*): Unit =
      is.foreach(testA(_, (a, j) => s"  C ⇒ ${c("36", a)} ⇒ ${c("34;1", j)} ⇒ S"))

    def testO(os: O*): Unit =
      os.foreach(testA(_, (a, j) => s"  S ⇒ ${c("36", a)} ⇒ ${c("34;1", j)} ⇒ C"))

    def testIO(is: I*)(implicit ev: I === O): Unit = {
      testI(is: _*)
      testO(ev.subst(is): _*)
    }

    def testA[A: Reader : Writer](a: A, f: (String, String) => String) = {
      val j = write(a)
      println(f(a.toString, j))
      val b = read[A](j)
      assert(b == a)
    }
  }

  override def tests = TestSuite {

    'Routines {

      'CustReqTypeOps {
        // TODO this is bullshit, need properties :(
        def id = CustReqType.Id(5)
        def mn = ReqType.Mnemonic("BR")
        def mn2 = ReqType.Mnemonic("X")
        def mn3 = ReqType.Mnemonic("Y")
        val s = "hehe"
        def c1 = CustReqType(id, mn, Set(mn2, mn3), s, ImplicationRequired, Dead)

        'Create {
          val kit = kitR(Routines.CustReqTypeOps.Create)
          'i - kit.testI( (mn,s,ImplicationRequired) )
          'o - kit.testO(None, Some(c1))
        }
        'Update {
          val kit = kitR(Routines.CustReqTypeOps.Update)
          'i - kit.testI( (id,(mn,s,ImplicationRequired)) )
          'o - kit.testO(None, Some(c1))
        }
      }
    }

    'JsEntryPoints {
      import JsEntryPoint._

      'reactExamples {
        import CustReqTypeOps._
        kitEP(reactExamples).testI(ForCfgReqType(
          Remote("x", Create),
          Remote("e", Update),
          Remote("f", SoftDelete),
          Remote("h", HardDelete),
          Remote("o", Restore)
        ))
      }
    }
  }
}
