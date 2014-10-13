package shipreq.webapp.shared.protocol

import scalaz.Leibniz.===
import utest._
import upickle._
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.RandomData
import shipreq.webapp.shared.prop._
import shipreq.webapp.shared.TestUtil._
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

    private type LogFmt = (String, String) => String
    private val logFmtI: LogFmt = (a, j) => s"  C ⇒ ${c("36", a)}\n    ⇒ ${c("34;1", j)} ⇒ S"
    private val logFmtO: LogFmt = (a, j) => s"  S ⇒ ${c("36", a)}\n    ⇒ ${c("34;1", j)} ⇒ C"

    def testI(is: I*): Unit = is.foreach(testA(_, logFmtI))
    def testO(os: O*): Unit = os.foreach(testA(_, logFmtO))

    def testIO(is: I*)(implicit ev: I === O): Unit = {
      testI(is: _*)
      testO(ev.subst(is): _*)
    }

    def testA[A: Reader : Writer](a: A, lf: LogFmt) = {
      val j = write(a)
      println(lf(a.toString, j))
      val b = read[A](j)
      assert(b == a)
    }

    def propA[A: Reader : Writer](lf: LogFmt) = Prop.withCtx[A]{ x =>
      import x.a
      val j = write(a)
      val b = read[A](j)
      if (!x.settings.debug && x.run == 0)
        println(lf(a.toString, j))
      b == a
    }

    def propI = propA[I](logFmtI)
    def propO = propA[O](logFmtO)
  }

  override def tests = TestSuite {

    'Routines {
      'CustomReqTypeOps {
        val prop = kitR(Routines.CustomReqTypeCrud).propI
        import RandomData.routines.customReqTypeCrud._
        'create { create _mustSatisfy prop }
        'update { update _mustSatisfy prop }
        'delete { delete _mustSatisfy prop }
//        'manual - {
//          import CustomReqType.Id, ReqType.Mnemonic
//          import Routines._
//          val d = CustomReqTypeCrud.create((Mnemonic("KQMBFJ"),
//            "\u001e\u587f\u1611\u523f\u9d9f\uf969\u73a1\uadd6\u3eae\u5f22\u9d6f\u3abf\u0c0d\uef1d\u1628\ud968\u93a0\ud8a3\uef1d\u1628\ud968\u93a0\ud8a3"
//            ,ImplicationRequired))
//          kitR(Routines.CustomReqTypeCrud).testI(d)
//        }
      }
    }

    'JsEntryPoints {
      import JsEntryPoint._, RandomData.routines._
      'reactExamples { kitEP(reactExamples).propI mustBeSatisfiedBy forCfgReqType }
    }

    'Δ {
      val prop = kitR(Routines.CustomReqTypeCrud).propO
      def test(p: Partition) = RandomData.remoteDelta forPart p mustSatisfy prop
      'CustomReqTypes - test(Partition.CustomReqTypes)
    }
  }
}
