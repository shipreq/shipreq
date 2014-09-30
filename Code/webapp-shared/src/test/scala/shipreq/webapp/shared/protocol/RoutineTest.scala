package shipreq.webapp.shared.protocol

import scalaz.Leibniz.===
import utest._
import upickle._
import shipreq.webapp.shared.data._
import Codecs._

object RoutineTest extends TestSuite {
  override def tests = TestSuite {

    class Kit[R <: Routine.Desc](r: R) {
      import r.{ri, wi, ro, wo, I, O}

      private def c(code: String, m: Any) = s"\033[${code}m$m\033[0m"

      def testI(i: I, is: I*): Unit =
        (i +: is).foreach(testA(_, (a,j) => s"  C ⇒ ${c("36",a)} ⇒ ${c("34;1",j)} ⇒ S"))

      def testO(o: O, os: O*): Unit =
        (o +: os).foreach(testA(_, (a,j) => s"  S ⇒ ${c("36",a)} ⇒ ${c("34;1",j)} ⇒ C"))
//        (o +: os).foreach(testA(_, (a,j) => s"  C ⇐ ${c("34;1",j)} ⇐ ${c("36",a)} ⇐ S"))

      def testIO(i: I, is: I*)(implicit ev: I === O): Unit = {
        testI(i, is: _*)
        testO(ev(i), ev.subst(is): _*)
      }

      def testA[A : Reader:Writer](a: A, f: (String, String) => String) = {
        val j = write(a)
        println(f(a.toString, j))
        val b = read[A](j)
        assert(b == a)
      }
    }

    'CustReqType {
      // TODO this is bullshit, need properties :(
      def id = CustReqType.Id(5)
      def mn = ReqType.Mnemonic("BR")
      def mn2 = ReqType.Mnemonic("X")
      def mn3 = ReqType.Mnemonic("Y")
      val s = "hehe"
      def c1 = CustReqType(id, mn, Set(mn2, mn3), s, ImplicationRequired, Dead)

      'Update {
        val kit = new Kit(Routines.CustReqTypeUpdate)
        'i {kit.testI( (id,mn,s,ImplicationRequired) )}
        'o {kit.testO(None, Some(c1))}
      }
    }
  }
}
