package shipreq.webapp.base.protocol

import java.util.concurrent.atomic.AtomicBoolean
import scalaz.Leibniz.===
import utest._
import upickle._
import upickle.Fns._
import upickle.BaseCodecs.UnitRW
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.RandomData
import shipreq.prop._
import shipreq.prop.test.PropTest._

object ProtocolTest extends TestSuite {

  implicit def equality[A] = scalaz.Equal.equalA[A]

  def kitR[R <: Routine.Desc](r: R) = {
    import r.{ri, wi, ro, wo}
    new KitIO[r.I, r.O]("Routines." + r.getClass.getSimpleName.replace("$",""))
  }

  def kitEP[I](ep: JsEntryPoint[I, _], name: String) = {
    import ep.{ri, wi}
    new KitIO[I, Unit]("JsEntryPoint." + name)
  }

  class KitIO[I: Reader : Writer, O: Reader : Writer](subject: String) {
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

    def propA[A: Reader : Writer](lf: LogFmt, name: String) = Prop.equalSelf[A](name, {
      val first = new AtomicBoolean(true)
      a => {
        val j = write(a)
        val b = read[A](j)
        // if (!x.settings.debug && x.run == 0)
        if (first.compareAndSet(true, false))
          println(lf(a.toString, j))
        b
      }
    })

    def propI = propA[I](logFmtI, s"$subject⁻: read(write(a)) = a")
    def propO = propA[O](logFmtO, s"$subject⁺: read(write(a)) = a")
  }

  override def tests = TestSuite {

    'Routines {
      'ProjectInit {
        val prop = kitR(Routines.ProjectInit).propO
        RandomData.project mustSatisfy prop
      }

      // TODO copy/paste/search/replace again
      'CustomIssueTypeCrud {
        val prop = kitR(Routines.CustomIssueTypeCrud).propI
        import RandomData.routines.customIssueTypeCrud._
        'create { create _mustSatisfy prop }
        'update { update _mustSatisfy prop }
        'delete { delete _mustSatisfy prop }
      }

      'CustomReqTypeCrud {
        val prop = kitR(Routines.CustomReqTypeCrud).propI
        import RandomData.routines.customReqTypeCrud._
        'create { create _mustSatisfy prop }
        'update { update _mustSatisfy prop }
        'delete { delete _mustSatisfy prop }
      }

      'TagCrud {
        val prop = kitR(Routines.TagCrud).propI
        import RandomData.routines.tagCrud._
        'create { create _mustSatisfy prop }
        'update { update _mustSatisfy prop }
        'delete { delete _mustSatisfy prop }
      }
    }

    'JsEntryPoints {
      import JsEntryPoint._, RandomData.routines._
      'reactExamples { kitEP(reactExamples, "reactExamples").propI mustBeSatisfiedBy projectSPA }
    }

    'Δ {
      val prop = kitR(Routines.CustomReqTypeCrud).propO
      def test(p: Partition) = RandomData.remoteDelta forPart confirmTest(p) mustSatisfy prop

      // This just spits out a compiler warning to remind you to add a manual test here
      def confirmTest(p: Partition) = p match {
        case Partition.CustomIssueTypes => p
        case Partition.CustomReqTypes   => p
        case Partition.Tags             => p
      }
      'CustomIssueTypes - test(Partition.CustomIssueTypes)
      'CustomReqTypes   - test(Partition.CustomReqTypes)
      'Tags             - test(Partition.Tags)
    }
  }
}
