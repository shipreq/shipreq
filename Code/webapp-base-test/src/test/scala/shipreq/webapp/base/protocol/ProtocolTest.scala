package shipreq.webapp.base.protocol

import java.util.concurrent.atomic.AtomicBoolean
import scalaz.Leibniz.===
import utest._
import upickle._
import upickle.Fns._
import upickle.BaseCodecs.UnitRW
import japgolly.nyaya._
import japgolly.nyaya.test.{Gen, Settings}
import japgolly.nyaya.test.PropTest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.{RandomData => $}
import $.TextGenExt

object ProtocolTest extends TestSuite {

  implicit def equality[A] = scalaz.Equal.equalA[A]

  def kitR[R <: Routine.Desc](r: R) = {
    import r.{ri, wi, ro, wo}
    new KitIO[r.I, r.O]("Routines." + r.getClass.getSimpleName.replace("$",""))
  }

  def kitEP[I](ep: JsEntryPoint[I, Unit], name: String) = {
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

    'Codecs {
      import DataCodecs._
      implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

      def test[A: Reader : Writer](name: String, g: Gen[A]): Unit =
        g.mustSatisfy(new KitIO[A, Unit](name).propI)//(implicitly[Settings].setDebug.copy(debugMaxLen = 5000))

      'Text {
        import shipreq.webapp.base.text.Text._
        'RecCodeGroupTitle - test("ReqCodeGroupTitle", $.TextGen.reqCodeGroupTitleAtom($.reqId, $.customIssueTypeId                   ).text)
        'GenericReqTitle   - test("GenericReqTitle",   $.TextGen.genericReqTitleAtom  ($.reqId, $.customIssueTypeId                   ).text)
        'InlineIssueDesc   - test("InlineIssueDesc",   $.TextGen.inlineIssueDescAtom  ($.reqId                                        ).text)
        'CustomTextField   - test("CustomTextField",   $.TextGen.customTextFieldAtom  ($.reqId, $.customIssueTypeId, $.applicableTagId).text1(CustomTextField))
      }
    }

    'Routines {
      def testCrud(r: Routine.Desc {type O = RemoteDelta})(g: Gen[r.I]): Unit = kitR(r).propI mustBeSatisfiedBy g
      def testUnitI(r: Routine.Desc {type I = Unit})(g: Gen[r.O]): Unit = kitR(r).propO mustBeSatisfiedBy g

      'ProjectInit         - testUnitI(Routines.ProjectInit       )($.project)
      'CustomIssueTypeCrud - testCrud(Routines.CustomIssueTypeCrud)($.routines.customIssueTypeCrud.any)
      'CustomReqTypeCrud   - testCrud(Routines.CustomReqTypeCrud  )($.routines.customReqTypeCrud.any)
      'TagCrud             - testCrud(Routines.TagCrud            )($.routines.tagCrud.any)
      'FieldCrud           - testCrud(Routines.FieldCrud          )($.protocol.fieldCfgAction.any)
    }

    'JsEntryPoints {
      import JsEntryPoint._
      def test[I](ep: JsEntryPoint[I, Unit], name: String)(g: Gen[I]): Unit = kitEP(ep, name).propI mustBeSatisfiedBy g

      'reactExamples - test(reactExamples, "reactExamples")($.routines.projectSPA)
    }

    'Δ {
      val prop = kitR(Routines.CustomReqTypeCrud).propO
      def test(p: Partition) = $.remoteDelta forPart confirmTest(p) mustSatisfy prop

      // This just spits out a compiler warning to remind you to add a manual test here
      def confirmTest(p: Partition) = p match {
        case Partition.CustomIssueTypes => p
        case Partition.CustomReqTypes   => p
        case Partition.Fields           => p
        case Partition.Tags             => p
      }
      'CustomIssueTypes - test(Partition.CustomIssueTypes)
      'CustomReqTypes   - test(Partition.CustomReqTypes)
      'Fields           - test(Partition.Fields)
      'Tags             - test(Partition.Tags)
    }
  }
}
