package shipreq.webapp.base.protocol

import boopickle._
import boopickle.Default.unitPickler
import java.util.concurrent.atomic.AtomicBoolean
import scalaz.Equal
import scalaz.Leibniz.===
import scalaz.std.anyVal.unitInstance
import scalaz.std.stream.streamEqual
import scalaz.syntax.equal._
import utest._
import japgolly.nyaya._
import japgolly.nyaya.test.{Gen, Settings}
import japgolly.nyaya.test.PropTest._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.util.TypeclassDerivation._
import shipreq.webapp.base.{RandomData => $}
import $.TextGenExt

object ProtocolTest extends TestSuite {

  implicit val equalityRemoteDeltaP = Equal.equal[RemoteDeltaP]((a, b) =>
    (a.partition == b.partition) &&
    (a.delete    == b.delete) &&
    (a.update    == b.update))

  implicit def equalityRemoteDeltaPR: Equal[RemoteDeltaPR] = deriveEqual

  implicit val equalityRemoteDelta = Equal.equal[RemoteDelta]((a, b) =>
    a.values.toStream ≟ b.values.toStream)

  implicit def equalProjectSPA: Equal[Routines.ProjectSPA] = {import AutoDerive._; deriveEqual}

  // -------------------------------------------------------------------------------------------------------------------

  def kitR[R <: Routine.Desc](r: R) = {
    import r.{pi, po}
    new KitIO[r.I, r.O]("Routines." + r.getClass.getSimpleName.replace("$",""))
  }

  def kitEP[I](ep: JsEntryPoint[I, Unit], name: String) = {
    import ep.pi
    new KitIO[I, Unit]("JsEntryPoint." + name)
  }

  class KitIO[I: Pickler, O: Pickler](subject: String) {
    private def c(code: String, m: Any) = s"\033[${code}m$m\033[0m"

    private type LogFmt = (String, String) => String
    private val logFmtI: LogFmt = (a, j) => s"  C ⇒ ${c("36", a)}\n    ⇒ ${c("34;1", j)} ⇒ S"
    private val logFmtO: LogFmt = (a, j) => s"  S ⇒ ${c("36", a)}\n    ⇒ ${c("34;1", j)} ⇒ C"

    def testI(is: I*)(implicit e: Equal[I]): Unit = is.foreach(testA(_, logFmtI))
    def testO(os: O*)(implicit e: Equal[O]): Unit = os.foreach(testA(_, logFmtO))

    def testIO(is: I*)(implicit e: Equal[I], ev: I === O): Unit = {
      testI(is: _*)
      testO(ev.subst(is): _*)(ev.subst(e))
    }

    def testA[A: Pickler : Equal](a: A, lf: LogFmt) = {
      val j = PickleImpl.intoBytes(a)
//      println(lf(a.toString, j))
      val b = UnpickleImpl[A].fromBytes(j)
      assertEq(b, a)
    }

    def propA[A: Pickler : Equal](lf: LogFmt, name: String) = Prop.equalSelf[A](name, {
//      val first = new AtomicBoolean(true)
      a => {
        val j = PickleImpl.intoBytes(a)
        val b = UnpickleImpl[A].fromBytes(j)
        // if (!x.settings.debug && x.run == 0)
//        if (first.compareAndSet(true, false))
//          println(lf(a.toString, j))
        b
      }
    })

    def propI(implicit e: Equal[I]) = propA[I](logFmtI, s"$subject⁻: read(write(a)) = a")
    def propO(implicit e: Equal[O]) = propA[O](logFmtO, s"$subject⁺: read(write(a)) = a")
  }


  override def tests = TestSuite {

    'Routines {
      import Routine.=>|=>
      def testCrud[I](r: Routine.Desc {type O = RemoteDelta})(g: Gen[r.I])(implicit e: Equal[r.I]): Unit =
        kitR(r).propI mustBeSatisfiedBy g

      def testUnitI[O](r: Unit =>|=> O)(g: Gen[O])(implicit e: Equal[O]): Unit =
        kitR(r).propO mustBeSatisfiedBy g

      'ProjectInit         - testUnitI(Routines.ProjectInit       )($.project)
      'CustomIssueTypeCrud - testCrud(Routines.CustomIssueTypeCrud)($.routines.customIssueTypeCrud.any)
      'CustomReqTypeCrud   - testCrud(Routines.CustomReqTypeCrud  )($.routines.customReqTypeCrud.any)
      'TagCrud             - testCrud(Routines.TagCrud            )($.routines.tagCrud.any)
      'FieldCrud           - testCrud(Routines.FieldCrud          )($.protocol.fieldCfgAction.any)
    }

    'JsEntryPoints {
      import JsEntryPoint._
      def test[I: Equal](ep: JsEntryPoint[I, Unit], name: String)(g: Gen[I]): Unit = kitEP(ep, name).propI mustBeSatisfiedBy g

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

    'Codecs {
      import BinCodecData._
      import AtomPicklers.instances._
      implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

      def test[A: Pickler : Equal](name: String, g: Gen[A]): Unit =
        g.mustSatisfy(new KitIO[A, Unit](name).propI)//(implicitly[Settings].setDebug.copy(debugMaxLen = 5000))

      'Text {
        import shipreq.webapp.base.text.Text._
        def gr = $.reqId
        def gc = $.reqCode.id
        def gi = $.customIssueTypeId
        def ga = $.applicableTagId
        'ReqCodeGroupTitle - test("ReqCodeGroupTitle", $.TextGen.reqCodeGroupTitleAtom(gr, gc, gi    ).text)
        'GenericReqTitle   - test("GenericReqTitle",   $.TextGen.genericReqTitleAtom  (gr, gc, gi, ga).text)
        'InlineIssueDesc   - test("InlineIssueDesc",   $.TextGen.inlineIssueDescAtom  (gr, gc        ).text)
        'CustomTextField   - test("CustomTextField",   $.TextGen.customTextFieldAtom  (gr, gc, gi, ga).text1(CustomTextField))
      }
    }

  }
}
