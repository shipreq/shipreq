package shipreq.webapp.base.protocol

import boopickle._
import boopickle.Default.unitPickler
import japgolly.microlibs.scalaz_ext.ScalazMacros
import scalaz.{Equal, \/, \/-}
import scalaz.Leibniz.===
import scalaz.syntax.equal._
import utest._
import nyaya.prop._
import nyaya.gen.Gen
import nyaya.test.Settings
import nyaya.test.PropTest._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.{RandomData => $}
import $.TextGenExt

object ProtocolTest extends TestSuite {

  implicit val equalProjectSpaInitData: Equal[ProjectSpaProtocols.InitData] =
    ScalazMacros.deriveEqual

  implicit val equalProjectSpaInitAsyncData: Equal[ProjectSpaProtocols.InitAsyncData] =
    ScalazMacros.deriveEqual

  // -------------------------------------------------------------------------------------------------------------------

  def kitR[I, O](r: ServerSideProc.Protocol[I, O]) = {
    import r._
    new KitIO[I, O]("Routines." + r.getClass.getSimpleName.replace("$",""))
  }

  def kitCF[I](c: ClientSideProc[I], name: String) = {
    import c.pickler
    new KitIO[I, Unit]("ClientFnDecl." + name)
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

  override def tests = Tests {

    // TODO Add more procs

    'ServerSideProcs {
      type CrudFn[I] = ServerSideProc.Protocol[I, ErrorMsg \/ VerifiedEvent.Seq]

      def testCrud[I](r: CrudFn[I])(g: Gen[r.Input])(implicit e: Equal[r.Input]): Unit =
        kitR(r).propI mustBeSatisfiedBy g

      def testUnitI[O](r: ServerSideProc.Protocol[Unit, ErrorMsg \/ O])(g: Gen[O])(implicit e: Equal[O]): Unit =
        kitR(r).propO mustBeSatisfiedBy g.map[ErrorMsg \/ O](\/-(_))

      'InitAsync           - testUnitI(ProjectSpaProtocols.InitAsync         )($.routines.projectSpaInitAsyncData)
      'CustomIssueTypeCrud - testCrud(ProjectSpaProtocols.CustomIssueTypeCrud)($.routines.customIssueTypeCrud.any)
      'CustomReqTypeCrud   - testCrud(ProjectSpaProtocols.CustomReqTypeCrud  )($.routines.customReqTypeCrud.any)
      'TagCrud             - testCrud(TagCrud.Protocol                       )($.routines.tagCrud.any)
      'FieldCrud           - testCrud(FieldCrud.Protocol                     )($.protocol.fieldCfgAction.any)
    }

    'ClientSideProcs {
      def test[I: Equal](ep: ClientSideProc[I], name: String)(g: Gen[I]): Unit =
        kitCF(ep, name).propI mustBeSatisfiedBy g

      'ProjectSpa - test(ProjectSpaProtocols.EntryPoint, "ProjectSpa")($.routines.projectSpa)
    }

    'Codecs {
      import BinCodecMemberData._
      import BinCodecMemberData.ReqTableDataPicklers._
      import AtomPicklers.instances._
      implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

      def test[A: Pickler : Equal](name: String, g: Gen[A]): Unit =
        g.mustSatisfy(new KitIO[A, Unit](name).propI)(implicitly[Settings].setSampleSize(20))
//        g.mustSatisfy(new KitIO[A, Unit](name).propI)//(implicitly[Settings].setDebug.copy(debugMaxLen = 5000))
//        g.mustSatisfy(new KitIO[A, Unit](name).propI)(implicitly[Settings].setDebug.copy(seed = Some(0L)))

      'ReqTableData {
//        'SortCriterionC - test("SortCriterionC", $.reqtableData.sortCriteriaC)
//        'SortCriteria - test("SortCriteria", $.project.flatMap($.reqtableData.visibleColumns).flatMap($.reqtableData.sortCriteria(_)))
//        'ValidFilter - test("ValidFilter", $.project.flatMap($.filter.valid.forProject))
        'SavedViews - test("SavedViews", $.project.flatMap($.reqtableData.nonEmptySavedViewsForProject))
      }

      'Text {
        import shipreq.webapp.base.text.Text._
        def gr = $.reqId
        def gu = $.useCaseStepId
        def gc = $.reqCode.id
        def gi = $.customIssueTypeId
        def ga = $.applicableTagId
        'CodeGroupTitle  - test("CodeGroupTitle" , $.TextGen.codeGroupTitleAtom (gr, gu, gc, gi    ).text)
        'GenericReqTitle - test("GenericReqTitle", $.TextGen.genericReqTitleAtom(gr, gu, gc, gi, ga).text)
        'InlineIssueDesc - test("InlineIssueDesc", $.TextGen.inlineIssueDescAtom(gr, gu, gc        ).text)
        'CustomTextField - test("CustomTextField", $.TextGen.customTextFieldAtom(gr, gu, gc, gi, ga).text1(CustomTextField))
      }
    }

  }
}
