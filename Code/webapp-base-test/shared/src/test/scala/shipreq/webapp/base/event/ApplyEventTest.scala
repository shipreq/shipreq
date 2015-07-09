package shipreq.webapp.base.event

import shipreq.base.util.NonEmpty

import scalaz.{\/-, -\/}
import scalaz.std.anyVal.intInstance
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import DeletionAction._

object ApplyEventTest extends TestSuite {

  def emptyProject = {
    import DataImplicits._
    val rev = Rev(0)
    implicit def autoRevAnd[D](d: D): RevAnd[D] = RevAnd(rev, d)

    val cit = emptyDataMap(CustomIssueType)
    val crt = emptyDataMap(CustomReqType)
    val fs  = FieldSet(emptyDataMap(CustomField), StaticField.values.whole)
    val tt  = TagTree.empty
    val cfg = ProjectConfig(cit, crt, fs, tt)

    val reqs     = Requirements.empty
    val reqCodes = ReqCodes.empty
    val reqText  = ReqData.emptyText
    val reqTags  = ReqData.emptyTags
    val reqImps  = Implications.empty

    Project(cfg, reqs, reqCodes, reqText, reqTags, reqImps)
  }

  val apply = new ApplyEvent()(Untrusted)

  def fmtEvents(es: Seq[Event]): String = {
    val t = es.length
    es.zipWithIndex.map { case (e, i) => s"[${i + 1}/$t] $e" } mkString "\n"
  }

  def assertPass(es: Event*): Unit = _assertPass(es: _*)

  def _assertPass(es: Event*): Project = {
    val r = apply(es) run emptyProject
    val p =
      r match {
        case \/-(v) => v
        case -\/(e) => fail(s"\nPass expected but failed with '$e'.\nEvents were:\n${fmtEvents(es)}")
      }
    assertQty(p, es: _*)
    p
  }

  def assertFail(errFrag: String)(es: Event*): Unit = {
    val r = apply(es) run emptyProject
    r match {
      case -\/(e) => assert(e contains errFrag)
      case \/-(_) => fail(s"\nFailure expected but didn't occur.\nEvents were:\n${fmtEvents(es)}")
    }
  }

  def assertQty(p: Project, es: Event*): Unit = {
    var customReqTypes = 0
    def ifHard(d: DeletionAction, f: => Unit): Unit =
      if (d == HardDel) f
    es foreach {
      case _: CreateCustomReqType    => customReqTypes += 1
      case DeleteCustomReqType(_, d) => ifHard(d, customReqTypes -= 1)
      case _: UpdateCustomReqType    => ()
    }
    assertEq("CustomReqTypes", p.config.customReqTypes.data.size, customReqTypes)
  }

  override def tests = TestSuite {

    'CustomReqTypeEvents {
      import CustomReqTypeGD._
      val mfName = "Major Feature"
      val c1  = CreateCustomReqType(1, nev(Mnemonic("MF"), Name(mfName), Imp(ImplicationRequired)))
      val c2  = CreateCustomReqType(2, nev(Mnemonic("FR"), Name("Functional Req"), Imp(ImplicationRequired.Not)))
      val u1  = UpdateCustomReqType(1, nev(Mnemonic("M")))
      val sd1 = DeleteCustomReqType(1, SoftDel)
      val hd1 = DeleteCustomReqType(1, HardDel)
      val r1  = DeleteCustomReqType(1, Restore)

      def mod(e: CreateCustomReqType, f: Values => Values): CreateCustomReqType =
        e.copy(vs = NonEmpty.force(f(e.vs.value)))

      'create {
        'one      - assertPass(c1)
        'two      - assertPass(c1, c2)
        'needName - assertFail("Name")          (mod(c1, _ - Name))
        'needMne  - assertFail("Mnemonic")      (mod(c1, _ - Mnemonic))
        'needImp  - assertFail("Imp")           (mod(c1, _ - Imp))
        'badId    - assertFail(" id ")          (c1.copy(id = -1))
        'badName  - assertFail("blank")         (mod(c1, _ + Name("")))
        'badMne   - assertFail("Mnemonic")      (mod(c1, _ + Mnemonic("?")))
        'dupName  - assertFail("unique")        (c1, mod(c2, _ + Name(mfName)))
        'dupMne   - assertFail("unique")        (c1, mod(c2, _ + Mnemonic("MF")))
        'dupId    - assertFail("already exists")(c1, c2.copy(id = c1.id))
      }

      'update {
        'ok - {
          var es = Vector(c1, u1)
          def r = _assertPass(es: _*).config.customReqTypes.data.get(1).get
          assertEq(CustomReqType(1, "M", Set("MF"), mfName, ImplicationRequired, Live), r)

          es :+= UpdateCustomReqType(1, nev(Mnemonic("X"), Name("xxx")))
          assertEq(CustomReqType(1, "X", Set("MF", "M"), "xxx", ImplicationRequired, Live), r)

          es :+= UpdateCustomReqType(1, nev(Mnemonic("MF"), Imp(ImplicationRequired.Not)))
          assertEq(CustomReqType(1, "MF", Set("M", "X"), "xxx", ImplicationRequired.Not, Live), r)
        }
        'notFound - assertFail("not found")(u1)
        'dead     - assertFail("dead")     (c1, sd1, u1)
        'badName  - assertFail("blank")    (c1, UpdateCustomReqType(1, nev(Name(""))))
        'badMne   - assertFail("Mnemonic") (c1, UpdateCustomReqType(1, nev(Mnemonic("?"))))
        'dupName  - assertFail("unique")   (c1, c2, UpdateCustomReqType(2, nev(Name(mfName))))
        'dupMne   - assertFail("unique")   (c1, c2, UpdateCustomReqType(2, nev(Mnemonic("MF"))))
      }

      'delete {
        'okHard  - assertPass(c1, hd1)
        'okSoft  - assertPass(c1, sd1)
        'okRest  - assertPass(c1, sd1, r1)
        'okMulti - assertPass(c1, sd1, r1, sd1, r1, hd1)
        'notFound - List(hd1, sd1, r1).foreach(d => assertFail("not found")(d))
        'hardTwice - assertFail("not found")(c1, hd1, hd1)

        // Disabling for now. These are NOP issues, not integrity issues.
        // 'softTwice - assertFail("x")(c1, sd1, sd1)
        // 'restTwice - assertFail("x")(c1, sd1, r1, r1)
        // 'restLive  - assertFail("x")(c1, r1)
      }

    }

  }
}
