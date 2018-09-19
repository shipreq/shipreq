package shipreq.webapp.server.logic

import scalaz.{\/-, -\/}
import utest._
import shipreq.base.util.PotentialChange._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Text
import AutoNES._
import SampleProject.Values._

object MakeEventTest extends TestSuite {

  class Tester(initial: Project = SampleProject.project) {
    private var p = initial

    import MakeEvent._

    def assertMakeEvent[E <: ActiveEvent](f: (MakeEvent.type, Project) => Result, u: PartialFunction[ActiveEvent, E]): E =
      f(MakeEvent, p) match {
        case Success(e) if u isDefinedAt e => u(e)
        case Success(e)                    => fail(s"Doesn't meet expected criteria: $e")
        case x                             => fail(s"MakeEvent failed: $x")
      }

    def assertMakeEventFails(f: (MakeEvent.type, Project) => Result): Unit =
      f(MakeEvent, p) match {
        case Failure(_) => ()
        case x          => fail(s"MakeEvent failure expected, got: $x")
      }

    def assertFails(f: (MakeEvent.type, Project) => Result): Unit =
      f(MakeEvent, p) match {
        case Failure(_) => ()
        case Success(e) =>
          ApplyEvent.untrusted.apply1(e)(p) match {
            case -\/(_) => ()
            case \/-(_) => fail(s"Failure expected, instead created and applied: $e")
          }
        case x          => fail(s"Failure expected, got: $x")
      }

    def assertApplies[E <: ActiveEvent](e: E): E = {
      ApplyEvent.untrusted.apply1(e)(p) match {
        case \/-(p2)  => p = p2; e
        case -\/(err) => fail(s"ApplyEvent failed.\nEvent: $e\nReason: $err")
      }
    }
  }

  override def tests = Tests {
    val t = new Tester()
    import t._

    'CreateCodeGroup {
      import CreateContentCmd.{CreateCodeGroup => Cmd}
      def apply(cmd: Cmd) =
        assertApplies(assertMakeEvent(_.createContent(cmd, _), {case e: CodeGroupCreate => e}))

      // Vacant code
      val cmd1 = Cmd("some.code", ∅)
      val e1 = apply(cmd1)
      assertEq(e1.id, ReqCodeId(1))

      // Code in use
      assertMakeEventFails(_.createContent(cmd1, _))

      // Vacant code. Reference first code
      val cmd2 = Cmd("some", Vector(Text.CodeGroupTitle.CodeRef(1)))
      val e2 = apply(cmd2)
      assertEq(e2.id, ReqCodeId(2))

      // Vacant code
      val cmd3 = Cmd("some.code.child", ∅)
      val e3 = apply(cmd3)
      assertEq(e3.id, ReqCodeId(3))

      // Should reuse ID on restore
      assertApplies(CodeGroupsDelete(e1.id))
      val e1b = apply(cmd1)
      assertEq(e1b.id, e1.id)
    }

    'CreateGenericReq {
      import CreateContentCmd.{CreateGenericReq => Cmd}
      def apply(cmd: Cmd) =
        assertApplies(assertMakeEvent(_.createContent(cmd, _), {case e: GenericReqCreate => e}))

      // OK
      val cmd1 = Cmd.empty(mf).copy(codes = Set("hello"))
      val e1 = apply(cmd1)
      assertEq(e1.id, GenericReqId(1))

      // Code in use
      assertFails(_.createContent(cmd1, _))

      // OK
      val cmd2 = Cmd.empty(mf).copy(codes = Set("hello.2"))
      val e2 = apply(cmd2)
      assertEq(e2.id, GenericReqId(2))
    }

    // TODO More MakeEvent tests would be good (esp for PatchReqCodes)

  }
}
