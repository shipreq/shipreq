package shipreq.webapp.server.logic

import scalaz.{-\/, \/-}
import shipreq.base.util.PotentialChange._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.Text
import sourcecode.Line
import utest._

object MakeEventTest extends TestSuite {
  import AutoNES._
  import Event._
  import SampleProject.Values._

  class Tester(initial: Project = SampleProject.project) {
    private var p = initial

    def project() = p

    import MakeEvent._

    def assertMakeEvent[E <: ActiveEvent](f: (MakeEvent.type, Project) => Result, u: PartialFunction[ActiveEvent, E])(implicit l: Line): E =
      f(MakeEvent, p) match {
        case Success(e) if u isDefinedAt e => u(e)
        case Success(e)                    => fail(s"Doesn't meet expected criteria: $e")
        case x                             => fail(s"MakeEvent failed: $x")
      }

    def assertMakeEventFails(f: (MakeEvent.type, Project) => Result)(implicit l: Line): Unit =
      f(MakeEvent, p) match {
        case Failure(_) => ()
        case x          => fail(s"MakeEvent failure expected, got: $x")
      }

    def assertNoChange(f: (MakeEvent.type, Project) => Result)(implicit l: Line): Unit =
      f(MakeEvent, p) match {
        case Unchanged => ()
        case x         => fail(s"Unchanged expected, got: $x")
      }

    def assertFails(f: (MakeEvent.type, Project) => Result)(implicit l: Line): Unit =
      f(MakeEvent, p) match {
        case Failure(_) => ()
        case Success(e) =>
          ApplyEvent.untrusted.apply1(e)(p) match {
            case -\/(_) => ()
            case \/-(_) => fail(s"Failure expected, instead created and applied: $e")
          }
        case x          => fail(s"Failure expected, got: $x")
      }

    def assertApplies[E <: ActiveEvent](e: E)(implicit l: Line): E = {
      ApplyEvent.untrusted.apply1(e)(p) match {
        case \/-(p2)  => p = p2; e
        case -\/(err) => fail(s"ApplyEvent failed.\nEvent: $e\nReason: $err")
      }
    }
  }

  override def tests = Tests {
    val t = new Tester()
    import t._

    "CreateCodeGroup" - {
      import CreateContentCmd.{CreateCodeGroup => Cmd}
      def apply(cmd: Cmd) =
        assertApplies(assertMakeEvent(_.createContent(cmd, _), {case e: CodeGroupCreate => e}))

      // Vacant code
      val cmd1 = Cmd("some.code", ∅)
      val e1 = apply(cmd1)
      assertEq(e1.id, ReqCodeGroupId(1))

      // Code in use
      assertMakeEventFails(_.createContent(cmd1, _))

      // Vacant code. Reference first code
      val cmd2 = Cmd("some", Text.CodeGroupTitle(Text.CodeGroupTitle.CodeRef(ReqCodeGroupId(1))))
      val e2 = apply(cmd2)
      assertEq(e2.id, ReqCodeGroupId(2))

      // Vacant code
      val cmd3 = Cmd("some.code.child", ∅)
      val e3 = apply(cmd3)
      assertEq(e3.id, ReqCodeGroupId(3))

      // Should reuse ID on restore
      assertApplies(CodeGroupsDelete(e1.id))
      val e1b = apply(cmd1)
      assertEq(e1b.id, e1.id)
    }

    "CreateGenericReq" - {
      import CreateContentCmd.{CreateGenericReq => Cmd}
      def applyCmd(cmd: Cmd)(implicit l: Line) =
        assertApplies(assertMakeEvent(_.createContent(cmd, _), {case e: GenericReqCreate => e}))

      // OK
      val cmd1 = Cmd.empty(mf).copy(codes = Set("hello"))
      val e1 = applyCmd(cmd1)
      assertEq(e1.id, GenericReqId(1))

      // Code in use
      assertFails(_.createContent(cmd1, _))

      // OK
      val cmd2 = Cmd.empty(mf).copy(codes = Set("hello.2"))
      val e2 = applyCmd(cmd2)
      assertEq(e2.id, GenericReqId(2))
    }

    // TODO More MakeEvent tests would be good (esp for PatchReqCodes)

    "UpdateConfigCmd" - {

      "TagSetLiveChildrenOrder" - {
        import UpdateConfigCmd.{TagSetLiveChildrenOrder => Cmd}
        val ^ = TagGroupGD

        "mismatch" - assertFails(_.updateConfig(Cmd(priTG, Vector(priHigh, priLow)), _))

        "noop" - assertNoChange(_.updateConfig(Cmd(priTG, Vector(priHigh, priMed, priLow)), _))

        "allLive" - {
          val c = Vector(priMed, priHigh, priLow)
          val e = assertMakeEvent(_.updateConfig(Cmd(priTG, c), _), { case e: TagGroupUpdate => e })
          assertEq(e.vs, ^.nev(^.ValueForChildren(c)))
        }

        "someDead" - {
          assertApplies(TagGroupUpdate(priTG, ^.nev(^.ValueForChildren(Vector(priMed, priHigh, priLow, statusTG)))))
          assertApplies(TagDelete(priLow))
          assertApplies(TagDelete(statusTG))
          val c = Vector(priHigh, priMed)
          val e = assertMakeEvent(_.updateConfig(Cmd(priTG, c), _), { case e: TagGroupUpdate => e })
          assertEq(e.vs, ^.nev(^.ValueForChildren(Vector(priLow, statusTG) ++ c)))
        }
      }

      "ApplicableTagUpdate"  - {
        import UpdateConfigCmd.{ApplicableTagUpdate => Cmd}
        val ^ = ApplicableTagGD

        def applyCmd(cmd: Cmd)(implicit l: Line) =
          assertApplies(assertMakeEvent(_.updateConfig(cmd, _), {case e: ApplicableTagUpdate => e}))

        "noop"         - assertNoChange(_.updateConfig(Cmd(priLow, ^.ValueForColour(None)), _))
        "children"     - assertFails(_.updateConfig(Cmd(priLow, ^.ValueForChildren(Vector(priMed))), _))
        "deadReqTypes" - assertFails(_.updateConfig(Cmd(priLow, ^.ValueForApplicableReqTypes(onlyReqTypes(dd))), _))

        "applicableReqTypes" - {
          val tag = priMed
          def applicableReqTypes() = project().config.tags.needApplicableTag(tag).applicableReqTypes

          assertApplies(CustomReqTypeRestore(dd))
          assertApplies(CustomReqTypeRestore(si))

          applyCmd(Cmd(tag, ^.ValueForApplicableReqTypes(onlyReqTypes(dd, mf, fr))))
          assertApplies(CustomReqTypeDeleteSoft(mf))
          assertApplies(CustomReqTypeDeleteSoft(dd))
          assertEq(applicableReqTypes(), onlyReqTypes(dd, mf, fr))

          applyCmd(Cmd(tag, ^.ValueForApplicableReqTypes(onlyReqTypes(si))))
          assertEq(applicableReqTypes(), onlyReqTypes(dd, mf, si))

          assertApplies(CustomReqTypeRestore(dd))
          applyCmd(Cmd(tag, ^.ValueForApplicableReqTypes(onlyReqTypes(br))))
          assertEq(applicableReqTypes(), onlyReqTypes(mf, br))

          applyCmd(Cmd(tag, ^.ValueForApplicableReqTypes(notReqTypes(si))))
          assertEq(applicableReqTypes(), notReqTypes(si))
        }

        "parents"  - {
          val tag = priMed
          def parents() = project().config.tags.parents(tag)

          assertApplies(ApplicableTagUpdate(tag, ^.ValueForParents(Map(priTG -> ∅, statusTG -> ∅))))
          assertApplies(TagDelete(priTG))

          applyCmd(Cmd(tag, ^.ValueForParents(Map(verTG -> ∅))))
          assertEq(parents(), Map(priTG -> ∅, verTG -> ∅): TagInTree.Parents)

          assertApplies(TagRestore(priTG))
          applyCmd(Cmd(tag, ^.ValueForParents(Map(statusTG -> ∅))))
          assertEq(parents(), Map(statusTG -> ∅): TagInTree.Parents)
        }

        "deadParent" - {
          assertApplies(TagDelete(statusTG))
          assertFails(_.updateConfig(Cmd(priLow, ^.ValueForParents(Map(statusTG -> None))), _))
        }
      }

      "TagGroupUpdate"  - {
        import UpdateConfigCmd.{TagGroupUpdate => Cmd}
        val ^ = TagGroupGD

        def applyCmd(cmd: Cmd)(implicit l: Line) =
          assertApplies(assertMakeEvent(_.updateConfig(cmd, _), {case e: TagGroupUpdate => e}))

        "noop" - assertNoChange(_.updateConfig(Cmd(priTG, ^.ValueForDesc(None)), _))

        "children" - {
          val tag = relTG
          def children() = project().config.tags.directChildren(tag)

          assertApplies(TagGroupUpdate(tag, ^.ValueForChildren(Vector(priTG, statusTG, priHigh))))
          assertApplies(TagDelete(statusTG))
          assertApplies(TagDelete(priHigh))

          applyCmd(Cmd(tag, ^.ValueForChildren(Vector(priLow))))
          assertEq(children(), Vector(statusTG, priHigh, priLow))

          assertApplies(TagRestore(statusTG))
          assertApplies(TagRestore(priHigh))
          applyCmd(Cmd(tag, ^.ValueForChildren(Vector(priTG))))
          assertEq(children(), Vector(priTG))
        }

        // 'deadChildren

        "parents"  - {
          val tag = relTG
          def parents() = project().config.tags.parents(tag)

          assertApplies(TagGroupUpdate(tag, ^.ValueForParents(Map(priTG -> ∅, statusTG -> ∅))))
          assertApplies(TagDelete(priTG))

          applyCmd(Cmd(tag, ^.ValueForParents(Map(verTG -> ∅))))
          assertEq(parents(), Map(priTG -> ∅, verTG -> ∅): TagInTree.Parents)

          assertApplies(TagRestore(priTG))
          applyCmd(Cmd(tag, ^.ValueForParents(Map(statusTG -> ∅))))
          assertEq(parents(), Map(statusTG -> ∅): TagInTree.Parents)
        }

        "deadParent" - {
          assertApplies(TagDelete(statusTG))
          assertFails(_.updateConfig(Cmd(relTG, ^.ValueForParents(Map(statusTG -> None))), _))
        }
      }

    }
  }
}
