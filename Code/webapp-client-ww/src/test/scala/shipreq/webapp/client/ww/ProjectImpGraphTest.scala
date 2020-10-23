package shipreq.webapp.client.ww

import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.GraphDir
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.CompiledFilter
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.ww.GraphViz.DOT
import utest._

object ProjectImpGraphTest extends TestSuite {
  import GraphTestUtil._
  import SampleImplicationGraph._, Values._

  private def render(fd: FilterDead,
                     p: Project,
                     filter: CompiledFilter = null,
                     config: ImpGraphConfig = ImpGraphConfig.default): DOT =
    new ProjectImpGraph(
      project    = p,
      plainText  = PlainText.ForProject.noCtx(p),
      filterDead = fd,
      _scope     = ImpGraphConfig.buildReqWhitelist(fd, Option(filter), p),
      config     = config,
    ).dot

  override def tests = Tests {

    // webapp-client-ww/testOnly -- shipreq.webapp.client.ww.ProjectImpGraphTest.extractDot
//    "extractDot" - {
//      val dot = render(ShowDead, SampleProject3.project)
//      println(s"\n\n${dot.content}\n\n")
//    }

    "basic" - {
      val actual = render(HideDead, SIG_dead_FR7)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr6[id="FR-6" label="FR-6"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |$mf4->$fr6[id="MF-4--FR-6"];
           |$br1->$mf2[id="BR-1--MF-2"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$mf3->$mf4[id="MF-3--MF-4"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "backwards" - {
      val actual = render(HideDead, SIG_dead_FR7, config = ImpGraphConfig.default.copy(GraphDir.RightToLeft))
      // rankdir=LR below is deliberate and correct because all the links have been run backwards
      // See https://shipreq.com/project/d6My#/reqs/FR-131
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=LR;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"][dir=back]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr6[id="FR-6" label="FR-6"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |$fr6->$mf4[id="MF-4--FR-6"];
           |$mf2->$br1[id="BR-1--MF-2"];
           |$br2->$br1[id="BR-1--BR-2"];
           |$mf4->$mf3[id="MF-3--MF-4"];
           |$fr4->$mf3[id="MF-3--FR-4"];
           |$mf3->$br2[id="BR-2--MF-3"];
           |$fr2->$mf2[id="MF-2--FR-2"];
           |$mf5->$fr5[id="FR-5--MF-5"];
           |$fr2->$fr1[id="FR-1--FR-2"];
           |$fr1->$mf1[id="MF-1--FR-1"];
           |$fr3->$fr2[id="FR-2--FR-3"];
           |$fr5->$fr4[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "impRequired" - {
      val p = applyEventSuccessfully(SIG_dead_FR7, TestEvent.genericReqCreate(GenericReqId(999), fr))
      val actual = render(HideDead, p)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |999[id="FR-8" label="FR-8"]
           |$fr6[id="FR-6" label="FR-6"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |{
           |edge[color="#dd0000"]
           |R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label="?"]
           |R->999;
           |}
           |
           |$mf4->$fr6[id="MF-4--FR-6"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$br1->$mf2[id="BR-1--MF-2"];
           |$mf3->$mf4[id="MF-3--MF-4"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "impRequiredFilteredOut" - {
      val id = GenericReqId(999)
      val p = applyEventSuccessfully(SIG_dead_FR7, TestEvent.genericReqCreate(id, fr))
      val f = CompiledFilter.empty.copy(req = OptionalBoolFn(_.id != id))
      val actual = render(HideDead, p, f)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |$fr6[id="FR-6" label="FR-6"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |$mf4->$fr6[id="MF-4--FR-6"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$br1->$mf2[id="BR-1--MF-2"];
           |$mf3->$mf4[id="MF-3--MF-4"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "impRequiredFiltered2" - {
      val x1 = GenericReqId(666)
      val x2 = GenericReqId(777)
      val scope = Set[ReqId](x1, x2)
      val p = applyEventsSuccessfully(SIG_dead_FR7,
        TestEvent.genericReqCreate(x1, fr, impTgts = Set1(br1)),
        TestEvent.genericReqCreate(x2, fr, impSrcs = Set1(br1)),
      )
      val f = CompiledFilter.empty.copy(req = OptionalBoolFn(r => scope.contains(r.id)))
      val actual = render(HideDead, p, f)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |node[fillcolor="#D5A8C9"]
           |777[id="FR-9" label="FR-9"]
           |666[id="FR-8" label="FR-8"]
           |
           |{
           |edge[color="#dd0000"]
           |R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label="?"]
           |R->666;
           |}
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "showDead" - {
      val actual = render(ShowDead, SIG_dead_FR7_MF4)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr7[id="FR-7" label="FR-7" fillcolor="#dddddd" color="#777777" fontcolor="#666666"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr6[id="FR-6" label="FR-6"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4" fillcolor="#dddddd" color="#777777" fontcolor="#666666"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |$mf4->$fr6[id="MF-4--FR-6"][color="#bbbbbb" style=dashed];
           |$br1->$mf2[id="BR-1--MF-2"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$mf3->$mf4[id="MF-3--MF-4"][color="#bbbbbb" style=dashed];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "hideDead" - {
      val actual = render(HideDead, SIG_dead_FR7_MF4)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr6[id="FR-6" label="FR-6"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |$br1->$mf2[id="BR-1--MF-2"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "v1.1" - {

      "wedged" - {
        import ImpGraphConfig._
        import SampleProject8.Values._
        import SampleProject8._

        val fd = HideDead
        val filter = needFilter("-MF-{10-27}", fd)

        val cfg = ImpGraphConfig(
          GraphDir.LeftToRight,
          LabelFormat.Pubid,
          Colours.ByTag(priTG),
        )
        val actual = render(fd, project, filter, cfg)
        val expect = DOT(
          raw"""
               |digraph G{
               |bgcolor=transparent;
               |rankdir=LR;
               |node[style=filled shape=ellipse color="#222222"]edge[color="#222222"]
               |
               |1401[id="BR-1" label="BR-1" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1402[id="BR-2" label="BR-2" style=wedged fillcolor="#ff0000:#123456" fontcolor="#ffffff"]
               |1403[id="BR-3" label="BR-3" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1001[id="FR-1" label="FR-1" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
               |1002[id="FR-2" label="FR-2" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
               |1101[id="MF-1" label="MF-1" style=filled fillcolor="#ff0000" fontcolor="#ffffff"]
               |1102[id="MF-2" label="MF-2" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1103[id="MF-3" label="MF-3" style=filled fillcolor="#ff0000" fontcolor="#ffffff"]
               |1104[id="MF-4" label="MF-4" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1105[id="MF-5" label="MF-5" style=filled fillcolor="#ff0000" fontcolor="#ffffff"]
               |1106[id="MF-6" label="MF-6" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1107[id="MF-7" label="MF-7" style=filled fillcolor="#ff0000" fontcolor="#ffffff"]
               |1108[id="MF-8" label="MF-8" style=filled fillcolor="#123456" fontcolor="#ffffff"]
               |1109[id="MF-9" label="MF-9" style=filled fillcolor="#ff0000" fontcolor="#ffffff"]
               |1203[id="UC-1" label="UC-1" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
               |1204[id="UC-2" label="UC-2" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
               |
               |1001->1002[id="FR-1--FR-2"];
               |1101->1002[id="MF-1--FR-2"];
               |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "striped" - {
        import ImpGraphConfig._
        import SampleProject8.Values._
        import SampleProject8._

        val fd = HideDead
        val filter = needFilter("-MF-{10-27}", fd)

        val cfg = ImpGraphConfig(
          GraphDir.LeftToRight,
          LabelFormat.PubidAndTitle,
          Colours.ByTag(priTG),
        )
        val actual = render(fd, project, filter, cfg)
        val expect = DOT(
          raw"""
               |digraph G{
               |bgcolor=transparent;
               |rankdir=LR;
               |node[style="rounded,filled" shape=box color="#222222"]edge[color="#222222"]
               |
               |1401[id="BR-1" label="BR-1\nMust make moneh" style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1402[id="BR-2" label="BR-2\nMust make moar moneh" style="rounded,striped" fillcolor="#ff0000:#123456" fontcolor="#ffffff"]
               |1403[id="BR-3" label="BR-3\nCEO owns islands!" style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1001[id="FR-1" label="FR-1\njapgolly@gmail.com is on https://github.com cos of\n[MF-6]\n#TODO<tex>c = \\pm\\sqrt{a^2 + b^2}</tex>"][style="rounded,filled" fillcolor="#0076f5" fontcolor="#ffffff"]
               |1002[id="FR-2" label="FR-2\n#TBD{ Pending\n[MF-26]\n}.\n[MF-28]\nis dead."][style="rounded,filled" fillcolor="#0076f5" fontcolor="#ffffff"]
               |1101[id="MF-1" label="MF-1\nUse Case Editor"][style="rounded,filled" fillcolor="#ff0000" fontcolor="#ffffff"]
               |1102[id="MF-2" label="MF-2\nAnonymous Share"][style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1103[id="MF-3" label="MF-3\nExport (PDF, XLS)"][style="rounded,filled" fillcolor="#ff0000" fontcolor="#ffffff"]
               |1104[id="MF-4" label="MF-4\nTemplates"][style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1105[id="MF-5" label="MF-5\nField Customisation"][style="rounded,filled" fillcolor="#ff0000" fontcolor="#ffffff"]
               |1106[id="MF-6" label="MF-6\nIncompletions"][style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1107[id="MF-7" label="MF-7\nOrganisation"][style="rounded,filled" fillcolor="#ff0000" fontcolor="#ffffff"]
               |1108[id="MF-8" label="MF-8\nHistory/Audit"][style="rounded,filled" fillcolor="#123456" fontcolor="#ffffff"]
               |1109[id="MF-9" label="MF-9\nCollaboration: authoring"][style="rounded,filled" fillcolor="#ff0000" fontcolor="#ffffff"]
               |1203[id="UC-1" label="UC-1\n\n[UC-1.0.X.1]\nand\n[UC-1.E.X.1]\nare dead.\n[UC-1.0.2.a]\nand\n[UC-1.E.1]\nare not."][style="rounded,filled" fillcolor="#0076f5" fontcolor="#ffffff"]
               |1204[id="UC-2" label="UC-2\nEmpty for now"][style="rounded,filled" fillcolor="#0076f5" fontcolor="#ffffff"]
               |
               |1001->1002[id="FR-1--FR-2"];
               |1101->1002[id="MF-1--FR-2"];
               |}
          """.stripMargin)
        assertDOT(actual, expect)
      }
    }

    "impRequired" - {
      import UnsafeTypes._
      import AutoNES._
      import SampleProject.Values._
      val GD = GenericReqGD

      // br3
      // fr7 -> fr8
      //     ↘
      //       fr9 -> br2
      // br1 ↗
      val br3: GenericReqId = 23
      val fr8: GenericReqId = 38
      val fr9: GenericReqId = 39
      val fr10: GenericReqId = 40
      val p = applyEventsSuccessfully(project,
        GenericReqCreate(br3, br, GD.emptyValues),
        GenericReqCreate(fr8, fr, GD.emptyValues + GD.ImpSrcs(fr7)),
        GenericReqCreate(fr9, fr, GD.emptyValues + GD.ImpSrcs(NonEmptySet(br1, fr7)) + GD.ImpTgts(br2)),
        GenericReqCreate(fr10, fr, GD.emptyValues),
        deleteReqs(fr10))

      // TODO also confirm dead fr with no imp doesn't stem from R

      val actual = render(ShowDead, p)
      val expect = DOT(
        s"""
           |digraph G{bgcolor=transparent;rankdir=TB;
           |node[style=filled shape=ellipse color="#222222"]
           |edge[color="#222222"]
           |
           |node[fillcolor="#B7D058"]
           |$br2[id="BR-2" label="BR-2"]
           |$br1[id="BR-1" label="BR-1"]
           |$br3[id="BR-3" label="BR-3"]
           |
           |node[fillcolor="#D5A8C9"]
           |$fr7[id="FR-7" label="FR-7"]
           |$fr9[id="FR-9" label="FR-9"]
           |$fr5[id="FR-5" label="FR-5"]
           |$fr1[id="FR-1" label="FR-1"]
           |$fr6[id="FR-6" label="FR-6"]
           |$fr8[id="FR-8" label="FR-8"]
           |$fr3[id="FR-3" label="FR-3"]
           |$fr2[id="FR-2" label="FR-2"]
           |$fr4[id="FR-4" label="FR-4"]
           |$fr10[id="FR-10" label="FR-10"][fillcolor="#dddddd" color="#777777" fontcolor="#666666"]
           |
           |node[fillcolor="#93D5BA"]
           |$mf4[id="MF-4" label="MF-4"]
           |$mf3[id="MF-3" label="MF-3"]
           |$mf2[id="MF-2" label="MF-2"]
           |$mf1[id="MF-1" label="MF-1"]
           |$mf5[id="MF-5" label="MF-5"]
           |
           |{edge[color="#dd0000"]
           |R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label="?"]
           |R->$fr7;
           |R->$fr8;
           |$fr7->$fr9[id="FR-7--FR-9"];
           |$fr7->$fr8[id="FR-7--FR-8"];
           |}
           |
           |$mf4->$fr6[id="MF-4--FR-6"];
           |$br1->$br2[id="BR-1--BR-2"];
           |$br1->$mf2[id="BR-1--MF-2"];
           |$br1->$fr9[id="BR-1--FR-9"];
           |$mf3->$mf4[id="MF-3--MF-4"];
           |$mf3->$fr4[id="MF-3--FR-4"];
           |$br2->$mf3[id="BR-2--MF-3"];
           |$mf2->$fr2[id="MF-2--FR-2"];
           |$fr9->$br2[id="FR-9--BR-2"];
           |$fr5->$mf5[id="FR-5--MF-5"];
           |$fr1->$fr2[id="FR-1--FR-2"];
           |$mf1->$fr1[id="MF-1--FR-1"];
           |$fr2->$fr3[id="FR-2--FR-3"];
           |$fr4->$fr5[id="FR-4--FR-5"];
           |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

  }
}
