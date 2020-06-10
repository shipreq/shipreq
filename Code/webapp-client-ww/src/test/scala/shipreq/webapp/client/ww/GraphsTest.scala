package shipreq.webapp.client.ww

import japgolly.microlibs.nonempty.NonEmptySet
import sourcecode.Line
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.CompiledFilter
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{PlainText, ProjectText, Text}
import Event._
import GraphViz.DOT
import WebappTestUtil._

object GraphsTest extends TestSuite {

  val _normalise = raw"""([\]{};])""".r

  def normaliseDOT(d: DOT): String =
    _normalise.replaceAllIn(d.content, "$1\n")
      .replace("]\n[", " ")
      .replace("][", " ")

  def assertDOT(actual: DOT, expect: DOT)(implicit l: Line): Unit = {
    val expect2 = expect.content
      .trim
      .replaceAll("\\s*(?:<!:)//[^\r\n]+", "")
      .replaceAll("\n *", "")
      .replaceAll("(?:GenericReqId|UseCaseId)\\((\\d+)\\)", "$1")
    val e = normaliseDOT(DOT(expect2))
    val a = normaliseDOT(actual)
    if (a != e) {
      println()
      println(actual.content)
      println()
      assertMultiline(actual = a, expect = e)
    }
  }

  def deleteReqs(id: ReqId*) =
    ReqsDelete(NonEmptySet force id.toSet, Set.empty, Text.empty)

  lazy val SIG_deadMF4: Project =
    applyEventsSuccessfully(SampleImplicationGraph.project, deleteReqs(SampleImplicationGraph.mf4))

  private def implicationAll(fd: FilterDead,
                             p: Project,
                             filter: CompiledFilter = null,
                             config: ImpGraphConfig = ImpGraphConfig.default): DOT =
    Graphs.implicationAll(
      project    = p,
      plainText  = PlainText.ForProject.noCtx(p),
      filterDead = fd,
      scope      = ImpGraphConfig.buildReqWhitelist(fd, Option(filter), p),
      config     = config,
    )

  override def tests = Tests {

    "stepFlow" - {
      // TODO Test Graphs.useCaseStepFlow with more complicated flow

      "init" - {
        import UnsafeTypes._
        val uc = UseCaseId(1)
        val project = applyEventsSuccessfully(Project.empty, UseCaseCreate(uc, 2, UseCaseGD.emptyValues))
        val actual = Graphs.useCaseStepFlow(uc, project, ProjectText.Context.Req(uc))
        val expect = DOT(
          """
            |digraph G{bgcolor=transparent;rankdir=LR;ranksep=0.28;
            |
            |S[shape=circle tooltip=Start style=filled color=black fontsize=1 height=.3]
            |E[shape=doublecircle tooltip=End style=filled color=black fontsize=1 height=.3]
            |
            |{node[fillcolor=lawngreen style=filled shape=invhouse]
            |  2[label="1.0" tooltip="<blank>"]
            |}
            |
            |{edge[weight=9]S->2->E;}
            |
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "sp6" - {
        import SampleProject6._, Values._
        val actual = Graphs.useCaseStepFlow(uc1, project, ProjectText.Context.Req(uc1))
        val expect = DOT(
          """
            |digraph G{bgcolor=transparent;rankdir=LR;ranksep=0.28;
            |
            |S[shape=circle tooltip=Start style=filled color=black fontsize=1 height=.3]
            |E[shape=doublecircle tooltip=End style=filled color=black fontsize=1 height=.3]
            |
            |{node[fillcolor=lawngreen style=filled shape=invhouse]
            |  10[label="1.0" tooltip="[1.0.X.1] and [1.E.X.1] are dead. [1.0.2.a] and [1.E.1] are not."]
            |}
            |{node[fillcolor=lawngreen style=filled shape=ellipse]
            |  11[label="1.0.1" tooltip="Get food"]
            |  12[label="1.0.2" tooltip="Put in mouth"]
            |  19[label="1.0.2.a" tooltip="<blank>"]
            |  13[label="1.0.3" tooltip="Still hungry?"]
            |}
            |
            |{node[fillcolor=skyblue style=filled shape=invhouse]
            |  14[label="1.1" tooltip="Have no food"]
            |}
            |{node[fillcolor=skyblue style="filled,rounded" shape=box]
            |  15[label="1.1.1" tooltip="Steal food"]
            |}
            |
            |{node[fillcolor=tomato style=filled shape=octagon]
            |  18[label="1.E.1" tooltip="<blank>"]
            |}
            |
            |{edge[weight=9]S->10->11->12->19->13->E;}
            |S->14->15;
            |S->18->E;
            |
            |15->12;
            |13->11;
            |
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    "implicationFocused" - {
      "basic" - {
        import SampleImplicationGraph._
        val actual = Graphs.implicationFocused(mf3, HideDead, project)
        val expect = DOT(
          """
            |digraph G{bgcolor=transparent;rankdir=LR;
            |node[style=filled shape=ellipse color="#222222"]
            |
            |F[style=bold fillcolor="#cccccc" label="MF-3"]
            |
            |node[fillcolor="#FFEDE2"]
            |21[id="BR-1" label="BR-1"]
            |
            |{rank=same;node[fillcolor="#FFC19C"]
            |22[id="BR-2" label="BR-2"]
            |}
            |
            |{rank=same;node[fillcolor="#7692B7" fontcolor=white]
            |14[id="MF-4" label="MF-4"]
            |34[id="FR-4" label="FR-4"]
            |}
            |
            |node[fillcolor="#D6E1EF"]
            |36[id="FR-6" label="FR-6"]
            |35[id="FR-5" label="FR-5"]
            |15[id="MF-5" label="MF-5"]
            |
            |edge[color="#FFC19C"]
            |21->22;
            |edge[color="#C27040"]
            |22->F;
            |edge[color="#31537F"]
            |F->14;
            |F->34;
            |edge[color="#7692B7"]
            |14->36;
            |34->35;
            |35->15;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "hideDead" - {
        import SampleImplicationGraph._
        val actual = Graphs.implicationFocused(mf3, HideDead, SIG_deadMF4)
        val expect = DOT(
          """
            |digraph G{bgcolor=transparent;rankdir=LR;
            |node[style=filled shape=ellipse color="#222222"]
            |
            |F[style=bold fillcolor="#cccccc" label="MF-3"]
            |
            |node[fillcolor="#FFEDE2"]
            |21[id="BR-1" label="BR-1"]
            |
            |{rank=same;node[fillcolor="#FFC19C"]
            |22[id="BR-2" label="BR-2"]
            |}
            |
            |{rank=same;node[fillcolor="#7692B7" fontcolor=white]
            |34[id="FR-4" label="FR-4"]
            |}
            |
            |node[fillcolor="#D6E1EF"]
            |35[id="FR-5" label="FR-5"]
            |15[id="MF-5" label="MF-5"]
            |
            |edge[color="#FFC19C"]
            |21->22;
            |edge[color="#C27040"]
            |22->F;
            |edge[color="#31537F"]
            |F->34;
            |edge[color="#7692B7"]
            |34->35;
            |35->15;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "showDead" - {
        import SampleImplicationGraph._
        val actual = Graphs.implicationFocused(mf3, ShowDead, SIG_deadMF4)
        val expect = DOT(
          """
            |digraph G{bgcolor=transparent;rankdir=LR;
            |node[style=filled shape=ellipse color="#222222"]
            |
            |F[style=bold fillcolor="#cccccc" label="MF-3"]
            |
            |node[fillcolor="#FFEDE2"]
            |21[id="BR-1" label="BR-1"]
            |
            |{rank=same;node[fillcolor="#FFC19C"]
            |22[id="BR-2" label="BR-2"]
            |}
            |
            |{rank=same;node[fillcolor="#7692B7" fontcolor=white]
            |14[id="MF-4" label="MF-4"][fillcolor="#dddddd" color="#777777" fontcolor="#666666"]
            |34[id="FR-4" label="FR-4"]
            |}
            |
            |node[fillcolor="#D6E1EF"]
            |36[id="FR-6" label="FR-6"]
            |35[id="FR-5" label="FR-5"]
            |15[id="MF-5" label="MF-5"]
            |
            |edge[color="#FFC19C"]
            |21->22;
            |edge[color="#C27040"]
            |22->F;
            |edge[color="#31537F"]
            |F->14[color="#bbbbbb" style=dashed];
            |F->34;
            |edge[color="#7692B7"]
            |14->36[color="#bbbbbb" style=dashed];
            |34->35;
            |35->15;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    "implicationAll" - {
      import SampleImplicationGraph._

      "basic" - {
        val actual = implicationAll(HideDead, project)
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
            |$mf4->$fr6;
            |$br1->$mf2;
            |$br1->$br2;
            |$mf3->$mf4;
            |$mf3->$fr4;
            |$br2->$mf3;
            |$mf2->$fr2;
            |$fr5->$mf5;
            |$fr1->$fr2;
            |$mf1->$fr1;
            |$fr2->$fr3;
            |$fr4->$fr5;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "showDead" - {
        val actual = implicationAll(ShowDead, SIG_deadMF4)
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
            |$mf4[id="MF-4" label="MF-4"][fillcolor="#dddddd" color="#777777" fontcolor="#666666"]
            |$mf3[id="MF-3" label="MF-3"]
            |$mf2[id="MF-2" label="MF-2"]
            |$mf1[id="MF-1" label="MF-1"]
            |$mf5[id="MF-5" label="MF-5"]
            |
            |$mf4->$fr6[color="#bbbbbb" style=dashed];
            |$br1->$mf2;
            |$br1->$br2;
            |$mf3->$fr4;
            |$mf3->$mf4[color="#bbbbbb" style=dashed];
            |$br2->$mf3;
            |$mf2->$fr2;
            |$fr5->$mf5;
            |$fr1->$fr2;
            |$mf1->$fr1;
            |$fr2->$fr3;
            |$fr4->$fr5;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "hideDead" - {
        val actual = implicationAll(HideDead, SIG_deadMF4)
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
            |$br1->$mf2;
            |$br1->$br2;
            |$mf3->$fr4;
            |$br2->$mf3;
            |$mf2->$fr2;
            |$fr5->$mf5;
            |$fr1->$fr2;
            |$mf1->$fr1;
            |$fr2->$fr3;
            |$fr4->$fr5;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "v1.1" - {

        "wedged" - {
          import SampleProject8._
          import SampleProject8.Values._
          import ImpGraphConfig._

          val fd = HideDead
          val filter = needFilter("-MF-{10-27}", fd)

          val cfg = ImpGraphConfig(
            GraphDir.LeftToRight,
            LabelFormat.Pubid,
            Colours.ByTag(priTG),
          )
          val actual = implicationAll(fd, project, filter, cfg)
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
                 |1001->1002;
                 |1101->1002;
                 |}
          """.stripMargin)
          assertDOT(actual, expect)
        }

        "striped" - {
          import SampleProject8._
          import SampleProject8.Values._
          import ImpGraphConfig._

          val fd = HideDead
          val filter = needFilter("-MF-{10-27}", fd)

          val cfg = ImpGraphConfig(
            GraphDir.LeftToRight,
            LabelFormat.PubidAndTitle,
            Colours.ByTag(priTG),
          )
          val actual = implicationAll(fd, project, filter, cfg)
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
                 |1001->1002;
                 |1101->1002;
                 |}
          """.stripMargin)
          assertDOT(actual, expect)
        }
      }

      "impRequired" - {
        import UnsafeTypes._, AutoNES._
        import SampleProject.Values._
        val GD = GenericReqGD

        // br3
        // fr7 -> fr8
        //     ↘
        //       fr9 -> br2
        // br1 ↗
        val br3: GenericReqId = 23
        val fr7: GenericReqId = 37
        val fr8: GenericReqId = 38
        val fr9: GenericReqId = 39
        val fr10: GenericReqId = 40
        val p = applyEventsSuccessfully(project,
          GenericReqCreate(br3, br, GD.emptyValues),
          GenericReqCreate(fr7, fr, GD.emptyValues),
          GenericReqCreate(fr8, fr, GD.emptyValues + GD.ImpSrcs(fr7)),
          GenericReqCreate(fr9, fr, GD.emptyValues + GD.ImpSrcs(NonEmptySet(br1, fr7)) + GD.ImpTgts(br2)),
          GenericReqCreate(fr10, fr, GD.emptyValues),
          deleteReqs(fr10))

        // TODO also confirm dead fr with no imp doesn't stem from R

        val actual = implicationAll(ShowDead, p)
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
            |$fr7->$fr9;
            |$fr7->$fr8;
            |}
            |
            |$mf4->$fr6;
            |$br1->$br2;
            |$br1->$mf2;
            |$br1->$fr9;
            |$mf3->$mf4;
            |$mf3->$fr4;
            |$br2->$mf3;
            |$mf2->$fr2;
            |$fr9->$br2;
            |$fr5->$mf5;
            |$fr1->$fr2;
            |$mf1->$fr1;
            |$fr2->$fr3;
            |$fr4->$fr5;
            |}
          """.stripMargin)
        assertDOT(actual, expect)
      }
    }

  }
}
