package shipreq.webapp.client.ww.graph

import shipreq.webapp.client.ww.graph.GraphViz.DOT
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.Colours
import utest._

object ReqImpGraphTest extends TestSuite {
  import GraphTestUtil._

  private def render(focus     : ReqId,
                     filterDead: FilterDead,
                     project   : Project,
                     colours   : Option[Colours] = None): DOT =
    new ReqImpGraph(focus, filterDead, project, colours).dot
  
  override def tests = Tests {

    "basic" - {
      import shipreq.webapp.member.test.project.SampleImplicationGraph._
      val actual = render(mf3, HideDead, project)
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
          |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
          |
          |edge[color="#FFC19C"]
          |21->22;
          |
          |edge[color="#C27040"]
          |22->F;
          |
          |edge[color="#31537F"]
          |F->14;
          |F->34;
          |
          |edge[color="#7692B7"]
          |14->36;
          |34->35;
          |35->15;
          |
          |edge[color="#aaaaaa" style=dashed]
          |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "hideDead" - {
      import shipreq.webapp.member.test.project.SampleImplicationGraph._
      val actual = render(mf3, HideDead, SIG_dead_FR7_MF4)
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
          |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
          |
          |edge[color="#FFC19C"]
          |21->22;
          |
          |edge[color="#C27040"]
          |22->F;
          |
          |edge[color="#31537F"]
          |F->34;
          |
          |edge[color="#7692B7"]
          |34->35;
          |35->15;
          |
          |edge[color="#aaaaaa" style=dashed]
          |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "showDead" - {
      import shipreq.webapp.member.test.project.SampleImplicationGraph._
      val actual = render(mf3, ShowDead, SIG_dead_FR7_MF4)
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
          |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
          |
          |edge[color="#FFC19C"]
          |21->22;
          |
          |edge[color="#C27040"]
          |22->F;
          |
          |edge[color="#31537F"]
          |F->14[color="#bbbbbb" style=dashed];
          |F->34;
          |
          |edge[color="#7692B7"]
          |14->36[color="#bbbbbb" style=dashed];
          |34->35;
          |35->15;
          |
          |edge[color="#aaaaaa" style=dashed]
          |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    // https://shipreq.com/project/d6My#/reqs/FR-21
    "impFieldExternals" - {

      "SIG1_FR1" - {
        // mf1 → [fr1] ↘
        //               fr2 → fr3
        //         mf2 ↗
        import shipreq.webapp.member.test.project.SampleImplicationGraph._
        val actual = render(fr1, HideDead, project)
        val expect = DOT(
          s"""
             |digraph G{bgcolor=transparent;rankdir=LR;
             |node[style=filled shape=ellipse color="#222222"]
             |
             |F[style=bold fillcolor="#cccccc" label="FR-1"]
             |
             |node[fillcolor="#FFEDE2"]
             |{
             |  rank=same;
             |  node[fillcolor="#FFC19C"]
             |  $mf1[id="MF-1" label="MF-1"]
             |}
             |{
             |  rank=same;
             |  node[fillcolor="#7692B7" fontcolor=white]
             |  $fr2[id="FR-2" label="FR-2"]
             |}
             |
             |node[fillcolor="#D6E1EF"]
             |$fr3[id="FR-3" label="FR-3"]
             |
             |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
             |$mf2[id="MF-2" label="MF-2"]
             |
             |edge[color="#FFC19C"]
             |edge[color="#C27040"]
             |$mf1->F;
             |edge[color="#31537F"]
             |F->$fr2;
             |edge[color="#7692B7"]
             |$fr2->$fr3;
             |
             |edge[color="#aaaaaa" style=dashed]
             |$mf2->$fr2;
             |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "SIG2_FB4" - {
        // [FB4] → FB3 → FR1
        //          ↑     ↑
        //         MF4   MF1
        import shipreq.webapp.member.test.project.SampleImplicationGraph2._
        val actual = render(fb4, HideDead, project)
        val expect = DOT(
          s"""
             |digraph G{bgcolor=transparent;rankdir=LR;
             |node[style=filled shape=ellipse color="#222222"]
             |
             |F[style=bold fillcolor="#cccccc" label="FB-4"]
             |
             |node[fillcolor="#FFEDE2"]
             |{
             |  rank=same;
             |  node[fillcolor="#FFC19C"]
             |}
             |{
             |  rank=same;
             |  node[fillcolor="#7692B7" fontcolor=white]
             |  $fb3[id="FB-3" label="FB-3"]
             |}
             |
             |node[fillcolor="#D6E1EF"]
             |$fr1[id="FR-1" label="FR-1"]
             |
             |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
             |$mf1[id="MF-1" label="MF-1"]
             |$mf4[id="MF-4" label="MF-4"]
             |
             |edge[color="#FFC19C"]
             |edge[color="#C27040"]
             |edge[color="#31537F"]
             |F->$fb3;
             |edge[color="#7692B7"]
             |$fb3->$fr1;
             |
             |edge[color="#aaaaaa" style=dashed]
             |$mf4->$fb3;
             |$mf1->$fr1;
             |}
          """.stripMargin)
        assertDOT(actual, expect)
      }

      "SIG3_FB4" - {
        //         MF1   MF4
        //           ↘   ↙
        //            UC3
        //           ↙   ↘
        // [FB4] → FB3 → FR1
        import shipreq.webapp.member.test.project.SampleImplicationGraph3._
        val actual = render(fb4, HideDead, project)
        val expect = DOT(
          s"""
             |digraph G{bgcolor=transparent;rankdir=LR;
             |node[style=filled shape=ellipse color="#222222"]
             |
             |F[style=bold fillcolor="#cccccc" label="FB-4"]
             |
             |node[fillcolor="#FFEDE2"]
             |{
             |  rank=same;
             |  node[fillcolor="#FFC19C"]
             |}
             |{
             |  rank=same;
             |  node[fillcolor="#7692B7" fontcolor=white]
             |  $fb3[id="FB-3" label="FB-3"]
             |}
             |
             |node[fillcolor="#D6E1EF"]
             |$fr1[id="FR-1" label="FR-1"]
             |
             |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
             |$uc3[id="UC-3" label="UC-3"]
             |$mf1[id="MF-1" label="MF-1"]
             |$mf4[id="MF-4" label="MF-4"]
             |
             |edge[color="#FFC19C"]
             |edge[color="#C27040"]
             |edge[color="#31537F"]
             |F->$fb3;
             |edge[color="#7692B7"]
             |$fb3->$fr1;
             |
             |edge[color="#aaaaaa" style=dashed]
             |$mf4->$uc3;
             |$uc3->$fr1;
             |$mf1->$uc3;
             |$uc3->$fb3;
             |}
          """.stripMargin)
        assertDOT(actual, expect)
      }
    }

    "colourByReqType" - {
      import shipreq.webapp.member.test.project.SampleImplicationGraph._
      val actual = render(mf3, HideDead, project, Some(Colours.ByReqType))
      val expect = DOT(
        """
          |digraph G{bgcolor=transparent;rankdir=LR;
          |node[style=filled shape=ellipse color="#222222"]
          |
          |F[style=bold fillcolor="#cccccc" label="MF-3"]
          |
          |node[fillcolor="#FFEDE2"]
          |21[id="BR-1" label="BR-1" fillcolor="#B7D058"]
          |
          |{rank=same;node[fillcolor="#FFC19C"]
          |22[id="BR-2" label="BR-2" fillcolor="#B7D058"]
          |}
          |
          |{rank=same;
          |14[id="MF-4" label="MF-4" fillcolor="#93D5BA"]
          |34[id="FR-4" label="FR-4" fillcolor="#D5A8C9"]
          |}
          |
          |node[fillcolor="#D6E1EF"]
          |36[id="FR-6" label="FR-6" fillcolor="#D5A8C9"]
          |35[id="FR-5" label="FR-5" fillcolor="#D5A8C9"]
          |15[id="MF-5" label="MF-5" fillcolor="#93D5BA"]
          |
          |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
          |
          |edge[color="#FFC19C"]
          |21->22;
          |
          |edge[color="#C27040"]
          |22->F;
          |
          |edge[color="#31537F"]
          |F->14;
          |F->34;
          |
          |edge[color="#7692B7"]
          |14->36;
          |34->35;
          |35->15;
          |
          |edge[color="#aaaaaa" style=dashed]
          |}
          """.stripMargin)
      assertDOT(actual, expect)
    }

    "colourByReqType" - {
      import shipreq.webapp.member.test.project.SampleProject8.Values._
      import shipreq.webapp.member.test.project.SampleProject8._
      val actual = render(mfs(1), HideDead, project, Some(Colours.ByTag(priTG)))
      val expect = DOT(
        """
          |digraph G{
          |bgcolor=transparent;
          |rankdir=LR;
          |node[style=filled shape=ellipse color="#222222"]
          |F[style=bold fillcolor="#cccccc" label="MF-1"]
          |
          |node[fillcolor="#FFEDE2"]
          |{rank=same;
          |  node[fillcolor="#FFC19C"]
          |}
          |
          |{rank=same;
          |  1002[id="FR-2" label="FR-2" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
          |}
          |
          |node[fillcolor="#D6E1EF"]
          |1127[id="MF-27" label="MF-27" style=filled fillcolor="#0076f5" fontcolor="#ffffff"]
          |
          |node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]
          |edge[color="#FFC19C"]
          |edge[color="#C27040"]
          |edge[color="#31537F"]
          |
          |F->1002;
          |
          |edge[color="#7692B7"]
          |
          |1002->1127;
          |
          |edge[color="#aaaaaa" style=dashed]
          |}
          """.stripMargin)
      assertDOT(actual, expect)
    }
  }
}
