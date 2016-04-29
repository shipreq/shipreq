package shipreq.webapp.client.ww

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.test._
import GraphViz.DOT

object GraphsTest extends TestSuite {

  val _normalise = "([\\]{;])".r

  def normaliseDOT(d: DOT): String =
    _normalise.replaceAllIn(d.content, "$1\n")

  def assertDOT(actual: DOT, expect: DOT): Unit = {
    val a = normaliseDOT(actual)
    val e = normaliseDOT(DOT(expect.content.trim.replaceAll("\\s*//[^\r\n]+", "").replaceAll("\n *", "")))
    assertMultiline(a, e)
  }

  // TODO Test Graphs.useCaseStepFlow with more complicated flow

  override def tests = TestSuite {

    'stepFlow {
      import SampleProject6._, Values._
      val actual = Graphs.useCaseStepFlow(uc1, project.reqs.useCases)
      val expect = DOT(
        """
          |digraph G{rankdir=LR;ranksep=0.28;
          |
          |S[shape=circle style=filled color=black fontsize=1 height=.3]
          |E[shape=doublecircle style=filled color=black fontsize=1 height=.3]
          |
          |{node[fillcolor=lawngreen style=filled shape=invhouse]
          |  10[label="1.0"]
          |}
          |{node[fillcolor=lawngreen style=filled shape=ellipse]
          |  11[label="1.0.1"]
          |  12[label="1.0.2"]
          |  19[label="1.0.2.a"]
          |  13[label="1.0.3"]
          |}
          |
          |{node[fillcolor=skyblue style=filled shape=invhouse]
          |  14[label="1.1"]
          |}
          |{node[fillcolor=skyblue style="filled,rounded" shape=box]
          |  15[label="1.1.1"]
          |}
          |
          |{node[fillcolor=tomato style=filled shape=octagon]
          |  18[label="1.E.1"]
          |}
          |
          |{edge[weight=9]S->10->11->12->19->13->E}
          |14->15;
          |18;
          |
          |15->12;
          |13->11;
          |
          |}
        """.stripMargin)
      assertDOT(actual, expect)
    }

    'implicationFocused {
      import SampleImplicationGraph._
      val actual = Graphs.implicationFocused(mf3, project.implications, project.reqs, project.config.customReqTypes)
      val expect = DOT(
        """
          |digraph G{rankdir=LR;
          |
          |// Palette: http://paletton.com/#uid=30m0u0kcsAE3HVh7UMch6tklvom
          |
          |node[style=filled color="#333333"]
          |
          |F[style=bold fillcolor="#cccccc" label="MF-3"]
          |
          |// Indirect parents
          |node[fillcolor="#FFEDE2"]
          |21[label="BR-1"]
          |
          |// Direct parents
          |{
          |rank=same;
          |node[fillcolor="#FFC19C"]
          |22[label="BR-2"]
          |}
          |
          |// Direct children
          |{
          |rank=same;
          |node[fillcolor="#7692B7" fontcolor=white]
          |14[label="MF-4"]
          |34[label="FR-4"]
          |}
          |
          |// Indirect children
          |node[fillcolor="#D6E1EF"]
          |36[label="FR-6"]
          |35[label="FR-5"]
          |15[label="MF-5"]
          |
          |edge[color="#FFC19C"]
          |21->22;
          |edge[color="#C27040"]
          |22->F;
          |edge[color="#31537F"]
          |F->14,34;
          |edge[color="#7692B7"]
          |14->36;
          |34->35;
          |35->15;
          |
          |}
        """.stripMargin)
      assertDOT(actual, expect)
    }
  }
}
