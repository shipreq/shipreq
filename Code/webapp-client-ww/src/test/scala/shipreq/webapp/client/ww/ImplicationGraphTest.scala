package shipreq.webapp.client.ww

import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.client.ww.GraphViz.DOT
import utest._

object ImplicationGraphTest extends TestSuite {
  import GraphTestUtil._

  override def tests = Tests {

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
      val actual = Graphs.implicationFocused(mf3, HideDead, SIG_dead_FR7_MF4)
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
      val actual = Graphs.implicationFocused(mf3, ShowDead, SIG_dead_FR7_MF4)
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
}
