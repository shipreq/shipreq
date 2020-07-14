package shipreq.webapp.client.ww

import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.ww.GraphViz.DOT
import utest._

object StepFlowGraphTest extends TestSuite {
  import GraphTestUtil._

  override def tests = Tests {

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
      import SampleProject6._
      import Values._
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
}
