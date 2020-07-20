package shipreq.webapp.client.project.widgets

import shipreq.base.test.BaseTestUtil._
import utest._

object UserDefinedGraphTest extends TestSuite {

  override def tests = Tests {
    "correction" - {

      "graphDefined" - {
        val body = "a [label=qwe]\na -> b; c,d->x"

        for {
          strict     <- List("strict ", "")
          graph      <- List("graph ", "digraph ")
          id         <- List("", "G", "my_graph_1000")
          postHeader <- List("{\n", " {")
        } yield {
          val input  = strict + graph + id + postHeader + body + "}"
          val expect = (strict + graph + id).trim + "{bgcolor=transparent;rankdir=LR;" + body + "}"
          val actual = UserDefinedGraph.correct(input)
          assertMultiline(actual, expect)
        }
      }

      "anonDigraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -> b
            |""".stripMargin.trim
        ),
        """digraph{bgcolor=transparent;rankdir=LR;a [label=qwe]
          |a -> b;}
          |""".stripMargin.trim
      )

      "anonDigraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -> b
            |""".stripMargin.trim
        ),
        """digraph{bgcolor=transparent;rankdir=LR;a [label=qwe]
          |a -> b;}
          |""".stripMargin.trim
      )

      "anonGraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -- b
            |""".stripMargin.trim
        ),
        """graph{bgcolor=transparent;rankdir=LR;a [label=qwe]
          |a -- b;}
          |""".stripMargin.trim
      )

    }
  }
}
