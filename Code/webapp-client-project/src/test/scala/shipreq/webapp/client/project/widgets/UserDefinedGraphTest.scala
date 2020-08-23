package shipreq.webapp.client.project.widgets

import shipreq.base.test.BaseTestUtil._
import utest._

object UserDefinedGraphTest extends TestSuite {
  import UserDefinedGraph.defaults

  override def tests = Tests {
    "correction" - {

      "graphDefined" - {
        val body = "a [label=qwe]\na -> b; c,d->x"

        for {
          strict     <- List("strict ", "")
          graph      <- List("graph ", "digraph ")
          id         <- List("", "G", "my_graph_1000")
          postHeader <- List("{\n", " {")
        } {
          val input  = strict + graph + id + postHeader + body + "}"
          val expect = (strict + graph + id).trim + s"{$defaults;$body}"
          val actual = UserDefinedGraph.correct(input)
          assertMultiline(actual, expect)
        }
      }

      "anonDigraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -> b
            |# wow
            |""".stripMargin.trim
        ),
        s"""digraph{$defaults;a [label=qwe]
           |a -> b
           |# wow
           |}
           |""".stripMargin.trim
      )

      "anonDigraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -> b
            |""".stripMargin.trim
        ),
        s"""digraph{$defaults;a [label=qwe]
           |a -> b
           |}
           |""".stripMargin.trim
      )

      "anonGraph" - assertMultiline(
        UserDefinedGraph.correct(
          """a [label=qwe]
            |a -- b
            |// ha
            |""".stripMargin.trim
        ),
        s"""graph{$defaults;a [label=qwe]
           |a -- b
           |// ha
           |}
           |""".stripMargin.trim
      )

    }
  }
}
