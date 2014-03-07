package shipreq.webapp
package test

import org.scalatest.WordSpec

class NodeUtilsTest extends WordSpec with TestHelpers {
  import NodeUtils._
  import TreeDSL._

  val BT_102 = $("a" ~> $("i", "ii", "iii"), "b", "c" ~> $("i", "ii"))
  val BT_103 = $("a" ~> $("i"), "b")
  val BigTree = $(
    "1.0" ~> $("1", "2" ~> BT_102, "3" ~> BT_103, "4"),
    "1.1" ~> $("1", "2", "3"),
    "1.2" ~> $("1", "2")
  ).toStepNodes

  "parseStepTree()" should {

    "parse textual trees" in {
      val txt = """
1.0. Step:1.0
  1. Step:1
  2. Step:2
    a. Step:a
      i. Step:i
      ii. Step:ii
      iii. Step:iii
    b. Step:b
    c. Step:c
      i. Step:i
      ii. Step:ii
  3. Step:3
    a. Step:a
      i. Step:i
    b. Step:b
  4. Step:4
1.1. Step:1.1
  1. Step:1
  2. Step:2
  3. Step:3
1.2. Step:1.2
  1. Step:1
  2. Step:2
      """
      parseStepTree(txt) should matchTree(BigTree)
    }

    "ignore leading indents that apply to all lines" in {
      val txt = """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
      """
      parseStepTree(txt) should matchTree(BigTree)
    }

    "handle tabs in common leading indents" in {
      val txt = """
		1.0. Step:1.0
		  1. Step:1
		  2. Step:2
		    a. Step:a
		      i. Step:i
		      ii. Step:ii
		      iii. Step:iii
		    b. Step:b
		    c. Step:c
		      i. Step:i
		      ii. Step:ii
		  3. Step:3
		    a. Step:a
		      i. Step:i
		    b. Step:b
		  4. Step:4
		1.1. Step:1.1
		  1. Step:1
		  2. Step:2
		  3. Step:3
		1.2. Step:1.2
		  1. Step:1
		  2. Step:2
      """
      parseStepTree(txt) should matchTree(BigTree)
    }
  }
}
