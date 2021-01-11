package shipreq.base.util.diff

import utest._

object PatienceDiffTest extends TestSuite {
  import DiffTestHelpers._

  private implicit val algo = PatienceDiff(MyersLinearDiff[Char])

  override def tests = Tests {

    "prop" - propTest()

    "1" - assertCharDiff("bcdefgzio", "abcxyfgi")(
      "Insert 1 @ 0 <- 0 (b <- a)",
      "Delete 2 @ 2 (d)",
      "Insert 2 @ 4 <- 3 (f <- x)",
      "Delete 1 @ 6 (z)",
      "Delete 1 @ 8 (o)",
    )

//    "2" - assertCharDiff("jabbcdddeml", "iabbghddfmk")(
//
//      "Delete 1 @ 0 (j)",
//      "Insert 1 @ 1 <- 0 (a <- i)",
//
//      "Delete 2 @ 4 (c)",
//      "Insert 2 @ 4 <- 4 (d <- g)",
//
//      "Delete 1 @ 8 (e)",
//      "Insert 1 @ 8 <- 8 (e <- f)",
//
//      "Delete 1 @ 10 (l)",
//      "Insert 1 @ 11 <- 10 (- <- k)",
//    )

  }
}
