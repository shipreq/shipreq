package shipreq.base.util.diff

import utest._

object MyersLinearDiffTest extends TestSuite {
  import DiffTestHelpers._

  private implicit val algo = MyersLinearDiff[Char]

  override def tests = Tests {

    "prop" - propTestChars()

    "1" - assertCharDiff("bcdefgzio", "abcxyfgi")(
      "Insert 1 @ 0 <- 0 (b <- a)",
      "Delete 1 @ 2 (d)",
      "Insert 2 @ 3 <- 3 (e <- x)",
      "Delete 1 @ 3 (e)",
      "Delete 1 @ 6 (z)",
      "Delete 1 @ 8 (o)",
    )

    "2" - assertCharDiff("omg here it is", "omg! There it goes!")(
      "Insert 1 @ 3 <- 3 (  <- !)",
      "Insert 1 @ 4 <- 5 (h <- T)",
      "Insert 2 @ 12 <- 14 (i <- g)",
      "Delete 1 @ 12 (i)",
      "Insert 1 @ 13 <- 16 (s <- e)",
      "Insert 1 @ 14 <- 18 (- <- !)",
    )

  }
}
