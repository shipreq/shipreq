package shipreq.base.util.diff

import utest._

object PatienceDiffTest extends TestSuite {
  import DiffTestHelpers._

  private val myers = MyersLinearDiff[Char]
  private val algoByChars = PatienceDiff(myers)
  private val algoByLines = PatienceDiff.splitStrings(myers)

  override def tests = Tests {

    "byChars" - {
      implicit def algo = algoByChars

      "prop" - propTestChars(100000)

      "1" - assertCharDiff("bcdefgzio", "abcxyfgi")(
        "Insert 1 @ 0 <- 0 (b <- a)",
        "Delete 2 @ 2 (d)",
        "Insert 2 @ 4 <- 3 (f <- x)",
        "Delete 1 @ 6 (z)",
        "Delete 1 @ 8 (o)",
      )

      "2" - assertCharDiff("CE", "EG")(
        "Delete 1 @ 0 (C)",
        "Insert 1 @ 2 <- 1 (- <- G)",
      )

      //    "3" - assertCharDiff("jabbcdddeml", "iabbghddfmk")(
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

    "byLines" - {
      implicit def algo = algoByLines

      "prop" - propTestLines()

      "1" - {
        val x = "CBBC\nADBABCCB\nAAB\nDCCD\nADA\n\nCA\nADBBBBA\nCCBCDAC\nABCAABDC\nC"
        val y = "DCCD\nB\nAD\nCBAB\nC\nC\nCCDCAC\nADCBC\nBDD\nDCA\nDAD\nAABCDDD"
        assertRoundTrip(x, y)
        ()
      }

      "2" - {
        val x = "DACA\nCAAC\nCDDD\nD"
        val y = "D\nCCBA\nA\nC"
        assertRoundTrip(x, y)
        ()
      }

    }
  }
}
