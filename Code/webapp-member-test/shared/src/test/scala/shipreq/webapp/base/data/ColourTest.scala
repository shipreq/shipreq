package shipreq.webapp.base.data

import shipreq.base.test.BaseTestUtil._
import sourcecode.Line
import utest._

object ColourTest extends TestSuite {

  private implicit def autoColour(s: String) = Colour(s).get

  override def tests = Tests {
    "contrast" - {

      "white" - {
        val c = Colour.white
        assertEqWithTolerance(c.contrastRatio(Colour.black), 21)
        assertEqWithTolerance(c.contrastRatio(Colour.white), 1)
        assertEq(c.foreground, Colour.black)
      }

      "black" - {
        val c = Colour.black
        assertEqWithTolerance(c.contrastRatio(Colour.black), 1)
        assertEqWithTolerance(c.contrastRatio(Colour.white), 21)
        assertEq(c.foreground, Colour.white)
      }

      "red" - {
        val c = Colour("#f00").get
        assertEq(c.foreground, Colour.white)
      }

      "orange" - {
        val c = Colour("#f80").get
        assertEqWithTolerance(c.contrastRatio(Colour.black), 8.773)
        assertEqWithTolerance(c.contrastRatio(Colour.white), 2.393)
        assertEq(c.foreground, Colour.white)
      }

      "yellow" - {
        val c = Colour("#ff0").get
        assertEq(c.foreground, Colour.black)
      }

      "blue" - {
        val c = Colour("#283ba3").get
        assertEqWithTolerance(c.contrastRatio(Colour.black), 2.244)
        assertEqWithTolerance(c.contrastRatio(Colour.white), 9.355)
        assertEq(c.foreground, Colour.white)
      }
    }

    "greyscale" - {
      "#ffffff" - assertEq("#ffffff".greyscale.value, "#ffffff")
      "#000000" - assertEq("#000000".greyscale.value, "#000000")
      "#9c9c9c" - assertEq("#9c9c9c".greyscale.value, "#9c9c9c")
      "#ff0000" - assertEq("#ff0000".greyscale.value, "#7f7f7f")
      "#00ff00" - assertEq("#00ff00".greyscale.value, "#dcdcdc")
      "#0000ff" - assertEq("#0000ff".greyscale.value, "#4c4c4c")
      "#123456" - assertEq("#123456".greyscale.value, "#333333")
    }

    "chooseForegroundOverMultipleBackgroundColours" - {
      def test(in: Colour*)(expectedResult: Colour)(implicit l: Line): Unit =
        assertEq(Colour.chooseForegroundOverMultipleBackgroundColours(in.toVector), expectedResult)

      "majorityEvenWhite" - test("#123", "#234", "#345", "#fde")(Colour.white)
      "majorityEvenBlack" - test("#eff", "#234", "#ffe", "#fde")(Colour.black)
      "majorityOddWhite"  - test("#123", "#234", "#ffe"        )(Colour.white)
      "majorityOddBlack"  - test("#efe", "#234", "#ffe"        )(Colour.black)
      "splitWhite" - {
        assertEq("#aaa".foreground, Colour.black)
        assertEq("#bbb".foreground, Colour.black)
        test("#123", "#234", "#aaa", "#bbb")(Colour.white)
      }
      "splitBlack" - {
        assertEq("#666".foreground, Colour.white)
        assertEq("#676".foreground, Colour.white)
        test("#676", "#666", "#ffe", "#eff")(Colour.black)
      }
    }
  }
}
