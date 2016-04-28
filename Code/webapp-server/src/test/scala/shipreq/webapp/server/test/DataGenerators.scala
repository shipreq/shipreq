package shipreq.webapp.server.test

/*
object DataGenerators extends Logger {
  import TestHelpers._

  implicit class RegexExt(val x: Regex) extends AnyVal {
    def matches(str: String) = x.pattern.matcher(str).matches()
  }

  implicit class BooleanExt(val x: Boolean) extends AnyVal {
    def :||(err: => String): Prop = Prop(x) :| (if (x) "" else err)
  }

//  def gen[T](f: Gen.Parameters => R[T]): Gen[T] = new Gen[T] {
//    def doApply(p: Gen.Parameters) = f(p)
//  }

  /**
   * Same as `Gen.frequency` except if the selected generator cannot provide, it is removed from the freq map and we
   * try again.
   */
  def frequencyTrialAndError[I, O](gs: (Int, Gen[I])*)(eval: I => O)(test: O => Boolean): Gen[(I, O)] =
    Gen.parameterized[(I, O)](prms => {
      @tailrec def go(remaining: List[(Int, Gen[I])]): Gen[(I, O)] = remaining match {
        case Nil => Gen.fail
        case _ =>
          val gen = Gen.frequency(gs: _*)
          gen.apply(prms).map(i => (i, eval(i))) match {
            case Some(r@(i, o)) if test(o) => Gen.const(r)
            case _ => go(remaining.filterNot(_._2 eq gen))
          }
      }
      go(gs.toList)
    })

  // -------------------------------------------------------------------------------------------------------------------
  // Low level

  implicit lazy val arbChar: Arbitrary[Char] = Arbitrary {
    val minChar: Char = 32
    Gen.frequency(
      (0xD800 - minChar, Gen.choose(minChar, (0xD800 - 1).asInstanceOf[Char])),
      (Char.MaxValue - 0xDFFF, Gen.choose((0xDFFF + 1).asInstanceOf[Char], Char.MaxValue))
    )
  }

  implicit lazy val arbitraryString: Arbitrary[String] = Arbitrary(
    //Gen.listOf1(arbitrary[Char] | lowAsciiChar).map(_.mkString)
    //Gen.listOf1(lowAsciiChar).map(_.mkString)
    lowAsciiChar.map(_.toString)
  )

  val lowAsciiChar: Gen[Char] = Gen.oneOf((33 to 126).map(_.toChar))

  val nothing: Gen[String] = ""

  val whitespaceChar: Gen[Char] = // ' '
    Gen.frequency((9, ' '), (1, '\t'), (2, '\n'))

  val whitespace: Gen[String] = " " //Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, whitespaceChar)).map(_.mkString)

  val optionalWhitespace: Gen[String] = Gen.oneOf(nothing, whitespace)

  def containsAlphaNum(str: String): Boolean = str.exists(Character.isLetterOrDigit)

  def withBraces(str: Gen[String]) = withOptionalSurroundingWhitespace(str).map(s => s"[$s]")

  def withOptionalBraces(str: Gen[String]) = Gen.oneOf(str, withBraces(str))

  def withOptionalSurroundingWhitespace(gen: Gen[String]): Gen[String] = for {
    w1 <- optionalWhitespace
    mid <- gen
    w2 <- optionalWhitespace
  } yield w1 + mid + w2

  def mkStringWithGen(gxs: Gen[List[String]], sep: Gen[String]): Gen[String] =
    gxs.flatMap(xs =>
      xs.foldRight(Gen.const("")) {
        case (a, g) =>
          for {b <- g; s <- sep} yield a + s + b
      })

  def mkStringWithWhitespace(gxs: Gen[List[String]]): Gen[String] = mkStringWithGen(gxs, whitespace)
}
*/
