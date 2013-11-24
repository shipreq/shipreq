package com.beardedlogic.usecase.feature.publish

import scalaz._, Scalaz._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.PropertyChecks
import com.beardedlogic.usecase.feature.Inspection.{listShow => _, _}
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerm
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import MarkupTokens._

class TextMarkupTest extends FunSpec with Matchers with PropertyChecks {

  object Gens {
    import org.scalacheck.Gen

    val lowAsciiChar: Gen[Char] = Gen.oneOf((33 to 126).map(_.toChar))
    val specialChar: Gen[Char] = Gen.oneOf('\n', '*', ' ')
    val anyChar: Gen[Char] = lowAsciiChar | specialChar
    val string: Gen[String] = for (cs <- Gen.listOf(anyChar)) yield cs.mkString
    val nonEmptyString: Gen[String] = for (cs <- Gen.listOf1(anyChar)) yield cs.mkString

    val plainText: Gen[PlainText] = nonEmptyString.map(PlainText)

    val jamaTerm: Gen[FreeTextTerm] = DeletedRef

    def option[T](g: Gen[T]): Gen[Option[T]] =
      Gen.oneOf[Option[T]](g.map(Some.apply), None)

    val freeTextTerms: Gen[List[FreeTextTerm]] = for {
      pts <- Gen.listOf(plainText)
      jama <- jamaTerm
      prefix <- option(jamaTerm)
      suffix <- option(jamaTerm)
    } yield
      (prefix.toList ::: pts ::: suffix.toList) intersperse jama
  }

  // -------------------------------------------------------------------------------------------

  type MTs = List[MarkupToken]

  implicit def autolist[T](t: T): List[T] = List(t)
  implicit def s2pt(t: String): PlainText = PlainText(t)
  implicit def s2fttl(t: String): List[FreeTextTerm] = List(t)

  def line(content: FreeTextTerm*) = Line(content.toList)
  def ul(h: LI, t: LI*) = UL(NonEmptyList(h, t: _*))
  def li(content: MarkupToken*) = LI(content.toList)

  def testLines(in: List[FreeTextTerm], exp: List[Line]): Unit = TextMarkup.introduce(in) shouldBe exp
  def test1(in: List[FreeTextTerm], exp: List[MarkupToken]): Unit = test2(TextMarkup.introduce(in), exp)
  def test2(in: List[MarkupToken], exp: List[MarkupToken]): Unit = TextMarkup.markup(in) shouldBe exp
  def testAll(examples: (List[FreeTextTerm], MTs)*): Unit = forAll(Table(("Input", "Expectation"), examples: _*))(test1)

  // -------------------------------------------------------------------------------------------

  describe("Terms -> Lines") {
    def rebuildTerms(lines: List[Line]): List[FreeTextTerm] =
      lines
      .foldRight(List.empty[FreeTextTerm])((line, acc) => line.toList ::: PlainText("\n") :: acc)
      .dropRight(1)
      .foldRight(List.empty[FreeTextTerm])({
        case (PlainText(a), PlainText(b) :: bs) => PlainText(a + b) :: bs
        case (a, bs) => a :: bs
      })

    it("should split into lines as per examples") {
      val exs = Table[List[FreeTextTerm], List[Line]](("Input", "Expectation")
        , (Nil, Nil)
        , ("Hehe", line("Hehe"))
        , ("He\nhe", line("He") :: line("he"))
        , ("He\n\nhe", line("He") :: BlankLine :: line("he"))
        , ("He\n \nhe", line("He") :: line(" ") :: line("he"))
        , ("\nHehe", BlankLine :: line("Hehe"))
        , ("Hehe\n", line("Hehe") :: BlankLine)
        , ("\n\nHehe", BlankLine :: BlankLine :: line("Hehe"))
        , ("Hehe\n\n", line("Hehe") :: BlankLine :: BlankLine)
      )
      forAll(exs)(testLines)
    }

    it("should be isomorphic") {
      forAll(Gens.freeTextTerms)(terms => {
        val lines = TextMarkup.introduce(terms)
        val t2 = rebuildTerms(lines)
        if (terms != t2) {
          var s = "\nt : " + terms.show
          s += "\nt2: " + t2.show
          s += "\nlines: " + lines + "\n"
          fail(s)
        }
        terms shouldBe t2
      })
    }
  }

  describe("UL") {
    implicit def s2l(t: String): Line = Line(t)

    it("should match lines starting with \"* \"") {
      test1("* Hehe", ul(li("Hehe")))
    }

    it("should trim left whitespace from LIs") {
      test1("*    Hehe", ul(li("Hehe")))
    }

    it("should join adjacent LIs") {
      testAll(
        ("* Hehe\n* Great", ul(li("Hehe"), li("Great")))
        , ("* Hehe\n* middle\n* Great", ul(li("Hehe"), li("middle"), li("Great")))
      )
    }

    it("should gobble blank lines between adjacent LIs") {
      testAll(
        ("* Hehe\n\n* Great", ul(li("Hehe"), li("Great")))
        ,("* Hehe\n\n\n* middle\n\n\n* Great", ul(li("Hehe"), li("middle"), li("Great"))))
    }

    it("should allow indented LI continuations") {
      testAll(("* Hehe\n  Cont", ul(li("Hehe", "Cont")))
        ,("* Hehe\n  Cont\n* Great", ul(li("Hehe", "Cont"), li("Great")))
        ,("* Hehe\n  Cont\n  More\n* Great\n  Yeah", ul(li("Hehe", "Cont", "More"), li("Great", "Yeah")))
      )
    }

    it("should trim indent from LI continuations") {
      test1("* Hehe\n        Cont", ul(li("Hehe", "Cont")))
    }

    it("should allow blank lines in LIs") {
      testAll(("* Hehe\n\n  Cont", ul(li("Hehe", BlankLine, "Cont")))
        ,("* Hehe\n  \n  Cont", ul(li("Hehe", BlankLine, "Cont")))
        ,("* Hehe\n  \n\n  Cont", ul(li("Hehe", BlankLine, BlankLine, "Cont")))
        ,("* Hehe\n  \n\n  Cont\n* cool", ul(li("Hehe", BlankLine, BlankLine, "Cont"), li("cool")))
      )
    }

    it("should consider end a UL when a non-LI is encountered") {
      testAll(("* Hehe\nTEXT", ul(li("Hehe")) :: Line("TEXT"))
        ,("* Hehe\nTEXT\n* Great", ul(li("Hehe")) :: Line("TEXT") :: ul(li("Great")))
        ,("* Hehe\n\nTEXT", ul(li("Hehe")) :: BlankLine :: Line("TEXT"))
        ,("* Hehe\n\nTEXT\n* Great", ul(li("Hehe")) ::BlankLine ::  Line("TEXT") :: ul(li("Great")))
        ,("* Hehe\n\nTEXT\n\nMore\n\n* Great\n  YeaH", ul(li("Hehe")) :: BlankLine :: Line("TEXT") :: BlankLine :: Line("More") :: BlankLine :: ul(li("Great", "YeaH")))
      )
    }

    it("should preserve blank line around ULs") {
      testAll(
        ("* Hehe\n\nTEXT", ul(li("Hehe")) :: BlankLine :: Line("TEXT"))
        ,("* Hehe\n\n\nTEXT", ul(li("Hehe")) :: BlankLine :: BlankLine :: Line("TEXT"))
        ,("TEXT\n\n* Hehe", Line("TEXT") :: BlankLine :: ul(li("Hehe")))
        ,("TEXT\n\n\n* Hehe", Line("TEXT") :: BlankLine :: BlankLine :: ul(li("Hehe")))
      )
    }
  }
}
