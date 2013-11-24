package com.beardedlogic.usecase.feature.publish

import scala.annotation.tailrec
import scala.util.matching.Regex
import scalaz.{\/, -\/, \/-, NonEmptyList => NEL}
import NEL.nel
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerm
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._

sealed trait MarkupToken
object MarkupTokens {

  object Line {
    def apply(content: List[FreeTextTerm]): Line = content match {
        case Nil    => BlankLine
        case h :: t => NonBlankLine(nel(h, t))
      }
  }

  sealed trait Line extends MarkupToken {
    def toList: List[FreeTextTerm]
  }
  case class NonBlankLine(content: NEL[FreeTextTerm]) extends Line {
    override def toList = content.list
  }
  case object BlankLine extends Line {
    override def toList = List.empty
  }

  /** Unordered list */
  case class UL(lis: NEL[LI]) extends MarkupToken

  /** List item */
  case class LI(content: List[MarkupToken])
}

// =====================================================================================================================

object TextMarkup {
  import MarkupTokens._

  val ParseLines = "(?s)^(.*)\n([^\n]*)$".r
  val ParseListItem = "^\\* +(.*)$".r
  val ParseListItemCont = "^ {2,}(.*)$".r
  val TwoBlankLines = BlankLine :: BlankLine :: Nil

  /**
   * Entry point for conversion. Turn source terms into lines.
   */
  def introduce(terms: List[FreeTextTerm]): List[Line] =
    terms.foldRight(List.empty[Line])((t, lines) =>
      t match {
        case PlainText(txt) => prependText(txt, lines)
        case DeletedRef
             | _: MathTexTerm
             | _: StepRef
             | _: InvalidStepRef
             | _: UseCaseRef
             | _: UseCaseSelfRef
             | _: InvalidUseCaseRef => prependTerm(t, lines)
      }
    )

  private def prependTerm(t: FreeTextTerm, lines: List[Line]): List[Line] =
    lines match {
      case Nil => List(NonBlankLine(NEL(t)))
      case curLine :: ls => NonBlankLine(nel(t, curLine.toList)) :: ls
    }

  @tailrec
  private def prependText(txt: String, lines: List[Line]): List[Line] =
    txt match {
      case "" => lines

      case ParseLines(a, b) =>
        val ls = if (b.nonEmpty)
          BlankLine :: prependTerm(PlainText(b), lines)
        else if (lines.isEmpty)
          TwoBlankLines
        else
          BlankLine :: lines
        prependText(a, ls)

      case _ => prependTerm(PlainText(txt), lines)
    }

  /**
   * Perform all applicable markup conversions to input.
   */
  def markup(t: List[MarkupToken]): List[MarkupToken] =
    markupUL(t)

  /**
   * Extracts ULs from text lines.
   */
  def markupUL(t: List[MarkupToken]): List[MarkupToken] =
    foldLIsIntoULs(
      t.foldLeft(Vector.empty[MarkupToken \/ LI])(foldIntoLIs))

  private def foldIntoLIs(acc: Vector[MarkupToken \/ LI], t: MarkupToken): Vector[MarkupToken \/ LI] =
    t match {
      case line@ NonBlankLine(_) =>

        // Check if start of new LI
        toLI(line) match {
          case newLi @ \/-(_) => acc :+ newLi
          case nonLi @ -\/(_) =>

            // Check if prev is LI
            acc.lastOption match {
              case Some(-\/(_)) | None => acc :+ nonLi
              case Some(\/-(prevLi)) =>

                // Check if line starts with indent
                toLIcont(line) match {
                  case Some(liCont) =>
                    acc.init :+ \/-(LI(liCont :: prevLi.content))
                  case None =>
                    acc :+ nonLi
                }
            }
        }

      case BlankLine =>
        // Check if prev is LI
        acc.lastOption match {
          case Some(-\/(_)) | None => acc :+ -\/(BlankLine)
          case Some(\/-(prevLi))   => acc.init :+ \/-(LI(BlankLine :: prevLi.content))
        }

      case UL(_) => acc :+ -\/(t)
    }

  def toLI(line: NonBlankLine): MarkupToken \/ LI =
    parsePlainText(line, ParseListItem, -\/(_), l => \/-(LI(List(l))))

  def toLIcont(line: NonBlankLine): Option[MarkupToken] =
    parsePlainText(line, ParseListItemCont, _ => None, Some(_))

  private def parsePlainText[R](line: NonBlankLine, parser: Regex, fail: NonBlankLine => R, pass: Line => R): R = {
    line.content.head match {
      case PlainText(lineStartText) =>
        lineStartText match {
          case parser(matchedTxt) =>

            // Matched
            val newLine = if (matchedTxt.isEmpty)
              Line(line.content.tail)
            else
              NonBlankLine(nel(PlainText(matchedTxt), line.content.tail))
            pass(newLine)

          case _ => fail(line) // doesn't match regex
        }
      case _ => fail(line) // Not a PlainText
    }
  }

  private def foldLIsIntoULs(xs: Vector[MarkupToken \/ LI]): List[MarkupToken] = {
    def addBlanks(acc: List[MarkupToken], blanks: Int): List[MarkupToken] = {
      var a = acc
      var b = blanks
      while (b != 0) {a = BlankLine :: a; b -= 1}
      a
    }

    @tailrec
    def go(ii: Int, acc: List[MarkupToken], blanks: Int): List[MarkupToken] = {
      val i = ii - 1
      if (i == -1)
        acc
      else
        xs(i) match {
          case -\/(BlankLine) => go(i, acc, blanks + 1)
          case -\/(t)         => go(i, t :: addBlanks(acc, blanks), 0)
          case \/-(liR) =>
            val li = LI(liR.content.dropWhile(_ == BlankLine).reverse)
            acc match {
              case UL(lis) :: t => go(i, UL(li <:: lis) :: t, 0)
              case _ =>
                val droppedBlanks = (liR.content.length - li.content.length)
                go(i, UL(NEL(li)) :: addBlanks(acc, blanks + droppedBlanks), 0)
            }
        }
    }

    go(xs.length, List.empty, 0)
  }
}