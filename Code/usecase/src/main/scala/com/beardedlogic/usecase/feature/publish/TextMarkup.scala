package com.beardedlogic.usecase.feature.publish

import scala.annotation.tailrec
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
    t.map(toLi).foldRight(List.empty[MarkupToken])(foldLis)

  private val toLi: MarkupToken => MarkupToken \/ LI = {
    case l@NonBlankLine(lineContent) =>
      lineContent.head match {
        case PlainText(lineStartText) =>
          lineStartText match {
            case ParseListItem(liStartText) =>
              val newLine =
                if (liStartText.isEmpty) Line(lineContent.tail)
                else NonBlankLine(nel(PlainText(liStartText), lineContent.tail))
              \/-(LI(newLine :: Nil))
            case _ => -\/(l)
          }
        case _ => -\/(l)
      }
    case t@_ => -\/(t)
  }

  private val foldLis: (MarkupToken \/ LI, List[MarkupToken]) => List[MarkupToken] =
    (e, acc) => e match {
      case -\/(mt) => mt :: acc
      case \/-(li) => acc match {
        case UL(lis) :: t => UL(li <:: lis) :: t
        case _            => UL(NEL(li)) :: acc
      }
    }
}