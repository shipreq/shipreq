package com.beardedlogic.usecase.lib.text

import scala.collection.immutable.SortedSet
import scala.util.matching.Regex
import com.beardedlogic.usecase.lib.Types._

object ParsingConfig {

  val RefBraceL = '['
  val RefBraceR = ']'

  val DeletedRef = makeRef("DELETED")

  val NormalisedRefRegex = "\\[D\\.(\\d+?)\\]".r

  sealed trait FlowStyle {
    val arrow: String
    val unicodeArrows: List[Char]
    val arrowRegex: Regex
    val arrowBadRegex: Regex
    val arrowBadReplacement: String
    final def replaceAllArrowsWithBad(input: String) = arrowBadRegex.replaceAllIn(input, arrowBadReplacement)
    final def makeFlowText(labels: SortedSet[LabelStr]) = {
      val sb = new StringBuilder(labels.size * 14 + 2)
      sb.append(arrow)
      labels.foreach(l => {
        sb.append(' ')
        makeRef(sb, l)
      })
      sb.toString
    }
    final def makeFlowTextOrEmpty(labels: SortedSet[LabelStr]) = if (labels.isEmpty) "" else makeFlowText(labels)
  }

  object FlowFromStyle extends FlowStyle {
    override val arrow = "⬅"
    override val unicodeArrows = (arrow + "←⇦⇐⇽").toList
    override val arrowRegex = s"<-{2,}|[${unicodeArrows.mkString}]".r
    override val arrowBadRegex = s"(?:<-|[${unicodeArrows.mkString}])-*".r
    override val arrowBadReplacement = "<-"
  }

  object FlowToStyle extends FlowStyle {
    override val arrow = "➡"
    override val unicodeArrows = (arrow + "→⇨⇒⇾").toList
    override val arrowRegex = s"-{2,}>|[${unicodeArrows.mkString}]".r
    override val arrowBadRegex = s"-*(?:->|[${unicodeArrows.mkString}])".r
    override val arrowBadReplacement = "->"
  }

  @inline def makeInvalidLabel(label: String) = label + "?"

  @inline def makeInvalidRef(sb: StringBuilder, label: String) = {
    sb += RefBraceL
    sb ++= label
    sb += '?'
    sb += RefBraceR
  }

  @inline def makeInvalidNormalisedRef(textIdentId: String) = makeRef("D." + textIdentId)

  @inline def makeNormalisedRef(textIdentId: TextIdentId) = makeRef("D." + textIdentId)

  @inline def makeRef(label: String) = RefBraceL + label + RefBraceR

  @inline def makeRef(sb: StringBuilder, label: String) {
    sb += RefBraceL
    sb ++= label
    sb += RefBraceR
  }
}
