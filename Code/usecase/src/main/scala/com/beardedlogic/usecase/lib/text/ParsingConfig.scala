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
    final def makeFlowText(labels: SortedSet[StepLabel]) = {
      val expSize = labels.size * 24 + 2
      val sb = new StringBuilder(expSize)
      sb.append(arrow)
      labels.foreach(l => {
        sb.append(' ')
        makeRef(sb, l)
      })
      val r = sb.toString
      assume(r.length <= expSize, s"Flow text string builder exceeded pre-alloc space. (Exp: $expSize. Got: ${r.length}.)")
      r
    }
    final def makeFlowTextOrEmpty(labels: SortedSet[StepLabel]) = if (labels.isEmpty) "" else makeFlowText(labels)
  }

  case object FlowFromStyle extends FlowStyle {
    override val arrow = "⬅"
    override val unicodeArrows = (arrow + "←⇦⇐⇽").toList
    override val arrowRegex = s"<-{2,}|[${unicodeArrows.mkString}]".r
    override val arrowBadRegex = s"(?:<-|[${unicodeArrows.mkString}])-*".r
    override val arrowBadReplacement = "<-"
  }

  case object FlowToStyle extends FlowStyle {
    override val arrow = "➡"
    override val unicodeArrows = (arrow + "→⇨⇒⇾").toList
    override val arrowRegex = s"-{2,}>|[${unicodeArrows.mkString}]".r
    override val arrowBadRegex = s"-*(?:->|[${unicodeArrows.mkString}])".r
    override val arrowBadReplacement = "->"
  }

  val AnyValidArrowRegex =
    "(?:" + List(FlowFromStyle.arrowRegex, FlowToStyle.arrowRegex).map(_.pattern.pattern).mkString("|") + ")"

  @inline private def makeRef_(sb: StringBuilder)(fn: => Any): Unit = { sb += RefBraceL; fn; sb += RefBraceR }
  @inline def makeRef(sb: StringBuilder, label: String) = makeRef_(sb)(sb ++= label)
  @inline def makeRef(label: String) = RefBraceL + label + RefBraceR

  val NormalisationPrefix = "D."
  @inline def makeNormalisedRef(textIdentId: TextIdentId) = makeRef(NormalisationPrefix + textIdentId)

  val InvalidRefSuffix = '?'
  @inline def makeInvalidLabel(label: String) = label + InvalidRefSuffix
  @inline def makeInvalidRef(label: String) = makeRef(makeInvalidLabel(label))
  @inline def makeInvalidRef(sb: StringBuilder, label: String) = makeRef_(sb) {sb ++= label; sb += InvalidRefSuffix}
  @inline def makeInvalidNormalisedRef(textIdentId: String) = makeInvalidRef(NormalisationPrefix + textIdentId)
}
