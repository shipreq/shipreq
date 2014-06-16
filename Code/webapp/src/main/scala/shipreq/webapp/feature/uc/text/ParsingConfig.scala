package shipreq.webapp.feature.uc.text

import scala.collection.immutable.SortedSet
import scala.util.matching.Regex
import shipreq.webapp.lib.Types._

object ParsingConfig {

  val RefBraceL = '['
  val RefBraceR = ']'
  val InvalidRefSuffix = '?'

  val RefBraceLs = RefBraceL.toString
  val RefBraceRs = RefBraceR.toString

  val DeletedRefInner = "DELETED"
  val DeletedRefStr = RefBraceL + DeletedRefInner + RefBraceR

  val NormalisationPrefix = "D."
  val NormalisedRefRegex = "\\[D\\.(-?\\d+?)\\]".r

  @inline def makeStepRef(label: StepLabel) = RefBraceL + label.value + RefBraceR
  @inline def makeInvalidStepRef(label: String) = RefBraceL + label + InvalidRefSuffix + RefBraceR
  @inline def makeNormalisedStepRef(textIdentId: TextIdentId) = RefBraceL + NormalisationPrefix + textIdentId.value + RefBraceR

  @inline def makeUseCaseRef(num: UseCaseNumber, title: String): String =
    new StringBuilder(title.length + 10).appendUseCaseRef(num, title).toString

  @inline def makeInvalidUseCaseRef(num: UseCaseNumber, title: Option[String]): String =
    new StringBuilder(title.getOrElse("").length + 12).appendInvalidUseCaseRef(num, title).toString

  val ValidUseCaseRefRegex = "\\[UC-(\\d+?): (.+?)\\]".r
  def makeNormalisedUseCaseRef(m: Regex.Match): String = RefBraceLs + "UC-" + m.group(1) + RefBraceRs

  implicit class StringBuilderPCExt(val sb: StringBuilder) extends AnyVal {
    def braced(fn: => Unit): StringBuilder = {sb += RefBraceL; fn; sb += RefBraceR; sb}

    def appendStepRef(valid: Boolean, label: StepLabel) = braced {
      sb.append(label.value)
      if (!valid) sb.append(InvalidRefSuffix)
    }

    def appendUseCaseRef(num: UseCaseNumber, title: String) = braced {
      sb.append("UC-").append(num.toInt).append(": ").append(title)
    }
    def appendInvalidUseCaseRef(num: UseCaseNumber, title: Option[String]) = braced {
      sb.append("UC-").append(num.toInt).append(InvalidRefSuffix)
      if (title.isDefined) sb.append(": ").append(title.get)
    }
    def appendMathTexTerm(tex: String) = sb.append("{|math: ").append(tex).append(" |}")
  }

  sealed trait FlowStyle {
    val arrow: String
    val unicodeArrows: List[Char]
    val arrowRegex: Regex
    val arrowBadRegex: Regex
    val arrowBadReplacement: String
    final def replaceAllArrowsWithBad(input: String) = arrowBadRegex.replaceAllIn(input, arrowBadReplacement)
    final def makeFlowText(labels: SortedSet[StepLabel]) = {
      val expSize = labels.size * 20 + 2
      val sb = new StringBuilder(expSize)
      sb.append(arrow)
      labels.foreach(l => sb.append(' ').appendStepRef(true, l))
      val r = sb.toString
      //assume(r.length <= expSize, s"Flow text string builder exceeded pre-alloc space. (Exp: $expSize. Got: ${r.length}.)")
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

  val AnyValidArrowRegexStr =
    "(?:" + List(FlowFromStyle.arrowRegex, FlowToStyle.arrowRegex).map(_.pattern.pattern).mkString("|") + ")"
  val AnyValidArrowRegex = AnyValidArrowRegexStr.r

}
