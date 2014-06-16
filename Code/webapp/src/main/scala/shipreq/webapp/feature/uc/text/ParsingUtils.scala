package shipreq.webapp.feature.uc.text

import net.liftweb.common.Logger
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.{StepAndLabelBiMap, SavedSteps}
import ParsingConfig._

object ParsingUtils extends Logger {

  @inline def areLabelsValid_?(labels: Seq[StepLabel])(implicit stepsAndLabels: StepAndLabelBiMap): Boolean = {
    if (labels.isEmpty) true
    else {
      val validLabels = stepsAndLabels.value.ba
      !labels.exists(!validLabels.contains(_))
    }
  }

  @inline def flowHasRefs_?(c: Option[FlowClause]) = c.exists(_.refs.nonEmpty)

  def invalidateFlowArrows(text: String): String = {
    var t = text
    t = FlowFromStyle.replaceAllArrowsWithBad(t)
    t = FlowToStyle.replaceAllArrowsWithBad(t)
    t
  }

  /**
   * Normalises text before saving to the database.
   */
  def normalise(text: String, refs: Map[LocalStepId, StepLabel], savedSteps: SavedSteps): NormalisedText = {

    /** Example: converts [4.0.1.b] to [D.1045]. */
    @inline def normaliseStepRefs(text: String) = {
      val localToDb = savedSteps.ba
      var r = text
      for {
        (localId, label) <- refs
        dataId <- localToDb.get(localId)
      } r = r.replace(makeStepRef(label), makeNormalisedStepRef(dataId))
      r
    }

    /** Example: converts [UC-3: Do stuff] to [UC-3] */
    @inline def normaliseUcRefs(text: String) =
      ValidUseCaseRefRegex.replaceAllIn(text, makeNormalisedUseCaseRef _)

    NormalisedText(normaliseStepRefs(normaliseUcRefs(text)))
  }

  /**
   * Realises (un-normalises) DB-id-based step references in text, to label based references.
   *
   * Example: converts [D.1045] to [4.0.1.b].
   *
   * @param text Text with all step references normalised with DB data IDs instead of human-readable labels.
   * @return Text with all step references
   */
  def realiseNormalisedStepRefs(text: NormalisedText)(implicit savedSteps: SavedSteps,
    stepsAndLabels: StepAndLabelBiMap): InputCorrected[String] = {

    val dbIdsToLocalIds = savedSteps.ab
    lazy val localIdsToLabels = stepsAndLabels.value.ab

    InputCorrected(NormalisedRefRegex.replaceAllIn(text, m => {
      val idText = m.group(1)
      val textIdentId = TextIdentId(idText.toLong)
      dbIdsToLocalIds.get(textIdentId).flatMap(nodeId => localIdsToLabels.get(nodeId)).fold {
        warn(s"Unable to realise normalised step reference. ❚ Text: $text ❚ TextIdentId: $textIdentId ❚ DbIdsToLocalIds: $dbIdsToLocalIds ❚ LocalIdsToLabels: $localIdsToLabels")
        makeInvalidStepRef(idText)
      }(makeStepRef)
    }))
  }
}
