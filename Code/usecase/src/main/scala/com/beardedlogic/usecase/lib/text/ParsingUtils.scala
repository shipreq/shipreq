package com.beardedlogic.usecase.lib.text

import net.liftweb.common.Logger
import com.beardedlogic.usecase.lib.Types._
import ParsingConfig._

object ParsingUtils extends Logger {

  @inline def areLabelsValid_?(labels: Seq[StepLabel])(implicit stepsAndLabels: StepAndLabelBiMap): Boolean = {
    if (labels.isEmpty) true
    else {
      val validLabels = stepsAndLabels.value.ba
      !labels.exists(!validLabels.contains(_))
    }
  }

  @inline def flowHasRefs_?(c: Option[FlowClause]) = c.map(_.refs.nonEmpty).getOrElse(false)

  def invalidateFlowArrows(text: String): String = {
    var t = text
    t = FlowFromStyle.replaceAllArrowsWithBad(t)
    t = FlowToStyle.replaceAllArrowsWithBad(t)
    t
  }

  /**
   * Keeps a map of step references (ie. step-id to label) up-to-date when the step tree structure changes.
   */
  def migrateRefsToNewStepTree(ft: FreeText)(implicit stepsAndLabels: StepAndLabelBiMap): Option[FreeText] = {
    lazy val idsToLabels = stepsAndLabels.value.ab
    var newRefs = Map.empty[LocalStepId, StepLabel]
    var newText = ft.text
    var changed = false

    // Check each references we know our text has
    for ((id, oldLabel) <- ft.refs) {
      idsToLabels.get(id) match {
        case Some(newLabel) =>
          if (oldLabel != newLabel) {
            newText = newText.replace(makeRef(oldLabel), makeRef(newLabel))
            changed = true
          }
          newRefs += (id -> newLabel)
        case _ =>
          newText = newText.replace(makeRef(oldLabel), DeletedRef)
          changed = true
      }
    }

    if (changed) Some(FreeText(newText, newRefs))
    else None
  }

  /**
   * Normalises references in text so that they can be saved to the DB.
   *
   * Example: converts [4.0.1.b] to [D.1045].
   */
  def normaliseRefs(text: String, refs: Map[LocalStepId, StepLabel], savedSteps: SavedSteps): TextWithNormalisedRefs = {
    val localToDb = savedSteps.ba
    var r = text
    for {
      (localId, label) <- refs
      dataId <- localToDb.get(localId)
    } r = r.replace(makeRef(label), makeNormalisedRef(dataId))
    r.hasNormalisedRefs
  }

  /**
   * Realises (un-normalises) DB-id-based step references in text, to label based references.
   *
   * Example: converts [D.1045] to [4.0.1.b].
   *
   * @param text Text with all step references normalised with DB data IDs instead of human-readable labels.
   * @return Text with all step references
   */
  def realiseNormalisedRefs(text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps,
    stepsAndLabels: StepAndLabelBiMap): String = {

    val dbIdsToLocalIds = savedSteps.ab
    lazy val localIdsToLabels = stepsAndLabels.value.ab

    NormalisedRefRegex.replaceAllIn(text, m => {
      val idText = m.group(1)
      val textIdentId = idText.toLong.tag[IsTextIdentId]
      dbIdsToLocalIds.get(textIdentId).flatMap(nodeId => localIdsToLabels.get(nodeId)).map(makeRef(_)).getOrElse {
        warn(s"Unable to realise normalised step reference. ❚ Text: $text ❚ TextIdentId: $textIdentId ❚ DbIdsToLocalIds: $dbIdsToLocalIds ❚ LocalIdsToLabels: $localIdsToLabels")
        makeInvalidNormalisedRef(idText)
      }
    })
  }
}
