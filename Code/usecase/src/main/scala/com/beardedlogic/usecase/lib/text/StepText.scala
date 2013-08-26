package com.beardedlogic.usecase.lib.text

import scala.collection.immutable.TreeSet
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._
import Changes._
import ParsingUtils._

case class StepTextFactory(stepId: LocalStepId) extends Parser[StepText] {
  override def empty = StepText.empty(stepId)

  override def load(text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps, stepsAndLabels: StepAndLabelBiMap) =
    StepText.load(stepId, text)

  override def parse(text: String)(implicit stepsAndLabels: StepAndLabelBiMap) =
    StepText.parse(stepId, text)
}

object StepText {
  def correctInput(input: String): String = input.trim

  def empty(stepId: LocalStepId) = StepText(stepId, FreeText.empty, None, None)

  def load(stepId: LocalStepId, text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps, stepsAndLabels: StepAndLabelBiMap) = {
    val e = empty(stepId)
    e.updateCorrected(realiseNormalisedRefs(text)).getOrElse(e)
  }

  def parse(stepId: LocalStepId, text: String)(implicit stepsAndLabels: StepAndLabelBiMap) = {
    val e = empty(stepId)
    e.update(text).getOrElse(e)
  }
}

case class StepText(
  stepId: LocalStepId,
  mainClause: FreeText,
  flowFromClause: Option[FlowFromClause],
  flowToClause: Option[FlowToClause])
  extends ParsedText[StepText] {

  override val text = {
    var t = List.empty[String]
    if (mainClause.text.nonEmpty) t :+= mainClause.text
    for (c <- flowFromClause) t :+= FlowFrom.toText(c)
    for (c <- flowToClause) t :+= FlowTo.toText(c)
    t.mkString(" ")
  }

  lazy val allRefs = {
    var m = mainClause.refs
    for (c <- flowFromClause) m ++= c.refs
    for (c <- flowToClause) m ++= c.refs
    m
  }

  override def textWithNormalisedRefs(implicit savedSteps: SavedSteps) = normaliseRefs(text, allRefs, savedSteps)

  override val hasRefs_? = mainClause.hasRefs_? || flowHasRefs_?(flowFromClause) || flowHasRefs_?(flowToClause)

  override protected def correctInput(input: String) = StepText.correctInput(input)

  protected def textChanged = StepTextChanged(stepId)

  override protected[text] def updateCorrected(newText: String)(implicit stepsAndLabels: StepAndLabelBiMap) = {
    val (text, from, to, msgs) = parseTextForFlow(newText)
    val main = mainClause.update(invalidateFlowArrows(text)).getOrElse(mainClause)
    val newVal = copy(mainClause = main, flowFromClause = from, flowToClause = to)
    Changed(newVal, textChanged ++ msgs)
  }

  def updateMainClause(newMainClauseText: String)(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[StepText, Change] =
    mainClause.
    update(newMainClauseText).
    mapChanges(_.map(convertFreeTextChange)).
    map(m => copy(mainClause = m))

  /**
   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
   *
   * If found (and valid), they are extracted and normalised.
   */
  private def parseTextForFlow(input: String)(implicit stepsAndLabels: StepAndLabelBiMap)
  : (String, Option[FlowFromClause], Option[FlowToClause], List[Change]) = {

    // Parse flow clauses
    val pr = Grammar.parseAll(Grammar.TextAndFlow, input)
    val (text, fromLabels: Option[List[LabelStr]], toLables: Option[List[LabelStr]]) =
      if (pr.successful) {
        // Clauses exist. Validate.
        val (text, flowResult) = pr.get
        (text, flowResult.from.asLabels, flowResult.to.asLabels)
      } else
      // Free text only
        (input, None, None)

    // Transition to new clauses
    val parsedTo = transitionFlowClause(FlowTo, flowToClause, toLables)
    val parsedFrom = if (parsedTo.isEmpty) None else transitionFlowClause(FlowFrom, flowFromClause, fromLabels)

    if (parsedFrom.isDefined && parsedTo.isDefined) {
      // No errors in From or To clauses
      val (newFrom, msgsFrom) = parsedFrom.get
      val (newTo, msgsTo) = parsedTo.get
      (text, newFrom, newTo, msgsFrom ::: msgsTo)
    } else
    // One or both flow clauses are invalid
    // TODO stop discarding a valid flow clause when the other is invalid
    // TODO broken flow doesn't broadcast change
      (input, None, None, List.empty)
  }

  private def transitionFlowClause[C <: FlowClause, F <: Flow[C]](flow: F, oldClause: Option[C]
    , labelsFoundInNewFlow: Option[List[LabelStr]])(implicit stepsAndLabels: StepAndLabelBiMap)
  : Option[(Option[C], List[Change])] =

    labelsFoundInNewFlow match {
      // No flow specified
      case None =>
        val changes = oldClause.map(_ => flow.flowClearedChangeFn(stepId)).toList
        Some(None, changes)

      // Flow specified and valid
      case Some(labels) if areLabelsValid_?(labels) =>
        val labelsToIds = stepsAndLabels.get.ba
        val newRefs = labels.map(l => (labelsToIds(l), l)).toMap
        oldClause.filter(_.refs == newRefs).map(clause =>
        // No changes
          Some(Some(clause), List.empty[Change])
        ).getOrElse {
          // Change detected
          val clause = flow.create(newRefs).get
          val changes = clause.flowChangeFn(stepId)
          Some(Some(clause), List(changes))
        }

      // Flow specified and invalid
      case _ => None
    }


  //  /**
//   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
//   *
//   * If found (and valid), they are extracted and normalised.
//   */
//  private def parseTextForFlow(input: String)(implicit stepsAndLabels: StepAndLabelBiMap)
//  : (String, Option[FlowFromClause], Option[FlowToClause], Set[Change]) = {
//
//    // Parse flow clauses
//    val pr = Grammar.parseAll(Grammar.TextAndFlow, input)
//    val (text, fromLabels: Option[List[LabelStr]], toLables: Option[List[LabelStr]]) =
//      if (pr.successful) {
//        // Clauses exist. Validate.
//        val (text, flowResult) = pr.get
//        (text, flowResult.from.asLabels, flowResult.to.asLabels)
//      } else
//      // Free text only
//        (input, None, None)
//
//    // Transition to new clauses
//    val parsedTo = transitionFlowClause(FlowTo, flowToClause, toLables)
//    val parsedFrom = transitionFlowClause(FlowFrom, flowFromClause, fromLabels)
//
//    (parsedFrom, parsedTo) match {
//      case (NoChange,              NoChange)          => NoChange
//      case (NoChange,              Changed(toV, toC)) => Changed((text, flowFromClause, toV), toC)
//      case (Changed(fromV, fromC), NoChange)          => Changed((text, fromV, flowToClause), fromC)
//      case (Changed(fromV, fromC), Changed(toV, toC)) => Changed((text, fromV, toV), fromC ++ toC)
//      // TODO stop discarding a valid flow clause when the other is invalid
//      // TODO broken flow doesn't broadcast change
//      case (ChangeFailure(_), _) | (_, ChangeFailure(_)) => (input, None, None, Set.empty)
//    }
//  }
//
//  private def transitionFlowClause[C <: FlowClause, F <: Flow[C]](flow: F, oldClause: Option[C]
//    , labelsFoundInNewFlow: Option[List[LabelStr]])(implicit stepsAndLabels: StepAndLabelBiMap)
//  : ChangeResultF[Option[C], Change] =
//
//    labelsFoundInNewFlow match {
//      // No flow specified
//      case None =>
//        val changes = oldClause.map(_ => flow.clearMsg(stepId)).toSet
//        Changed(None, changes)
//
//      // Flow specified and valid
//      case Some(labels) if areLabelsValid_?(labels) =>
//        val labelsToIds = stepsAndLabels.get.ba
//        val newRefs = labels.map(l => (labelsToIds(l), l)).toMap
//        oldClause.filter(_.refs == newRefs).map(clause => NoChange).getOrElse {
//          // Change detected
//          val clause = flow.create(newRefs).get
//          val msg = clause.flowChangeMsg(stepId)
//          Changed(Some(clause), Set(msg))
//        }
//
//      // Flow specified and invalid // TODO Shouldn't this be a change failure?
//      case _ => ChangeFailure("Invalid flow.")
//    }

  override def respondToChange(c: Change)(implicit stepsAndLabels: StepAndLabelBiMap) = c match {

    // Update step references when they change
    case _: ExistingStepLabelsChanged => updateRefs

    // Add or Remove flow references
    case FlowFromChange(fromIds, id) => processFlowChange(FlowTo, flowToClause, withFlowTo, fromIds, id)
    case FlowToChange(id, toIds) => processFlowChange(FlowFrom, flowFromClause, withFlowFrom, toIds, id)

    case _ => NoChange
  }

  private def withFlowFrom(from: Option[FlowFromClause]) = copy(flowFromClause = from)
  private def withFlowTo(to: Option[FlowToClause]) = copy(flowToClause = to)

  def updateRefs(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[StepText, Change] = {
    val main = mainClause.updateRefs.mapChanges(_.map(convertFreeTextChange))
    val from = updateRefs(FlowFrom, flowFromClause)
    val to = updateRefs(FlowTo, flowToClause)

    ChangeResult.map3(main, from, to)(mainClause, flowFromClause, flowToClause)(
      (m, f, t) => copy(mainClause = m, flowFromClause = f, flowToClause = t))
    }

  private def convertFreeTextChange(c: Change): Change = c match {
    case TextChanged => textChanged
    case _ => c
  }

  /**
   * Removes invalid references and creates a new flow clause (text).
   *
   * @return A new (different) flow clause. If no change is required, then `None` is returned.
   */
  private def updateRefs[C <: FlowClause, F <: Flow[C]](f: F, co: Option[C])(implicit stepsAndLabels: StepAndLabelBiMap)
  : ChangeResult[Option[C], Change] = co match {
    case Some(c) =>
      lazy val localIdsToLabels = stepsAndLabels.get.ab
      val changeFound = c.refs.exists {case (localId, label) => localIdsToLabels.get(localId).map(_ != label).getOrElse(true)}
      if (!changeFound) NoChange
      else {
        var newLabels = TreeSet.empty[LabelStr]
        var newRefs = Map.empty[LocalStepId, LabelStr]
        for ((id, _) <- c.refs)
          localIdsToLabels.get(id).map(l => {
            newRefs += (id -> l)
            newLabels += l
          }) // orElse: step deleted, just omit
        f.create(newRefs) @: textChanged
      }
    case _ => NoChange
  }

  private def processFlowChange[C <: FlowClause, F <: Flow[C]](
    f: F, co: Option[C], updateFn: Option[C] => StepText, manyIds: Set[LocalStepId], oneId: LocalStepId)
    (implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[StepText, Change] =

    if (oneId == stepId) NoChange
    else {
      val clauseRefs = co.map(_.refs).getOrElse(Map.empty)
      val a = manyIds.contains(stepId)
      val b = clauseRefs.contains(oneId)
      if (a == b) NoChange
      else {
        // Update required
        val newRefs = if (a)
          clauseRefs + (oneId -> stepsAndLabels.get.ab(oneId))
        else
          clauseRefs - oneId
        updateFn(f.create(newRefs)) @: textChanged
      }
    }
}
