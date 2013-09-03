package com.beardedlogic.usecase.lib.text

import scala.collection.immutable.TreeSet
import scalaz.{NonEmptyList, Cord}
import com.beardedlogic.usecase.lib.Misc.{CordExt, SingleSpace}
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._
import com.beardedlogic.usecase.lib.text.Grammar.{InvalidRefToken, PotentiallyValidRef, ParsedFlowClause}
import com.beardedlogic.usecase.lib.text.ParsingConfig.{FlowToStyle, FlowFromStyle, makeInvalidRef}
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

/**
 * Represents the value of a single step in a `StepField`.
 */
case class StepText(
  stepId: LocalStepId,
  mainClause: FreeText,
  flowFromClause: Option[FlowFromClause],
  flowToClause: Option[FlowToClause])
  extends ParsedText[StepText] {

  override val text = {
    var t = List.empty[String]
    for (c <- flowToClause) t ::= FlowTo.toText(c)
    for (c <- flowFromClause) t ::= FlowFrom.toText(c)
    if (mainClause.text.nonEmpty) t ::= mainClause.text
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
    val p = parseTextForFlow(newText)
    val (from, c1) = updateFlowClause(FlowFrom, flowFromClause, p.from)
    val (to, c2) = updateFlowClause(FlowTo, flowToClause, p.to)
    val changes = NonEmptyList.apply[Change](textChanged, c1 ::: c2: _*)
    val main = mainClause.update(invalidateFlowArrows(p.text)).getOrElse(mainClause)
    val newVal = copy(mainClause = main, flowFromClause = from, flowToClause = to)
    Changed(newVal, changes)
  }

  def updateMainClause(newMainClauseText: String)(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[StepText, Change] =
    mainClause.
    update(newMainClauseText).
    mapChanges(_.map(convertFreeTextChange)).
    map(m => copy(mainClause = m))

  def updateFlowClause[C <: FlowClause, F <: Flow[C]](flow: F, oldClause: Option[C], newRefs: Flow.Refs): (Option[C], List[Change]) = {
    val newClause = flow.create(newRefs)
    def change = List(flow.changeFor(stepId, newClause))
    val changes: List[Change] = (oldClause, newClause) match {
      case (None, None) => Nil
      case (None, Some(_)) => change
      case (Some(old), _) => if (old.refs == newRefs) Nil else change
    }
    (newClause, changes)
  }

  private case class TextAndRefsC(text: Cord, from: Flow.Refs, to: Flow.Refs)
  private case class TextAndRefsS(text: String, from: Flow.Refs, to: Flow.Refs)

  /**
   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
   *
   * If found (and valid), they are extracted and normalised.
   */
  private def parseTextForFlow(input: String)(implicit stepsAndLabels: StepAndLabelBiMap): TextAndRefsS = {

    val labelLookup = stepsAndLabels.get.ba

    def parseFlowClause(acc: TextAndRefsC, clause: ParsedFlowClause): TextAndRefsC = {

      def validateEachRef = {
        var bad = List.empty[Cord]
        var good: Flow.Refs = Map.empty
        for (token <- clause.refs)
          token match {
            case PotentiallyValidRef(lbl) =>
              labelLookup.get(lbl) match {
                case Some(stepId) => good += (stepId -> lbl)
                case None => bad ::= Cord(makeInvalidRef(lbl))
              }
            case InvalidRefToken(token) =>
              bad ::= Cord(token)
          }
        (bad, good)
      }

      val (bad, good) = validateEachRef

      // Add bad refs back into text
      val newText: Cord =
        if (bad.isEmpty) acc.text
        else {
          val badRefs = bad.foldRight(Cord.empty)((e, a) => a ++ SingleSpace ++ e)
          val badClause: Cord = clause.style.arrowBadReplacement +: badRefs
          val badClause2: Cord = if (acc.text.isEmpty) badClause else SingleSpace ++ badClause
          acc.text ++ badClause2
        }

      // Combine results
      if (good.isEmpty)
        acc.copy(text = newText)
      else clause.style match {
        case FlowFromStyle => acc.copy(text = newText, from = acc.from ++ good)
        case FlowToStyle => acc.copy(text = newText, to = acc.to ++ good)
      }
    }

    // Parse flow clauses
    val parseResult = Grammar.parseAll(Grammar.TextAndFlows, input)
    if (parseResult.successful) {

      // Flow clauses found
      val (text, flowClauses) = parseResult.get
      val z = TextAndRefsC(text, Map.empty, Map.empty)
      val r = flowClauses.foldLeft(z)(parseFlowClause)
      TextAndRefsS(r.text.toString, r.from, r.to)

    } else {

      // No flow clauses detected
      TextAndRefsS(input, Map.empty, Map.empty)
    }
  }

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
