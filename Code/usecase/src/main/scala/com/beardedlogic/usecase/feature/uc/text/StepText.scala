package com.beardedlogic.usecase.feature.uc.text

import scala.collection.immutable.TreeSet
import scalaz.{NonEmptyList, Cord}
import com.beardedlogic.usecase.db.{FieldKeyType, FieldKeyRec}
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import com.beardedlogic.usecase.feature.uc.change._
import com.beardedlogic.usecase.feature.uc.field.{NormalCourseField, StepField}
import com.beardedlogic.usecase.feature.uc.text.ParsingConfig.{FlowToStyle, FlowFromStyle, makeInvalidStepRef}
import com.beardedlogic.usecase.lib.Misc.SingleSpace
import com.beardedlogic.usecase.lib.Types._
import Changes._
import ParsingUtils._

object StepText {
  def correctInput(input: String): String @@ InputCorrected =
    Validator.stepFieldText.correct(input)

  val empty = StepText(FreeText.empty, None, None)

  def load(text: NormalisedText)(implicit savedSteps: SavedSteps, ctx: UcParsingCtx): StepText = {
    implicit val stepsAndLabels = ctx.stepsAndLabels
    parseCorrected(realiseNormalisedStepRefs(text))
  }

  def parse(text: String)(implicit ctx: UcParsingCtx): StepText =
    parseCorrected(correctInput(text))

  def parseCorrected(text: String @@ InputCorrected)(implicit ctx: UcParsingCtx): StepText =
    fakeUpdater.updateCorrectedAndGet(empty, text)

  private val fakeStepField: StepField = NormalCourseField(FieldKeyRec(0.tag[IsFieldKeyId], FieldKeyType.NormalAndAlternateCourses, None))
  private val fakeStepId: LocalStepId = "".asLocalStepId
  private val fakeUpdater = new StepTextUpdater(fakeStepField, fakeStepId)
}

// =====================================================================================================================

case class StepText(mainClause: FreeText, flowFromClause: Option[FlowFromClause], flowToClause: Option[FlowToClause]) extends ParsedText {

  override val text = {
    var t = List.empty[String]
    for (c <- flowToClause) t ::= FlowTo.toText(c)
    for (c <- flowFromClause) t ::= FlowFrom.toText(c)
    if (mainClause.text.nonEmpty) t ::= mainClause.text
    t.mkString(" ")
  }

  lazy val allStepRefs = {
    var m = mainClause.stepRefs
    for (c <- flowFromClause) m ++= c.refs
    for (c <- flowToClause) m ++= c.refs
    m
  }

  override def normalisedText(implicit savedSteps: SavedSteps) = normalise(text, allStepRefs, savedSteps)

  val hasStepRefs_? = mainClause.hasStepRefs_? || flowHasRefs_?(flowFromClause) || flowHasRefs_?(flowToClause)
}

// =====================================================================================================================

class StepTextUpdater(field: StepField, stepId: LocalStepId) extends ParsedTextUpdater[StepText] with SeqChangeResponder[StepText] {
  private val textChanged = StepTextChanged(field, stepId)
  private def mainClauseUpdater = new FreeTextUpdater(textChanged)
  private def setMainClause(t: StepText) = (ft: FreeText) => t.copy(mainClause = ft)
  private def setFlowFrom(t: StepText) = (from: Option[FlowFromClause]) => t.copy(flowFromClause = from)
  private def setFlowTo(t: StepText) = (to: Option[FlowToClause]) => t.copy(flowToClause = to)
  private def convert(t: StepText, f: FreeText => ChangeResult[FreeText, Change]): ChangeResult[StepText, Change] = f(t.mainClause).mapValue(setMainClause(t))

  override def correctInput(input: String) = StepText.correctInput(input)

  def updateMainClause(t: StepText, newMainClauseText: String)(implicit ctx: UcParsingCtx): ChangeResult[StepText, Change] =
    convert(t, mainClauseUpdater.update(_, newMainClauseText))

  override def updateCorrected(t: StepText, newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx) = {
    val p          = parseTextForFlow(newText)
    val (from, c1) = updateFlowClause(FlowFrom, t.flowFromClause, p.from)
    val (to, c2)   = updateFlowClause(FlowTo, t.flowToClause, p.to)
    val changes    = NonEmptyList.apply[Change](textChanged, c1 ::: c2: _*)
    val main       = mainClauseUpdater.updateAndGet(t.mainClause, invalidateFlowArrows(p.text))
    val newVal     = t.copy(mainClause = main, flowFromClause = from, flowToClause = to)
    Changed(newVal, changes)
  }

  private case class TextAndRefsC(text: Cord, from: Flow.Refs, to: Flow.Refs)
  private case class TextAndRefsS(text: String, from: Flow.Refs, to: Flow.Refs)

  /**
   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
   *
   * If found (and valid), they are extracted and normalised.
   */
  private def parseTextForFlow(input: String)(implicit ctx: UcParsingCtx): TextAndRefsS = {
    import Grammar.FlowParsers._

    val labelsToIds = ctx.getLabelsToIds

    def parseFlowClause(acc: TextAndRefsC, clause: ParsedFlowClause): TextAndRefsC = {

      def validateEachRef = {
        var bad = List.empty[Cord]
        var good: Flow.Refs = Map.empty
        for (token <- clause.refs)
          token match {
            case PotentiallyValidRef(lbl) =>
              labelsToIds.get(lbl) match {
                case Some(stepId) => good += (stepId -> lbl)
                case None => bad ::= Cord(makeInvalidStepRef(lbl))
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
    val parseResult = Grammar.parseAll(TextAndFlowClauses, input)
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

  private def updateFlowClause[C <: FlowClause, F <: Flow[C]](flow: F, oldClause: Option[C], newRefs: Flow.Refs): (Option[C], List[Change]) = {
    val newClause = flow.create(newRefs)
    def change = List(flow.changeFor(stepId, newClause))
    val changes: List[Change] = (oldClause, newClause) match {
      case (None, None) => Nil
      case (None, Some(_)) => change
      case (Some(old), _) => if (old.refs == newRefs) Nil else change
    }
    (newClause, changes)
  }

  // -------------------------------------------------------------------------------------------------------------------

  override def respondToChange(t: StepText, c: Change)(implicit ctx: UcParsingCtx) = c match {
    // Update step references when they change
    case _: ExistingStepLabelsChanged => updateAfterStepTreeChange(t)

    // Add or Remove flow references
    case FlowFromChange(fromIds, id) => processFlowChange(FlowTo, t.flowToClause, setFlowTo(t), fromIds, id)
    case FlowToChange(id, toIds) => processFlowChange(FlowFrom, t.flowFromClause, setFlowFrom(t), toIds, id)

    // Delegate to the main clause
    case _ => convert(t, mainClauseUpdater.respondToChange(_, c))
  }

  def updateAfterStepTreeChange(t: StepText)(implicit ctx: UcParsingCtx): ChangeResult[StepText, Change] = {
    val main = mainClauseUpdater.updateAfterStepTreeChange(t.mainClause)
    val from = updateAfterStepTreeChange(FlowFrom, t.flowFromClause)
    val to = updateAfterStepTreeChange(FlowTo, t.flowToClause)

    ChangeResult.map3(main, from, to)(t.mainClause, t.flowFromClause, t.flowToClause)(
      (m, fr, to) => t.copy(mainClause = m, flowFromClause = fr, flowToClause = to))
    }


  /**
   * Removes invalid references and creates a new flow clause (text).
   *
   * @return A new (different) flow clause. If no change is required, then `None` is returned.
   */
  private def updateAfterStepTreeChange[C <: FlowClause, F <: Flow[C]](f: F, co: Option[C])(implicit ctx: UcParsingCtx)
  : ChangeResult[Option[C], Change] = co match {
    case Some(c) =>
      val localIdsToLabels = ctx.getIdsToLabels
      val changeFound = c.refs.exists {case (localId, label) => localIdsToLabels.get(localId).map(_ != label).getOrElse(true)}
      if (!changeFound) NoChange
      else {
        var newLabels = TreeSet.empty[StepLabel]
        var newRefs = Map.empty[LocalStepId, StepLabel]
        for ((id, _) <- c.refs)
          localIdsToLabels.get(id).map(l => {
            newRefs += (id -> l)
            newLabels += l
          }) // orElse: step deleted, just omit
        f.create(newRefs) @: textChanged
      }
    case None => NoChange
  }

  private def processFlowChange[C <: FlowClause, F <: Flow[C]](
    f: F, co: Option[C], updateFn: Option[C] => StepText, manyIds: Set[LocalStepId], oneId: LocalStepId)
    (implicit ctx: UcParsingCtx): ChangeResult[StepText, Change] =

    if (oneId == stepId) NoChange
    else {
      val clauseRefs = co.map(_.refs).getOrElse(Map.empty)
      val a = manyIds.contains(stepId)
      val b = clauseRefs.contains(oneId)
      if (a == b) NoChange
      else {
        // Update required
        val newRefs = if (a)
          clauseRefs + (oneId -> ctx.getIdsToLabels(oneId))
        else
          clauseRefs - oneId
        updateFn(f.create(newRefs)) @: textChanged
      }
    }
}