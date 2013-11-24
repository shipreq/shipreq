package com.beardedlogic.usecase.feature.uc.text

import scala.collection.immutable.{SortedSet, TreeSet}
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.uc.change.Change
import com.beardedlogic.usecase.feature.uc.change.Changes.{FlowToChange, FlowFromChange}
import ParsingConfig.{FlowToStyle, FlowFromStyle, FlowStyle}

object Flow {
  type Refs = Map[LocalStepId, StepLabel]
}
import Flow.Refs

sealed trait Flow[Clause <: FlowClause] {

  def style: FlowStyle
  protected def justCreate(refs: Refs): Clause
  protected def change(stepId: LocalStepId, refs: Refs): Change

  def create(refs: Refs): Option[Clause] =
    if (refs.isEmpty) None
    else Some(justCreate(refs))

  def changeFor(stepId: LocalStepId, clause: Option[Clause]): Change = clause match {
    case None => change(stepId, Map.empty)
    case Some(c) => change(stepId, c.refs)
  }

  def toText(c: Clause) = style.makeFlowTextOrEmpty(c.sortedLabels)
}

sealed trait FlowClause {

  type Self <: FlowClause
  def flowObj: Flow[Self]

  val refs: Refs

  def sortedLabels: SortedSet[StepLabel] = {
    var s = TreeSet.empty[StepLabel]
    for (lbl <- refs.values) s += lbl
    s
  }
}

// ---------------------------------------------------------------------------------------------------------------------

case object FlowFrom extends Flow[FlowFromClause] {
  override def style = FlowFromStyle
  override protected def justCreate(refs: Refs) = FlowFromClause(refs)
  override protected def change(stepId: LocalStepId, refs: Refs) = FlowFromChange(refs.keySet, stepId)
}

case class FlowFromClause(refs: Refs) extends FlowClause {
  override type Self = FlowFromClause
  override def flowObj = FlowFrom
}

// ---------------------------------------------------------------------------------------------------------------------

case object FlowTo extends Flow[FlowToClause] {
  override def style = FlowToStyle
  override protected def justCreate(refs: Refs) = FlowToClause(refs)
  override protected def change(stepId: LocalStepId, refs: Refs) = FlowToChange(stepId, refs.keySet)
}

case class FlowToClause(refs: Refs) extends FlowClause {
  override type Self = FlowToClause
  override def flowObj = FlowTo
}