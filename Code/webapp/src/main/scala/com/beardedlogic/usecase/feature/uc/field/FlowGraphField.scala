package com.beardedlogic.usecase.feature.uc.field

import scalaz.NonEmptyList
import com.beardedlogic.usecase.db.{FieldKeyRec, FieldKeyType}
import com.beardedlogic.usecase.feature.uc.change._
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import Changes._

// =====================================================================================================================

object FlowGraphFieldDefinition extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.FlowGraph
  override val fieldKeyData = None
  override def field(rec: FieldKeyRec) = FlowGraphField(rec)
}

// =====================================================================================================================

trait FlowGraphFieldLike {
  this: Field with FlowGraphField =>
  override type Value = Unit
  override def defn = FlowGraphFieldDefinition
  override def empty = ()
  override def toString = s"${getClass.getSimpleName}[#${rec.id}]"
  override def changeResponder = FlowGraphChangeResponder
}

// =====================================================================================================================

final object FlowGraphChangeResponder extends ChangeResponder[Unit] {

  def respondToChanges(u: Unit, changes: NonEmptyList[Change])(implicit ctx: UcParsingCtx) =
    if (requiresRedraw_?(changes))
      ChangeFlowGraph
    else
      NoChange

  @inline def requiresRedraw_?(changes: NonEmptyList[Change]): Boolean =
    requiresRedraw(changes.head) || changes.tail.exists(requiresRedraw)

  val requiresRedraw: Change => Boolean =
    _ match {
      case _: ExistingStepLabelsChanged
           | FlowFromChange(_, _)
           | FlowToChange(_, _)
           | TailStepAdded(_, _) => true
      case TitleChanged(_, _)
           | TextChanged(_)
           | StepTextChanged(_, _)
           | FlowGraphChanged => false
    }

  val ChangeFlowGraph: R = Changed((), NonEmptyList(FlowGraphChanged))
}