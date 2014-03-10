package shipreq.webapp.feature.uc
package persist

import scala.annotation.tailrec
import shipreq.webapp.db._
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.ExternalId
import field._
import step.{StepNode, StepTree}
import text.StepText
import StepFieldConsts.StartingLabelIndices

object StepFieldPersistence {
  type SavedData = Map[TextIdentId, UcFieldText]
}

case class StepFieldPersistence(f: StepField) extends FieldPersistence[StepField] {
  override type SavedData = StepFieldPersistence.SavedData

  import f.{rec, sli, defaultLoadValue}

  override def saver(v: StepFieldValue, stepsAndLabels: StepAndLabelBiMap) =
    new StepFieldSaver(v, rec, sli, stepsAndLabels)

  // ===================================================================================================================

  override def load(loadCtx: FieldLoadCtx) = {

    val normTextMap = Map.newBuilder[LocalStepId, NormalisedText]
    val savedStepMap = Map.newBuilder[LocalStepId, TextIdentId]
    val savedData = Map.newBuilder[TextIdentId, UcFieldText]

    // NOTE: Depends on ORDER BY index
    def parseRels(allRels: List[UcFieldTextWithFK], level: Int, parentId: Option[TextRevId]): List[StepNode] = {
      @tailrec def go(rels: List[UcFieldTextWithFK], level: Int, labelIndex: Int, parentId: Option[TextRevId], nodes: List[StepNode]): List[StepNode] =
        rels match {
          // Done
          case Nil => nodes

          // New node
          case h :: t if (h.fkId == rec.id && h.parentId == parentId) =>
            val children = parseRels(allRels, level + 1, Some(h.id))
            val idStr = ExternalId.TextRev(h.id).asLocalStepId
            normTextMap += (idStr -> h.text)
            savedStepMap += (idStr -> h.textRev.identId)
            savedData += (h.textRev.identId -> h.rel)
            val newNodes = nodes :+ StepNode(idStr, level, labelIndex, children)
            go(t, level, labelIndex + 1, parentId, newNodes)

          // Noise, move on
          case h :: t  => go(t, level, labelIndex, parentId, nodes)
        }

      go(allRels, level, sli.startingLabelIndex(level), parentId, Nil)
    }

    val stepNodes = parseRels(loadCtx.fieldData, 0, None)
    if (stepNodes.isEmpty) {

      // Use default value
      val (stepTree, fn) = defaultLoadValue(loadCtx.header)
      FieldLoadResult[Value, SavedData](Map.empty, stepTree, (savedSteps, stepsAndLabels) => (fn(), None))

    } else {

      // Use loaded value
      val stepTree = StepTree(stepNodes)
      FieldLoadResult[Value, SavedData](savedStepMap.result, Some(stepTree), (savedSteps, stepsAndLabels) => {
        val textmap = for ((id, txt) <- normTextMap.result) yield (id -> StepText.load(txt)(savedSteps, stepsAndLabels))
        val sfv = StepFieldValue(f, stepTree, textmap)
        (sfv, Some(savedData.result))
      })
    }
  }
}

// =====================================================================================================================
import StepFieldPersistence.SavedData

/**
 * Saves a StepField to the database.
 *
 * @param v The field value to save.
 */
class StepFieldSaver(
  val v: StepFieldValue,
  val fieldKeyRec: FieldKeyRec,
  val sli: StartingLabelIndices,
  val stepsAndLabels: StepAndLabelBiMap
  ) extends FieldSaver[SavedData] {

  override def record_required_? = v.tree.nonEmpty

  override def differsFromPrevSave_?(prev: SavedData)(implicit savedSteps: SavedSteps): Boolean = {
    val labelsByLocalId = stepsAndLabels.value.ab
    val labelsToLocalId = stepsAndLabels.value.ba

    @inline def different_?(cur: StepNode, prev: UcFieldText) =
      labelsByLocalId(cur.id) != prev.label.get ||
      v.textmap(cur.id).normalisedText(savedSteps) != prev.text

    def anyNewOrModified = v.tree.iteratorRecursive.exists(n =>
      savedSteps.ba.get(n.id)
        .flatMap(txtIdentId => prev.get(txtIdentId))
        .map(prev => different_?(n, prev))
        .getOrElse(true)
    )

    def anyRemoved = prev.exists(_._2.label match {
      case None      => false
      case Some(lbl) => labelsToLocalId.get(lbl).map(!v.textmap.contains(_)).getOrElse(true)
    })

    anyNewOrModified || anyRemoved
  }

  override def presave(dao: DaoT, ucId: UseCaseIdentId, prevSavedSteps: Option[SavedSteps]): Map[LocalStepId, TextIdentId] = {

    var stepIds = Map.empty[LocalStepId, TextIdentId]

    def newStep(localId: LocalStepId): Unit = {
      val id = dao.createTextIdent(ucId, fieldKeyRec)
      stepIds += (localId -> id)
    }

    def presaveAll(): Unit = v.tree.foreachRecursive(n => newStep(n.id))

    def presaveNew(savedSteps: Map[LocalStepId, TextIdentId]): Unit = {
      v.tree.foreachRecursive(n =>
        savedSteps.get(n.id) match {
          case None    => newStep(n.id)
          case Some(_) =>
        }
      )
    }

    prevSavedSteps match {
      // No previous save, add everything for first time
      case None => presaveAll
      // Compare to previous and save deltas
      case Some(savedSteps) => presaveNew(savedSteps.ba)
    }

    stepIds
  }

  override def save(dao: DaoT, ucId: UseCaseIdentId, ucRevId: UseCaseRevId, prevSave: Option[SavedData])
    (implicit savedSteps: SavedSteps): SavedData = {

    val labelLookup = stepsAndLabels.value.ab
    val textIdentIds = savedSteps.ba
    val prevSaveData = prevSave.getOrElse(Map.empty)

    def saveStep(step: StepNode): TextRev = {
      val stepText = v.getNormalisedText(step.id)
      val textIdentId = textIdentIds(step.id)
      prevSaveData.get(textIdentId) match {
        // TextRev reusable!
        case Some(rel) if rel.text == stepText => rel.textRev
        // Text changed, save next rev
        case Some(rel) => dao.createTextRev(rel.textRev.identId, (rel.textRev.rev + 1).toShort, stepText)
        // New step, save rev #1
        case None => dao.createTextRev(textIdentId, 1:Short, stepText)
      }
    }

    var savedData = prevSaveData
    def saveAndLinkSteps(steps: List[StepNode], parent: Option[TextRevId]): Unit = {
      var index = 0
      for (step <- steps) {
        val textRev = saveStep(step)
        val label = labelLookup(step.id)
        val rel = dao.linkUcToStep(ucRevId, label, index.toShort, parent, textRev)
        savedData += (textRev.identId -> rel)
        saveAndLinkSteps(step.children, Some(textRev.id))
        index += 1
      }
    }

    saveAndLinkSteps(v.tree.nodes, None)

    savedData
  }
}
