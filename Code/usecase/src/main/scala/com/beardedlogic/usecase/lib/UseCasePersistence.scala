package com.beardedlogic.usecase
package lib

import Types._
import field._
import model._
import util.BiMap
import UseCaseFns._

// ---------------------------------------------------------------------------------------------------------------------

case class FieldLoadCtx(fieldData: List[UcFieldTextWithFK])

// ---------------------------------------------------------------------------------------------------------------------

case class FieldLoadResult[+V <: Field#Value, +SD <: Field#SavedData](
  savedSteps: Map[LocalIdStr, TextIdentId],
  stepTree: Option[StepTree],
  phase2: (SavedSteps, StepAndLabelBiMap) => (V, Option[SD]))

object FieldLoadResult {
  def noSteps[V <: Field#Value, SD <: Field#SavedData](phase2: (SavedSteps, StepAndLabelBiMap) => (V, Option[SD])) =
    FieldLoadResult(Map.empty, None, phase2)
}

// ---------------------------------------------------------------------------------------------------------------------

case class UseCaseSaveCheckpoint(
  uc: UseCase,
  rec: UseCaseRev,
  savedSteps: SavedSteps,
  savedData: Map[Field, Field#SavedData])

// ---------------------------------------------------------------------------------------------------------------------

object UseCasePersistence {

  def load(ucRev: UseCaseRev, dao: DAO, lock: Locks.ReadLockToken): UseCaseSaveCheckpoint = {

    @inline def uch = ucRev.header
    val fieldList = Defaults.FieldList.get.fields // TODO hardcoded fieldlist
    val loadCtx = FieldLoadCtx(dao.findAllUcFieldData(ucRev.id))

    var loadResults = List.empty[(Field, FieldLoadResult[Field#Value, Field#SavedData])]
    var stepAndLabelMaps = List.empty[Map[LocalIdStr, LabelStr]]
    var savedStepMap = Map.empty[LocalIdStr, TextIdentId]

    for (f <- fieldList) {
      val r = f.load(loadCtx)
      loadResults +:= (f -> r)
      for (tree <- r.stepTree) stepAndLabelMaps +:= generateStepAndLabelMap(f, tree, uch)
      if (r.savedSteps.nonEmpty) savedStepMap ++= r.savedSteps
    }

    val savedSteps: SavedSteps = BiMap.swapped(savedStepMap)
    val stepAndLabels = generateStepAndLabelBiMap(stepAndLabelMaps)
    val fieldValues = Map.newBuilder[Field, Field#Value]
    val savedData = Map.newBuilder[Field, Field#SavedData]

    for ((f, r) <- loadResults) {
      val (fv, sdOpt) = r.phase2(savedSteps, stepAndLabels)
      fieldValues += (f -> fv)
      for (sd <- sdOpt) savedData += (f -> sd)
    }

    val uc = UseCase(uch, fieldList, fieldValues.result, stepAndLabels)
    val cp = UseCaseSaveCheckpoint(uc, ucRev, savedSteps, savedData.result)

    cp
  }

  // ===================================================================================================================

  /**
   * Saves the use case.
   *
   * Does nothing if there are differences between the current UC, and the last-saved revision.
   *
   * @return A checkpoint is there was anything to save, else `None` if UC was already up-to-date.
   */
  def save(uc: UseCase, prevSave: Option[UseCaseSaveCheckpoint], dao: DAO): Option[UseCaseSaveCheckpoint] = {
    type ValueSavers = Map[Field, FieldValueSaver[_]]

    val allSavers: ValueSavers =
      uc.fieldValues.map { case (f, v_) =>
        val v = f.castValue(v_)
        val s = f.valueSaver(v, uc.stepsAndLabels)
        (f -> s)
      }.toMap

    def getPrevSaveDataFor[F <: Field, S <: f.SavedData forSome {val f : F}](f: F): Option[S] =
      prevSave.flatMap(_.savedData.get(f).asInstanceOf[Option[S]])

    def isSaveRequired_?(savers: ValueSavers) : Boolean = {

      def isSaveRequired_?(cp: UseCaseSaveCheckpoint): Boolean =
        (uc.header != cp.uc.header) || uc.fields.exists(fieldRequiresSave_?(cp))

      def fieldRequiresSave_?(cp: UseCaseSaveCheckpoint)(f: Field): Boolean = {
        val saver = f.saver(savers)
        cp.savedData.get(f) match {
          case Some(sd_) => saver.differsFromPrevSave_?(f.castSavedData(sd_))(cp.savedSteps)
          case _         => saver.record_required_?
        }
      }

      prevSave match {
        case Some(cp) => isSaveRequired_?(cp)
        case _ => true
      }
    }

    def selectFieldsRequiringSave(savers: ValueSavers): ValueSavers =
      savers.filter {case (f, s) => getPrevSaveDataFor(f).isDefined || s.record_required_?}

    def saveUcHeader(): UseCaseRev = prevSave match {
      case Some(cp) => dao.createUseCase(cp.rec.identId, (cp.rec.rev + 1).toShort, uc.header)
      case _ => dao.createInitialUseCase(uc.header)
    }

    def presave(ucId: UseCaseIdentId, savers: ValueSavers): SavedSteps = {
      val prevSavedSteps = prevSave.map(_.savedSteps)
      var newSavedSteps: Map[LocalIdStr, TextIdentId] = prevSavedSteps.map(_.ba).getOrElse(Map.empty)
      for ((f, s) <- savers) newSavedSteps ++= s.presave(dao, ucId, prevSavedSteps)
      BiMap.swapped(newSavedSteps)
    }

    def save(savers: ValueSavers, ucId: UseCaseIdentId, ucRevId: UseCaseRevId)(implicit savedSteps: SavedSteps): Map[Field, Field#SavedData] = {
      var savedData = Map.empty[Field, Field#SavedData]
      for ((f, s_) <- savers) {
        val s = s_.asInstanceOf[FieldValueSaver[f.SavedData]]
        val d = s.save(dao, ucId, ucRevId, getPrevSaveDataFor(f))
        savedData += (f -> d)
      }
      savedData
    }

    def withUseCaseWriteLock[R](fn: => R): R =
      prevSave.map(cp => Locks.UseCase.withWriteLock(cp.rec)(fn)).getOrElse(fn)

    def performSave(): UseCaseSaveCheckpoint =
      dao.withTransaction {
        val ucRev = saveUcHeader()
        val savers = selectFieldsRequiringSave(allSavers)
        implicit val newSavedSteps = presave(ucRev.identId, savers)
        val savedData = save(savers, ucRev.identId, ucRev.id)
        UseCaseSaveCheckpoint(uc, ucRev, newSavedSteps, savedData)
      }

    if (isSaveRequired_?(allSavers))
      Some(withUseCaseWriteLock(performSave))
    else
      None
  }
}
