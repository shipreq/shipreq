package com.beardedlogic.usecase
package feature.uc

import net.liftweb.util.TimeHelpers.logTime
import app.Defaults
import db._
import field._
import lib.Locks.{SingleUseCase, UseCaseNumbers}
import lib.Types._
import step.StepTree
import util.{PreparedLock, BiMap, Lock}
import UseCaseFns._

// ---------------------------------------------------------------------------------------------------------------------

case class FieldLoadCtx(header: UseCaseHeader, fieldData: List[UcFieldTextWithFK])

// ---------------------------------------------------------------------------------------------------------------------

case class FieldLoadResult[+V <: Field#Value, +SD <: Field#SavedData](
  savedSteps: Map[LocalStepId, TextIdentId],
  stepTree: Option[StepTree],
  phase2: (SavedSteps, UcParsingCtx) => (V, Option[SD]))

object FieldLoadResult {
  def noSteps[V <: Field#Value, SD <: Field#SavedData](phase2: (SavedSteps, UcParsingCtx) => (V, Option[SD])) =
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

  def load(ucRev: UseCaseRev, dao: DaoT, lock: Lock.Read[UseCaseNumbers]): (UseCaseSaveCheckpoint, UseCaseRelations) = {

    @inline def uch = ucRev.header
    @inline def ucn = ucRev.ident.number
    @inline def projectId = ucRev.ident.projectId
    val fieldList = Defaults.fieldList.value.fields // TODO hardcoded fieldlist
    val loadCtx = FieldLoadCtx(uch, dao.findAllUcFieldData(ucRev.id))

    var loadResults = List.empty[(Field, FieldLoadResult[Field#Value, Field#SavedData])]
    var stepAndLabelMaps = List.empty[Map[LocalStepId, StepLabel]]
    var savedStepMap = Map.empty[LocalStepId, TextIdentId]

    for (f <- fieldList) {
      val r = f.load(loadCtx)
      loadResults +:= (f -> r)
      for (tree <- r.stepTree) stepAndLabelMaps +:= generateStepAndLabelMap(ucn, f, tree)
      if (r.savedSteps.nonEmpty) savedStepMap ++= r.savedSteps
    }

    val savedSteps: SavedSteps = BiMap.swapped(savedStepMap)
    val stepAndLabels = generateStepAndLabelBiMap(stepAndLabelMaps)
    val fieldValues = Map.newBuilder[Field, Field#Value]
    val savedData = Map.newBuilder[Field, Field#SavedData]
    val rels = CachedUseCaseRelations(dao.summariseUseCases(projectId))
    val ctx = UcParsingCtx(ucn, uch.title, stepAndLabels, rels)

    for ((f, r) <- loadResults) {
      val (fv, sdOpt) = r.phase2(savedSteps, ctx)
      fieldValues += (f -> fv)
      for (sd <- sdOpt) savedData += (f -> sd)
    }

    val uc = UseCase(ucn, uch, fieldList, fieldValues.result, stepAndLabels)
    val cp = UseCaseSaveCheckpoint(uc, ucRev, savedSteps, savedData.result)

    (cp, rels)
  }

  def loadAll(projectId: ProjectId, dao: DaoT, lock: Lock.Read[UseCaseNumbers]): List[UseCase] =
    loadAll(dao.findAllLatestUseCaseRevsByProject(projectId), dao, lock)

  def loadAll(ucRevs: List[UseCaseRev], dao: DaoT, lock: Lock.Read[UseCaseNumbers]): List[UseCase] =
    logTime(s"UseCasePersistence.loadAll(${ucRevs.size} UCs)")(
      ucRevs.map(load(_, dao, lock)._1.uc)
    )

  // ===================================================================================================================

  /**
   * Saves the use case.
   *
   * Does nothing if there are differences between the current UC, and the last-saved revision.
   *
   * @return A checkpoint is there was anything to save, else `None` if UC was already up-to-date.
   */
  def save(uc: UseCase, prevSave: UseCaseSaveCheckpoint, preparedLock: PreparedLock.Write[SingleUseCase], dao: DaoT) : Option[UseCaseSaveCheckpoint] = {
    type ValueSavers = Map[Field, FieldValueSaver[Field#SavedData]]

    val allSavers: ValueSavers =
      uc.fieldValues.map { case (f, v_) =>
        val v = f.castV(v_)
        val s = f.saver(v, uc.stepsAndLabels)
        (f pairS2 s)
      }.toMap

    def getPrevSaveDataFor[F <: Field, S <: f.SavedData forSome {val f : F}](f: F): Option[S] =
      prevSave.savedData.get(f).asInstanceOf[Option[S]]

    def isSaveRequired_?(savers: ValueSavers) : Boolean = {

      def isSaveRequired_?(cp: UseCaseSaveCheckpoint): Boolean =
        (uc.header != cp.uc.header) || uc.fields.exists(fieldRequiresSave_?(cp))

      def fieldRequiresSave_?(cp: UseCaseSaveCheckpoint)(f: Field): Boolean = {
        val saver = f.castS2(savers(f))
        cp.savedData.get(f) match {
          case Some(sd_) => saver.differsFromPrevSave_?(f.castS(sd_))(cp.savedSteps)
          case None      => saver.record_required_?
        }
      }

      isSaveRequired_?(prevSave)
    }

    def selectFieldsRequiringSave(savers: ValueSavers): ValueSavers =
      savers.filter {case (f, s) => getPrevSaveDataFor(f).isDefined || s.record_required_?}

    def saveUcHeader(): UseCaseRev =
      dao.createUseCaseRev(prevSave.rec.ident, (prevSave.rec.rev + 1).toShort, uc.header)

    def presave(ucId: UseCaseIdentId, savers: ValueSavers): SavedSteps = {
      var newSavedSteps: Map[LocalStepId, TextIdentId] = prevSave.savedSteps.ba
      val someSavedSteps = Some(prevSave.savedSteps)
      for ((f, s) <- savers) newSavedSteps ++= s.presave(dao, ucId, someSavedSteps)
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

    def performSave(lock: Lock.Write[SingleUseCase]): UseCaseSaveCheckpoint = {
      val ucRev = saveUcHeader()
      val savers = selectFieldsRequiringSave(allSavers)
      implicit val newSavedSteps = presave(ucRev.identId, savers)
      val savedData = save(savers, ucRev.identId, ucRev.id)
      UseCaseSaveCheckpoint(uc, ucRev, newSavedSteps, savedData)
    }

    if (isSaveRequired_?(allSavers))
      Some(preparedLock(performSave))
    else
      None
  }
}
