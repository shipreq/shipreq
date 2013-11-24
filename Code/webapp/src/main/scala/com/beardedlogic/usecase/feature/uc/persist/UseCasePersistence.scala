package com.beardedlogic.usecase
package feature.uc
package persist

import net.liftweb.util.TimeHelpers.logTime
import scalaz.{Functor, Foldable}

import app.Defaults
import db._
import field._
import lib.Locks.{SingleUseCase, UseCaseNumbers}
import lib.ScalazSubset._
import lib.Types._
import util.{PreparedLock, BiMap, Lock}
import UseCaseFns._

case class UseCaseSaveCheckpoint(
  uc: UseCase,
  rec: UseCaseRev,
  savedSteps: SavedSteps,
  savedData: UseCasePersistence.SavedDataByField) {
  @inline final def ucAndRev = (uc, rec)
}

final object UseCasePersistence {

  // -------------------------------------------------------------------------------------------------------------------
  // Madness for (relative) path-dep type-safety in a generic context

  type SD[F <: Field] = FieldPersistence[F]#SavedData
  type Saver[F <: Field] = FieldSaver[SD[F]]

  final class FieldAndDepVal[X[_ <: Field]](val f: Field, raw: X[_]) {
    def force[F <: Field]: X[F] = raw.asInstanceOf[X[F]]
    def get = force[f.type]
  }

  type FieldAndSavedData = FieldAndDepVal[SD]
  object FieldAndSavedData {
    def apply(f: Field)(sd: SD[f.type]) =
      new FieldAndDepVal[SD](f, sd)
  }

  type FieldAndSaver = FieldAndDepVal[Saver]
  object FieldAndSaver {
    def apply(f : Field)(p: FieldPersistence[f.type])(s: FieldSaver[p.SavedData]) =
      new FieldAndDepVal[Saver](f, s.asInstanceOf[Saver[f.type]])
  }

  final class FieldAndLoadResult(val f: Field, lr: FieldLoadResult[_, _]) {
    def get = lr.asInstanceOf[FieldLoadResult[f.type#Value, SD[f.type]]]
  }
  object FieldAndLoadResult {
    def apply(f : Field)(p: FieldPersistence[f.type])(r: FieldLoadResult[f.Value, p.SavedData]) =
      new FieldAndLoadResult(f, r)
  }

  def get[X[_ <: Field]](f: Field, xs: Iterable[FieldAndDepVal[X]]): Option[X[f.type]] =
    xs.find(_.f == f).map(_.force[f.type])

  // -------------------------------------------------------------------------------------------------------------------

  type SavedDataByField = List[FieldAndSavedData]

  def persist(field: Field): Option[FieldPersistence[field.type]] = {
    val fp: Option[FieldPersistence[_]] = field match {
      case f: TextField      => Some(TextFieldPersistence(f))
      case f: StepField      => Some(StepFieldPersistence(f))
      case f: FlowGraphField => None
    }
    fp.asInstanceOf[Option[FieldPersistence[field.type]]]
  }

  // ===================================================================================================================

  case class Loader[C[_] : Foldable : Functor](
    projectId: ProjectId,
     ucRevsFn: DaoT => ProjectId => C[UseCaseRev],
      loadCtx: DaoT => C[UseCaseRev] => UseCaseRev => FieldLoadCtx,
       relsFn: DaoT => ProjectId => UseCaseRelations = loadRels
    ) {

    def filter(f: C[UseCaseRev] => C[UseCaseRev]): Loader[C] =
      copy(ucRevsFn = {d => (f compose ucRevsFn(d))})

    def useRels(r: UseCaseRelations): Loader[C] =
      copy(relsFn = {_ => _ => r})

    def run(dao: DaoT, lock: Lock.Read[UseCaseNumbers]): C[UseCaseSaveCheckpoint] = logTime {
      val ucRevs   = ucRevsFn(dao)(projectId)
      val rels     = relsFn(dao)(projectId)
      val loadCtxs = loadCtx(dao)(ucRevs)
      val loadFn   = loadPure(rels) _
      val cps      = ucRevs.map(u => loadFn(u, loadCtxs(u)))
      (s"UseCasePersistence.load(${ucRevs.count} UCs)" ,cps)
    }
  }

  def load(ucRev: UseCaseRev) =
    Loader[Id](ucRev.projectId
      , _ => _ => ucRev
      , d => _ => u => FieldLoadCtx(u.header, d findAllUcFieldData u))

  def loadAll(projectId: ProjectId) =
    Loader[List](projectId
      , _.findAllLatestUseCaseRevsByProject
      , d => ucs => serveBulkLoadCtxs(d findAllUcFieldData ucs.map(_.id)))

  private val loadRels: DaoT => ProjectId => UseCaseRelations =
    d => p => CachedUseCaseRelations(d.summariseUseCases(p))

  private def serveBulkLoadCtxs(bulk: List[(UseCaseRevId, UcFieldTextWithFK)]): UseCaseRev => FieldLoadCtx =
    u => {
      val fieldData = bulk.foldRight(List.empty[UcFieldTextWithFK]) {
        case ((id, row), r) => if (id == u.id) row :: r else r
      }
      FieldLoadCtx(u.header, fieldData)
    }

  private def loadPure(rels: UseCaseRelations)(ucRev: UseCaseRev, loadCtx: FieldLoadCtx): UseCaseSaveCheckpoint = {
    @inline def uch = ucRev.header
    @inline def ucn = ucRev.ident.number
    val fieldList = Defaults.fieldList.value.fields // TODO hardcoded fieldlist

    var transientFields = List.empty[Field]
    var loadResults = List.empty[FieldAndLoadResult]
    var stepAndLabelMaps = List.empty[Map[LocalStepId, StepLabel]]
    var savedStepMap = Map.empty[LocalStepId, TextIdentId]
    for (f <- fieldList)
      persist(f) match {
        case Some(p) =>
          val r = p.load(loadCtx)
          loadResults ::= FieldAndLoadResult(f)(p)(r)
          for (tree <- r.stepTree)
            stepAndLabelMaps ::= generateStepAndLabelMap(ucn, f, tree)
          if (r.savedSteps.nonEmpty)
            savedStepMap ++= r.savedSteps

        case None =>
          transientFields ::= f
      }

    val savedSteps: SavedSteps = BiMap.swapped(savedStepMap)
    val stepAndLabels = generateStepAndLabelBiMap(stepAndLabelMaps)
    val ctx = UcParsingCtx(ucn, uch.title, stepAndLabels, rels)

    var savedData = List.empty[FieldAndSavedData]
    var fieldValues = Map.empty[Field, Field#Value]
    for (fr  <- loadResults) {
      val (fv, sdOpt) = fr.get.phase2(savedSteps, ctx)
      fieldValues += (fr.f -> fv)
      for (sd <- sdOpt)
        savedData ::= FieldAndSavedData(fr.f)(sd)
    }
    for (f <- transientFields)
      fieldValues += (f -> f.empty)

    val uc = UseCase(ucn, uch, fieldList, fieldValues, stepAndLabels)
    val cp = UseCaseSaveCheckpoint(uc, ucRev, savedSteps, savedData)
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
  def save(uc: UseCase, prevSave: UseCaseSaveCheckpoint, preparedLock: PreparedLock.Write[SingleUseCase], dao: DaoT) : Option[UseCaseSaveCheckpoint] = {
    type ValueSavers = Iterable[FieldAndSaver]

    val allSavers: ValueSavers =
      uc.fieldValues.toStream.map(ffv => {
        val f = ffv._1
        persist(f).map(p => {
          val v = f.castV(ffv._2)
          val s = p.saver(v, uc.stepsAndLabels)
          FieldAndSaver(f)(p)(s)
        })
      }).filter(_.isDefined).map(_.get)

    def getPrevSaveDataFor(f: Field) = get(f, prevSave.savedData)

    def isSaveRequired_?(savers: ValueSavers) : Boolean = {
      def isSaveRequired_?(cp: UseCaseSaveCheckpoint): Boolean =
        (uc.header != cp.uc.header) || uc.fields.exists(fieldRequiresSave_?(cp))

      def fieldRequiresSave_?(cp: UseCaseSaveCheckpoint)(f: Field): Boolean = {
        get(f, savers) match {
          case Some(saver) =>
            get(f, cp.savedData) match {
              case Some(sd) => saver.differsFromPrevSave_?(sd)(cp.savedSteps)
              case None     => saver.record_required_?
            }
          case None => false
        }
      }

      isSaveRequired_?(prevSave)
    }

    def selectFieldsRequiringSave(savers: ValueSavers): ValueSavers =
      savers.filter(r => getPrevSaveDataFor(r.f).isDefined || r.get.record_required_?)

    def saveUcHeader(): UseCaseRev =
      dao.createUseCaseRev(prevSave.rec.ident, (prevSave.rec.rev + 1).toShort, uc.header)

    def presave(ucId: UseCaseIdentId, savers: ValueSavers): SavedSteps = {
      var newSavedSteps: Map[LocalStepId, TextIdentId] = prevSave.savedSteps.ba
      val someSavedSteps = Some(prevSave.savedSteps)
      for (r <- savers)
        newSavedSteps ++= r.get.presave(dao, ucId, someSavedSteps)
      BiMap.swapped(newSavedSteps)
    }

    def save(savers: ValueSavers, ucId: UseCaseIdentId, ucRevId: UseCaseRevId)(implicit savedSteps: SavedSteps): SavedDataByField = {
      var savedData = List.empty[FieldAndSavedData]
      for (r <- savers) {
        val d = r.get.save(dao, ucId, ucRevId, getPrevSaveDataFor(r.f))
        savedData ::= FieldAndSavedData(r.f)(d)
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
