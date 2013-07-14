package com.beardedlogic.usecase
package lib

import field.{NormalAndAlternateCourseFields => NCAC, ExceptionCourseFields => EC, _}
import model._
import TypeTags._
import util.{MessageCentre, CachedFunction, BiMap}
import net.liftweb.common.Logger

// TODO Remove cometActor from UseCaseCtx
class UseCaseCtx(val cometActor: AnyRef = null) extends Logger {

  val msgCentre = new MessageCentre

  var lastSave: Option[UseCaseSaveCheckpoint] = None

  var number = 1: Short
  var title = "Untitled"

  // This needs to be before fields as they reference it immediately to hand to SmartText
  val stepLabelMap = CachedFunction.lazy0(
    BiMap(courseFields.foldLeft(Map.empty[LocalIdStr, LabelStr]) { _ ++ _.stepLabelMap.get })
  )

  // TODO hardcoded fieldlist
  val fieldList = Defaults.FieldList
  val fields = fieldList.get.fieldKeys.map(k => k.fieldDef.newFieldInstance(this, k))
  @inline final def genericFields = fields.asInstanceOf[List[Field[Any]]]
  def field(fk: FieldKey) = fields.collectFirst {case f if f.fieldKey == fk => f}

  val courseFields: List[CourseFields] = fields.collect { case f: CourseFields => f }
  def textFields = fields.collect { case f: TextField => f }
  def ncacField: Option[NCAC] = courseFields.collectFirst { case f: NCAC => f }
  def ecField: Option[EC] = courseFields.collectFirst { case f: EC => f }

  val normalCourseTitleId = ncacField.get.courses.head.stepTextId

  val savedSteps = CachedFunction.eager1WithInitial[FieldSaveCtx, BiMap[Long_StepDataId, LocalIdStr]](
    (saveCtx: FieldSaveCtx) => {
      BiMap(saveCtx.stepValues.map {
        case (localStepId, stepValue) => (stepValue.taggedDataId -> localStepId)
      })
    })(BiMap.empty)

  def init() { fields.foreach(_.init) }

  // -------------------------------------------------------------------------------------------------------------------

  def restoreCheckpoint(checkpoint: UseCaseSaveCheckpoint) {
    // TODO restoreCheckpoint only works before init. Undo not yet supported because no HTML is updated.

    // Set the number & title
    // (Must be done before setting fields)
    this.number = checkpoint.uc.number
    this.title = checkpoint.uc.title

    var finaliseStateFns: List[() => Unit] = List.empty
    for (f <- genericFields) {
      val state = checkpoint.fieldStates.get(f.fieldKey)
      if (state.isDefined) {
        finaliseStateFns :+= f.setState(state.get)
      }
      //    If S didn't exist
      //      F.state = None
      //        Clear courses, stepLabelMap, textFields
    }

    // Build ucCtx . map of stepDataId  →  Step Node ID
    savedSteps.refresh(checkpoint.saveCtx)

    for (fn <- finaliseStateFns) fn()

    lastSave = Some(checkpoint)
  }

  /**
   * Saves the use case.
   *
   * Does nothing if there are differences between the current UC, and the last-saved revision.
   *
   * @return Whether or not the use case was saved.
   */
  def save(dao: DAO): Boolean = {
    def actuallySave: Boolean = dao.withTransaction {
      var changesDetected = false
      val saveCtx1 = new MutableFieldSaveCtx
      var UCsFVs = Set.empty[Value[DataType.FieldValue]]
      var removeOldFVs = Set.empty[PlainValue[DataType.FieldValue]]

      // Check fields for changes and presave
      for (f <- genericFields) {
        val fk = f.fieldKey
        val oldFV: Option[PlainValue[DataType.FieldValue]] = lastSave.flatMap(_.saveCtx.fieldValues.get(fk))
        trace(s"$f - fk=$fk, oldFV=$oldFV")

        // Check if field has anything to save
        if (!f.save_?) {
          if (oldFV.isDefined) {
            changesDetected = true
            removeOldFVs += oldFV.get
            trace(s"$f - Nothing to save anymore. Used to be, thus removal required.")
          }
        } else {
          // Compare state and presave
          val previous: Option[(FieldSaveCtx, Any)] = for {
            ls <- lastSave
            fs <- ls.fieldStates.get(fk)
          } yield (ls.saveCtx, fs)
          val fieldChanged = f.presave(previous, saveCtx1, dao)
          if (fieldChanged) {
            // Field changed, presave a new field value
            val newValue = if (oldFV.isEmpty)
              dao.createInitialValue(DataType.FieldValue)
            else
              dao.createValue(oldFV.get, LatestRev)
            trace(s"$f - New value created: rev=${newValue.rev}, id=${newValue.valueId}")
            saveCtx1.fieldValues += (fk -> newValue)
            changesDetected = true
            UCsFVs += newValue
          } else {
            // Reuse the existing field value
            oldFV.foreach(UCsFVs += _)
            trace(s"$f - Reuse.")
          }
        }
      }

      // Check for changes to the use case itself
      changesDetected ||= (if (lastSave.isEmpty) true
      else {
        val old = lastSave.get.uc
        this.title != old.title || this.number != old.number
      })

      if (changesDetected) {
        val saveCtx2 = saveCtx1.immutable
        val combinedSaveCtx = if (lastSave.isEmpty) saveCtx2 else saveCtx2.combineWith(lastSave.get.saveCtx)
        savedSteps.refresh(combinedSaveCtx)

        // Create new usecase
        val ucValue = if (lastSave.isEmpty)
          dao.createInitialValue(DataType.UseCase)
        else
          dao.createValue(lastSave.get.uc.value, LatestRev)
        val uc = dao.createUseCase(ucValue, title, number, fieldList.get)

        // Save new field values
        var newFieldStates = lastSave.map(_.fieldStates).getOrElse(Map.empty[FieldKey, Any])
        for {
          f <- fields
          fv <- saveCtx2.fieldValues.get(f.fieldKey)
        } {
          val (fieldData, fieldState) = f.save(combinedSaveCtx, saveCtx2, dao)
          dao.createFieldValue(fv, f.fieldKey, fieldData)
          newFieldStates += (f.fieldKey -> fieldState)
        }

        // Link usecase to field values
        // TODO make bulk insert
        for (fv <- UCsFVs)
          dao.relate_usecase_has_fieldValue(uc, fv)

        // Save checkpoint
        val finalSaveCtx =
          if (removeOldFVs.isEmpty) combinedSaveCtx
          else combinedSaveCtx.copy(fieldValues = combinedSaveCtx.fieldValues.filterNot(e => removeOldFVs.contains(e._2)))
        lastSave = Some(UseCaseSaveCheckpoint(uc, finalSaveCtx, newFieldStates))
      }

      changesDetected
    }

    // Safely save
    lastSave.map(ls => Locks.UseCase.withWriteLock(ls.uc.dataId)(actuallySave))
    .getOrElse(actuallySave)
  }
}

case class UseCaseSaveCheckpoint(
  uc: UseCase,
  saveCtx: FieldSaveCtx,
  fieldStates: Map[FieldKey, Any]
  )

object UseCaseLoader {

  def loadCheckpoint(uc: UseCase, dao: DAO, lock: Locks.ReadLockToken): UseCaseSaveCheckpoint = {
    // Load use case
    val fieldList = Defaults.FieldList.get // TODO hardcoded fieldlist

    val saveCtx = new MutableFieldSaveCtx

    // Load fields
    val loadCtx = dao.getFieldLoadCtxFor(uc.valueId)
    val fieldStates = Map.newBuilder[FieldKey, Any]
    for (fk <- fieldList.fieldKeys) {

      // Load field states
      // (saveCtx.stepValues populated here by field-loaders)
      val fs = fk.fieldDef.stateLoader(fk).load(loadCtx, saveCtx)
      fieldStates += (fk -> fs)

      // Add field values
      for (fv <- loadCtx.fieldValues.get(fk.taggedId)) saveCtx.fieldValues += (fk -> fv)
    }

    UseCaseSaveCheckpoint(uc, saveCtx.immutable, fieldStates.result)
  }
}
