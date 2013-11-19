package com.beardedlogic.usecase
package snippet.uce

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.{JsExp, JsCmd, JsCmds}
import net.liftweb.util.Helpers.nextFuncName
import JsCmds.Noop

import app.{Defaults, DI, RequestVars}
import db.UseCaseHeader
import lib.{Misc, SnippetHelpers, Locks, StaticSnippetHelpers}
import lib.ScalazSubset._
import lib.Types._
import feature.uc._
import feature.uc.change._
import feature.uc.field._
import feature.uc.persist.{UseCasePersistence, UseCaseSaveCheckpoint}
import feature.validation.VFailure
import util.JsExt.JqFocus

object UseCaseEditor {

  case class State(uc: UseCase, prevSave: Option[UseCaseSaveCheckpoint], saveEnabled: Boolean) {
    def currentRevision: Short = prevSave.map(_.rec.rev).getOrElse(0: Short)
  }
  object State {
    def apply(cp: UseCaseSaveCheckpoint): State = State(cp.uc, Some(cp), false)
  }

  case class UcModifier(
    updateFn: UseCaseUpdater => UcUpdateResult,
    nopFn: Option[Renderer => JsCmd],
    focusOnErr: Option[JsExp],
    errFn: Option[VFailure => JsCmd] = None)
}

import UseCaseEditor._

object UseCaseEditorFns extends StaticSnippetHelpers with DI {

  // TODO Delete UCE . initial state
  val DefaultInitialState: State = {
    val ucn = (0:Short).tag[IsUseCaseNumber]
    val h = UseCaseHeader("DEMO".tag[Validated])
    val fl = Defaults.fieldList.value.fields
    val ncf = Misc.filterCovar[NormalCourseField](fl).head
    val fv = fl.map(f => (f ~> f.empty)).toMap + (ncf ~> ncf.defaultLoadValue(h)._2.apply)
    val sl = UseCaseFns.generateStepAndLabelBiMap(ucn, fv)
    val uc = UseCase(ucn, h, fl, fv, sl)
    State(uc, None, false)
  }

  def loadLatest(ucId: UseCaseIdentId): (State, UseCaseRelations) = requireResult_!(for {
      lock   <- Locks.UseCaseNumbers.readM(RequestVars.ProjectId.get.value)
      dao    <- daoProvider.forTransaction
      ucRec  <- Box(dao.findUseCaseLatestRev(ucId)) ~> NotFoundResponse()
    } yield {
      val rels = CachedUseCaseRelations(RequestVars.UseCases.get)
      val cp = UseCasePersistence.load(ucRec).useRels(rels).run(dao, lock)
      (State(cp), rels)
    })

  def allowSave(before: State, after: UseCase): Boolean = before.prevSave match {
    case None => false
    case Some(cp) => !UseCaseEquality.uc.equal(cp.uc, after)
  }
}

import UseCaseEditorFns._

class UseCaseEditor(initialState: UseCaseEditor.State, val rels: UseCaseRelations) extends StatefulSnippet with SnippetHelpers {

  // Constructor for demo page
  def this() = this(DefaultInitialState, UseCaseRelations.Empty)

  // Constructor for real page
  def this(p: (State, UseCaseRelations)) = this(p._1, p._2)
  def this(ucId: UseCaseIdentId) = this(loadLatest(ucId))

  private var state__ = initialState
  @inline final def state = state__
  @inline final def uc = state.uc
  @inline final def uch = uc.header
  @inline final def fields = uc.fields
  @inline final def fieldValues = uc.fieldValues

  val textFieldIds: Map[Field, LocalTextFieldId] =
    Misc.filterCovar[TextField](fields)
    .map(f => (f -> nextFuncName.tag[IsLocalTextFieldId]))
    .toMap

  private var renderer__ = Renderer(state, textFieldIds, update, state.prevSave.map(_ => save _))
  private var ucUpdater__ = UseCaseUpdater(uc, rels)
  @inline final def renderer = renderer__
  @inline final def ucUpdater = ucUpdater__

  protected def setState(newState: State): Unit = {
    state__ = newState
    renderer__ = renderer__.copy(state = newState)
    ucUpdater__ = UseCaseUpdater(uc, rels)
  }

  override def dispatch = { case _ => renderer.render }

  def update(m: UcModifier): JsCmd =
    m.updateFn(ucUpdater) match {
      case Changed(newUc, changes) =>
        setState(State(newUc, state.prevSave, allowSave(state, newUc)))
        renderer.jsRespondToChanges(changes)

      case NoChange =>
        m.nopFn.map(_(renderer)) getOrElse Noop

      case ChangeFailure(vf) =>
        val a = m.errFn.map(_(vf)) getOrElse renderer.jsRespondChangeFailure(vf)
        val b = m.focusOnErr.map[JsCmd](_ ~> JqFocus) getOrElse Noop
        a |+| b
    }

  def save(): JsCmd = state.prevSave match {
    case Some(cp) =>
      daoProvider.withTransaction(dao => {
        val lock = Locks.SingleUseCase.writeP(cp, cp)
        UseCasePersistence.save(uc, cp, lock, dao) match {
          case Some(cp) =>
            setState(State(cp))
            jsPostSave
          case None =>
            jsPostSaveNop
        }
      })
    case None => shouldNeverHappen_!("Save failed. Ident missing.")
  }

  def jsPostSave: JsCmd = jsPostSaveNop & renderer.jsUpdateRevision
  def jsPostSaveNop: JsCmd = renderer.jsEnableSaveButton(false)

}
