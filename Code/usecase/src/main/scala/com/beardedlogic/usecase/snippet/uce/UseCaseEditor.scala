package com.beardedlogic.usecase
package snippet.uce

import net.liftweb.common._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http._
import net.liftweb.util.Helpers._
import JsCmds.Noop

import db.UseCaseIdent
import lib._
import change._
import field._
import Types._

object UseCaseEditor extends StaticSnippetHelpers with DI {

  case class State(uc: UseCase, prevSave: Option[UseCaseSaveCheckpoint], saveEnabled: Boolean) {
    def currentRevision: Short = prevSave.map(_.rec.rev).getOrElse(0: Short)
  }
  object State {
    def apply(cp: UseCaseSaveCheckpoint): State = State(cp.uc, Some(cp), false)
  }

  // TODO Delete UCE . initial state
  val DefaultInitialState: State = {
    val ucn = (0:Short).tag[UseCaseNumberTag]
    val h = Defaults.useCaseHeader
    val fl = Defaults.fieldList.value.fields
    val ncf = UseCaseFns.filter[NormalCourseField](fl).head
    val fv = fl.map(f => (f ~> f.empty)).toMap + (ncf ~> ncf.defaultLoadValue(h)._2.apply)
    val sl = UseCaseFns.generateStepAndLabelBiMap(ucn, fv)
    val uc = UseCase(ucn, h, fl, fv, sl)
    State(uc, None, false)
  }

  def loadLatest(ucId: UseCaseIdentId): State = requireResult_!(for {
      lock   <- Locks.useCase.readM(ucId)
      dao    <- daoProvider.forTransaction
      ucRec  <- Box(dao.findUseCaseLatestRev(ucId)) ~> NotFoundResponse()
    } yield State(UseCasePersistence.load(ucRec, dao, lock)))

  def allowSave(before: State, after: UseCase): Boolean = before.prevSave match {
    case None => false
    case Some(cp) => !UseCaseEquality.uc.equal(cp.uc, after)
  }
}

import UseCaseEditor._

class UseCaseEditor(initialState: UseCaseEditor.State) extends StatefulSnippet with SnippetHelpers {

  def this() = this(DefaultInitialState)
  def this(ucId: UseCaseIdentId) = this(loadLatest(ucId))

  private var state__ = initialState
  @inline final def state = state__
  @inline final def uc = state.uc
  @inline final def uch = uc.header
  @inline final def fields = uc.fields
  @inline final def fieldValues = uc.fieldValues

  val textFieldIds: Map[Field, LocalTextFieldId] =
    UseCaseFns.filter[TextField](fields)
    .map(f => (f -> nextFuncName.tag[LocalTextFieldIdTag]))
    .toMap

  private var renderer__ = Renderer(state, textFieldIds, update, state.prevSave.map(_ => save _))
  @inline final def renderer = renderer__

  protected def setState(newState: State): Unit = {
    state__ = newState
    renderer__ = renderer__.copy(state = newState)
  }

  override def dispatch = { case _ => renderer.render }

  def update(f: UseCase => UcUpdateResult): JsCmd =
    f(uc) match {
      case Changed(newUc, changes) =>
        setState(State(newUc, state.prevSave, allowSave(state, newUc)))
        renderer.jsRespondToChanges(changes)

      case NoChange => Noop

      case ChangeFailure(err) => renderer.jsRespondChangeFailure(err)
    }

  def save(): JsCmd = state.prevSave match {
    case Some(cp) =>
      daoProvider.withTransaction(dao => {
        UseCasePersistence.save(uc, cp, dao) match {
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
