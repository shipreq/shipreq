package com.beardedlogic.usecase
package snippet.uce

import net.liftweb.common._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http._
import net.liftweb.util.Helpers._
import JsCmds.Noop

import lib._
import change._
import field._
import model._
import Types._

object UseCaseEditor extends StaticSnippetHelpers with DI {

  case class State(uc: UseCase, prevSave: Option[UseCaseSaveCheckpoint]) {
    def currentRevision = prevSave.map(_.rec.rev.toString).getOrElse("0")
  }
  object State {
    def apply(cp: UseCaseSaveCheckpoint): State = State(cp.uc, Some(cp))
  }

  // TODO Delete UCE . initial state
  val DefaultInitialState: State = {
    val h = UseCaseHeader(1, Defaults.Title)
    val fl = Defaults.FieldList.get.fields
    val ncf = UseCaseFns.filter[NormalCourseField](fl).head
    val fv = fl.map(f => (f ~> f.empty)).toMap + (ncf ~> ncf.defaultLoadValue(h)._2.apply)
    val sl = UseCaseFns.generateStepAndLabelBiMap(fv, h)
    val uc = UseCase(h, fl, fv, sl)
    State(uc, None)
  }

  def loadLatest(ucId: UseCaseIdentId): State = {
    val tryToLoad = for {
      lock   <- Locks.UseCase.forRead(ucId)
      dao    <- daoProvider.forTransaction
      ucRec  <- Box(dao.findLatestUseCase(ucId)) ~> NotFoundResponse()
    } yield UseCasePersistence.load(ucRec, dao, lock)
    tryToLoad match {
      case Full(cp)                               => State(cp)
      case ParamFailure(_, _, _, r: LiftResponse) => respondImmediately(r)
      case _                                      => shouldNeverHappen_!
    }
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

  private var renderer__ = Renderer(state, textFieldIds, update, save)
  @inline final def renderer = renderer__

  protected def setState(newState: State): Unit = {
    state__ = newState
    renderer__ = renderer__.copy(state = newState)
  }

  override def dispatch = { case _ => renderer.render }

  def update(f: UseCase => UcUpdateResult): JsCmd =
    f(uc) match {
      case Changed(newUc, changes) =>
        setState(State(newUc, state.prevSave))
        renderer.jsRespondToChanges(changes)

      case NoChange => Noop

      case ChangeFailure(err) => renderer.jsRespondChangeFailure(err)
    }

  def save(): JsCmd =
    daoProvider.withTransaction(dao => {
      UseCasePersistence.save(uc, state.prevSave, dao) match {
        case thisSave@Some(cp) =>
          setState(State(cp.uc, thisSave))
          renderer.jsUpdateRevision
        case None => Noop
      }
    })
}
