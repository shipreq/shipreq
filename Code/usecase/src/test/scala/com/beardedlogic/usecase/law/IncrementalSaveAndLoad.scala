package com.beardedlogic.usecase
package law

import org.scalacheck._
import test.DataGenerators._
import test.{TestDatabaseHelpers, TestDB}
import db.UseCaseHeader
import lib.{UseCase, UseCaseSaveCheckpoint}
import lib.change.{Changed, ChangeFailure, NoChange}
import lib.Types.ProjectId

/**
 * Mutate UC, save, load and compare, repeat.
 */
object IncrementalSaveAndLoad extends Commands {

  case class SystemInvariants(pid: ProjectId, db: TestDatabaseHelpers)

  class System {
    type HistoryEntry = (UseCase, String)
    var invars: SystemInvariants = null
    var cp: UseCaseSaveCheckpoint = null
    var history: List[HistoryEntry] = Nil

    def db = invars.db
    def dao = db.dao
    def pid = invars.pid

    def uninit(): Unit = if (invars != null) {
      invars.db.session.rollback()
      invars.db.session.close()
      invars = null
      cp = null
      history = Nil
    }

    def reset(): Unit = {
      uninit()
      invars = {
        val s = TestDB.Slick.createSession()
        s.conn.setAutoCommit(false)
        val db = TestDatabaseHelpers(s)
        val pid = db.newProjectId()
        SystemInvariants(pid, db)
      }
      val rev1 = db.createUseCaseIdentAndRev1(pid, UseCaseHeader("Do Stuff"))
      cp = db.loadUseCase(rev1, pid)
      history = (cp.uc, "Starting point.") :: Nil
    }

    def reload: UseCaseSaveCheckpoint = db.loadUseCase(cp.rec, pid)

    def save(uc: UseCase, mutationDesc: String): Option[UseCaseSaveCheckpoint] = {
      history = (uc, mutationDesc) :: history
      val result = db.saveUseCase(uc, Some(cp), pid)
      cp = result.getOrElse(cp)
      result
    }

    def rev = cp.rec.rev

    def historyRecreation: String = {
      var c = 0
      val t = system.history.size
      //(system.history :\ "")(((uc: UseCase, mutationDesc: String), a: String) => {
      (system.history :\ "")((he: HistoryEntry, a: String) => {
        val (uc, mutationDesc) = he
        c += 1
        a + s"// ###### UC $c/$t: $mutationDesc\nval uc$c = ${uc.inspect}\n"
      })
    }
  }

  val system = new System

  // -------------------------------------------------------------------------------------------------------------------

  case class State(uc: UseCase, mutations: Int, prevTransition: Option[(State, UseCaseMutator, String)]) {
    override def toString = super.toString
  }

  override def initialState() = {
    system.reset()
    State(system.cp.uc, 0, None)
  }

  case class MutationCommand(mutator: UseCaseMutator) extends Command {

    override def toString = "M"

    override def nextState(s: State): State = {
      val (r, desc) = mutator(s.uc)
      r match {
        case NoChange | ChangeFailure(_) => s.copy(prevTransition = Some(s, mutator, desc))
        case Changed(newUc, _)           => State(newUc, s.mutations + 1, Some(s, mutator, desc))
      }
    }

    preConditions += (s => s.prevTransition match {
      case None => true
      case Some((p, _, _)) if s.mutations != p.mutations => true
      case Some((p, _, d)) if s.mutations == p.mutations => trace(s"Rejecting mutation: $d"); false
    })

    // -----------------------------------------------------------------------------------------------------------------

    override def run(s: State): Option[String] = {
      val mutationDesc = s.prevTransition.map(_._3).getOrElse(if (system.rev == 1) "Initial state" else "?")
      debug(s"Performing mutation #${system.history.size} on UC rev #${system.rev}: $mutationDesc")

      val newUc = s.uc
      system.save(newUc, mutationDesc)
      val reloaded = system.reload.uc

      val newUcV = newUc.userView
      val reloadedV = reloaded.userView
      if (newUcV == reloadedV) None
      else {
        val history = system.historyRecreation
        val err = s"Failed after ${system.history.size} mutations. Loaded UC differs from saved." +
          s"\nEXPECTED:${newUcV}\nLOADED:${reloadedV}\nHISTORY:\n${history}\n"
        Some(err)
      }
    }

    postConditions += {
      case (_, _, err: Option[_]) => err match {
        case None => true
        case Some(msg) => Prop(false) :| msg.toString
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  override def genCommand(s: State): Gen[Command] = useCaseMutator.flatMap(MutationCommand(_))
}
