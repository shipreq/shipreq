package com.beardedlogic.usecase.feature.uc.persist

import org.scalatest.FunSuite
import com.beardedlogic.usecase.app.DI
import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.lib.Locks
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.stress.GenerateUCs
import com.beardedlogic.usecase.test.{TestHelpers, TestDB}
import TestDB.withDbHelpers

class ParSave extends FunSuite with TestHelpers with DI {

  // Number of UC revisions to save
  val count = 8

  var setupData: SetupData = null

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupData = new SetupData
  }

  override def afterAll(): Unit = {
    if (setupData != null) {
      setupData.teardown()
      setupData = null
    }
    super.afterAll()
  }

  class SetupData extends {
    var u, v: UserId = null
    var p, q, r: ProjectId = null

    withDbHelpers(true)(h => {
      u = h.newUserId()
      p = h.newProjectId(u)
      q = h.newProjectId(u)

      v = h.newUserId()
      r = h.newProjectId(v)
    })

    def teardown(): Unit = withDbHelpers(false)(h => {
      h.deleteUser(u)
      h.deleteUser(v)
    })
  }

  test("Concurrent saving and loading") {
    val ucs1 = GenerateUCs.generate(count, (1: Short).tag).toList
    val ucs2 = GenerateUCs.generate(count, (2: Short).tag).toList
    val ucs3 = GenerateUCs.generate(count, (3: Short).tag).toList

    val t1 = TestThread("[1]", setupData.u, setupData.p, ucs1)
    val t2 = TestThread("[2]", setupData.u, setupData.p, ucs2)
    val t3 = TestThread("[3]", setupData.u, setupData.p, ucs3)
    val tq = TestThread("[Q]", setupData.u, setupData.q, rnd.shuffle(ucs1))
    val tv = TestThread("[V]", setupData.v, setupData.r, rnd.shuffle(ucs1))
    val results = List(t1, t2, t3, tq, tv).par.map(_.run).toList

    val errors = results.filter(_.isDefined).map(_.get)
    errors.headOption.foreach(error("Error", _))
    errors shouldBe empty
  }

  case class TestThread(prefix: String, u: UserId, p: ProjectId, ucs: List[UseCase]) {
    def run(): Option[Throwable] = try {
      debug(s"$prefix Starting...")

      // Create initial UC
      val rev1 = daoProvider.withTransaction(d => {
        val ucn = ucs.head.number
        val i = d.createUseCaseIdentWithForcedNumber(p, ucn)
        d.createUseCaseRev(i, 1, ucs.head.header)
      })
      var cp = loadLatest(rev1)

      // Test updates
      for ((uc,i) <- ucs.zipWithIndex) {
        debug(s"$prefix Pass #${i + 1}")
        //debug(s"$prefix Pass #${i + 1}\n" + uc.devView)
        cp = save(uc, cp)
        debug(cp.savedSteps)
        assertUseCasesLookSameToUser(loadLatest(rev1).uc, uc)
      }

      None
    } catch {
      case t: Throwable => Some(t)
    }

    def loadLatest(ucId: UseCaseIdentId): UseCaseSaveCheckpoint =
      (for {
        lock <- Locks.UseCaseNumbers.readM(p)
        dao <- daoProvider.forTransaction
        ucRec <- dao.findUseCaseLatestRev(ucId)
      } yield UseCasePersistence.load(ucRec).run(dao, lock)
        ).getOrElse(throw new RuntimeException("Load failed!"))

    def save(uc: UseCase, cp: UseCaseSaveCheckpoint): UseCaseSaveCheckpoint =
      daoProvider.withTransaction(dao => {
        val lock = Locks.SingleUseCase.writeP(cp, cp)
        UseCasePersistence.save(uc, cp, lock, dao) getOrElse cp
      })
  }
}

