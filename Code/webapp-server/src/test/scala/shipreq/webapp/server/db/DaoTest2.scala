package shipreq.webapp.server.db

import java.util.concurrent.atomic.AtomicInteger
import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTestOps._
import shipreq.webapp.base.RandomData
import shipreq.webapp.server.test.{TestDatabaseHelpers, TestDB}
import utest._
import shipreq.base.util.{ThreadLocalRes, UnivEq}, UnivEq.Implicits._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.ProjectHash
import EventDao.EventSeq

object DaoTest2 extends TestSuite {

  val tldb = TestDB.threadLocalResH()
    .xmap(t => (t._1, t._2, t._2.newProjectId()))(t => (t._1, t._2))

//  import shipreq.webapp.base.text.Text.Equality._
//  import shipreq.webapp.base.util.TypeclassDerivation._
  implicit def activeEventEq = UnivEq.force[ActiveEvent]
//  implicit def activeEventEq: UnivEq[ActiveEvent] = UnivEq._deriveAuto
//  implicit def activeEventEq: scalaz.Equal[ActiveEvent] = deriveEqual

  override def tests = TestSuite {
    'event {

//      implicit val settings = DefaultSettings.propSettings.setSampleSize(10).setGenSize(10).setDebug.setSingleThreaded.setSeed(0)
      implicit val settings = DefaultSettings.propSettings.setSampleSize(10000).setGenSize(30) //.setSeed(0).setSingleThreaded

      val seqCounter = new AtomicInteger()

      val prop = Prop.equal[(ActiveEvent, ProjectHash)]("load . save = id")(
        i => {
          val seq = EventSeq(seqCounter.incrementAndGet())
          implicit val (s, db, projectId) = tldb.get()
          val dao = db.dao
          dao.createEvent(projectId, seq, i._1, i._2)
          db.debugSelectOnError(s"select * from event where seq = ${seq.value}") {
            dao.findEvent(projectId, seq)
          }
        },
        Some(_))

      val rnd = Gen.tuple2(RandomData.events.activeEvent, RandomData.events.projectHash)

      tldb around prop.mustBeSatisfiedBy(rnd)
    }
  }
}
