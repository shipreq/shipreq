package shipreq.taskman.server

import doobie.imports._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import scala.util.Random
import shipreq.base.db.SqlHelpers._
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.base.util.JavaTimeHelpers._
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.taskman.api.impl.Serialisation
import shipreq.taskman.api._
import Msg.ReRegistrationAttempted
import SopImpl.Sql._

class SopImplTest extends Specification with DatabaseTest {

  def dao = SopImpl.Dao
  val n = NodeId(123)
  val w = WorkerId(666)
  val rng = new Random()
  val defaultMsg = ReRegistrationAttempted(EmailAddr("haha cool"))

  "reassignWorker" should {

    val insertQ = Query[(Option[NodeId], Option[WorkerId], Duration, Duration, Duration), MsgId](
      "INSERT INTO msgq(type, data, node, worker, updated_at, created_at, effective_from, priority, priority_base)" +
        s"VALUES(0, NULL, ?, ?, now()-?, now()-?, now()-?, 5,5) RETURNING id")

    def getUpdatedAt(id: MsgId): Instant =
      sql"select updated_at from msgq where id=$id".query[Instant].unique.runNow()

    def timestampBeforeAfter(id: MsgId) = {
      val b = getUpdatedAt(id)
      dao.reassignWorker(n, w, id).runNow()
      val a = getUpdatedAt(id)
      (a, b)
    }

    "when still assigned to self" in {
      def insert = insertQ.toQuery0((Some(n), Some(w), 10.minutes, 3.days, 3.days)).unique.runNow()

      "return true" in {
        dao.reassignWorker(n, w, insert).runNow() must beTrue
      }
      "update timestamp" in {
        val (a, b) = timestampBeforeAfter(insert)
        a must_!= b
      }
    }

    "when still assigned to self" >> {
      def insert = insertQ.toQuery0((Some(n), None, 10.minutes, 3.days, 3.days)).unique.runNow()

      "return false" in {
        dao.reassignWorker(n, w, insert).runNow() must beFalse
      }
      "not update timestamp" in {
        val (a, b) = timestampBeforeAfter(insert)
        a must_== b
      }
    }
  }

  "getMsgsAssignNode" should {

    val insertQ = Query[(Short, Priority, Priority, Option[Int], Option[Short], Duration, Duration, Duration), MsgId](
      "INSERT INTO msgq(type, priority, priority_base, node, worker, created_at, updated_at, effective_from)" +
        "VALUES(?, ?, ?, ?, ?, now()+?, now()+?, now()+?) RETURNING id")

    def insert(node: Boolean = false,
               worker: Boolean = false,
               pri: Priority = Priority(50),
               created: Duration = -1 second,
               updated: Duration = null,
               effective: Duration = null
                ): MsgId =
      insertQ.toQuery0((
        rng.nextInt().toShort,
        pri,
        Priority(rng.nextInt().toShort),
        if (node) Some(rng.nextInt()) else None,
        if (worker) Some(rng.nextInt().toShort) else None,
        created,
        Option(updated) getOrElse created,
        Option(effective) getOrElse created
      )).unique.runNow()

    def insertP(pris: Int*): Vector[MsgId] =
      rng.shuffle(
        pris.toVector
          .map(p => Priority(p.toShort))
          .zipWithIndex)
        .map(_.map1(p => insert(pri = p)))
        .sortBy(_._2)
        .map(_._1)

    def idAndPri(m: MsgHeader) = (m.id, m.priority)

    def returnOnly(ids: MsgId*) =
      containTheSameElementsAs(ids) ^^ {(_:List[MsgHeader]).map(_.id)}

    def returnOnlyP(idsAndPris: Seq[(MsgId, Priority)]) =
      containTheSameElementsAs(idsAndPris) ^^ {(_:List[MsgHeader]).map(idAndPri)}

    def testWithoutHighPriorityQueueOverrides(queue: Option[(Priority, Int)], allowResults: Boolean = true) = {

      def ifAllowedReturnOnly(ids: MsgId*): Matcher[List[MsgHeader]] =
        if (allowResults) returnOnly(ids: _*) else be_==(Nil)

      "return nothing when nothing is available" in {
        dao.getMsgsAssignNode(n, 2, 2 days, queue).runNow() must be empty
      }

      "return unassigned msgs" in {
        val id = insert()
        dao.getMsgsAssignNode(n, 2, 2 days, queue).runNow() must ifAllowedReturnOnly(id)
      }

      "not return recently assigned msgs" in {
        insert(created = -16 hours, updated = -15 hours, node = true)
        insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
        dao.getMsgsAssignNode(n, 2, 24 hours, queue).runNow() must be empty
      }

      "ignore old assignment" in {
        val a = insert(created = -16 hours, updated = -15 hours, node = true)
        val b = insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
        dao.getMsgsAssignNode(n, 2, 8 hours, queue).runNow() must ifAllowedReturnOnly(a, b)
      }

      "limit results to top n highest priority" in {
        val ids = insertP(0, 1, 2, 3, 4, 5, 6)
        dao.getMsgsAssignNode(n, 3, 8 hours, queue).runNow() must ifAllowedReturnOnly(ids takeRight 3: _*)
      }

      "filter by effective_from" in {
        insert(effective = 1 minute)
        dao.getMsgsAssignNode(n, 2, 2.days, queue).runNow() must be empty
      }
    }

    def test(limit: Int, queuedAlready: Int)(pris: Int*)(expectedIndexes: Int*) = {
      val under = List(4,5,6,7,8,9).sorted.reverse
      val allIds = insertP(under ++ pris: _*)
      val (idsU, idsP) = allIds.splitAt(under.size)
      val exps = expectedIndexes.map(i => {
        val (id, p) = if (i<0) {
                        val j = -(i+1)
                        (idsU(j), under(j))
                      } else
                        (idsP(i), pris(i))
        (id, Priority(p.toShort))
      })
      dao.getMsgsAssignNode(n, limit, 8 hours, Some((Priority(9), queuedAlready))).runNow() must returnOnlyP(exps)
    }

    "when in-memory queue is empty" >> {
      testWithoutHighPriorityQueueOverrides(None)
    }

    "when in-memory queue is full" >> {

      "ignore queue when all DB > mem" >> {
        testWithoutHighPriorityQueueOverrides(Some((Priority(0), 99)))
      }

      "ignore DB when mem > all DB" >> {
        testWithoutHighPriorityQueueOverrides(Some((Priority(200), 99)), false)
      }

      "More HP than limit" in { test(3, 99)(14, 13, 12, 11, 10)(0, 1, 2) }
      "HP equal to limit"  in { test(3, 99)(12, 11, 10        )(0, 1, 2) }
      "HP less than limit" in { test(3, 99)(11, 10            )(0, 1   ) }
    }

    "when in-memory queue is partially full" >> {
      "HP count = 0"                   in { test(5, 3)(          )(-1, -2  ) }
      "HP count = free slots"          in { test(5, 3)(11, 10    )(0, 1    ) }
      "HP count < free slots"          in { test(6, 3)(11, 10    )(0, 1, -1) }
      "HP count > free slots, < limit" in { test(5, 4)(11, 10    )(0, 1    ) }
      "HP count > free slots, = limit" in { test(3, 1)(12, 11, 10)(0, 1, 2 ) }
      "HP count > free slots, > limit" in { test(2, 1)(12, 11, 10)(0, 1    ) }
    }
  }

  "getMsgAssignWorker" should {

    val insertQ = Query[(Short, JsonStr[Msg], Option[NodeId], Option[WorkerId], Duration, Duration, Duration), MsgId](
      "INSERT INTO msgq(type, data, node, worker, created_at, updated_at, effective_from, priority, priority_base)" +
        "VALUES(?, ?, ?, ?, now()-?, now()-?, now()-?, 5,5) RETURNING id")

    def insert(node: Option[NodeId] = None, worker: Option[WorkerId] = None, msg: Msg = defaultMsg): MsgId = {
      val d: Duration = 2.days
      insertQ.toQuery0((MsgType.lookup(msg).id, Serialisation.serialise(msg), node, worker, d, d, d)).unique.runNow()
    }

    def insertAssignedToOwnNode() = insert(node = Some(n))

    def test(id: MsgId) =
      () => dao.getMsgAssignWorker(n, w, MsgHeader(id, Priority(5), Instant.now())).runNow()

    "not assign if msg has been picked up by another node" in {
      test(insert(node = Some(NodeId(6789))))() must beNone
    }

    "not assign if msg has been picked up by another worker" in {
      test(insert(node = Some(n), worker = Some(WorkerId(6789))))() must beNone
    }

    "deserialise the msg" in {
      test(insertAssignedToOwnNode())() must beLike{ case Some(MsgDetail(_, msg, _)) if msg == defaultMsg => ok }
    }

    "assign when unassigned" in {
      val t = test(insertAssignedToOwnNode())
      (t().isDefined, t().isDefined) ==== (true, false)
    }
  }
}
