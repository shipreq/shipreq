package shipreq.taskman.server

import cats.instances.all._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scala.util.Random
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db.TestDb
import shipreq.taskman.api._
import shipreq.taskman.api.impl.TaskmanDoobieCodecs._
import shipreq.taskman.server.logic.{NodeId, TaskDetail, TaskHeader, WorkerId}
import utest._

object ServerOpFxTest extends TestSuite {
  import Task.ReRegistrationAttempted
  import ServerOpFx.Sql._

  private def dao = ServerOpFx.Dao
  private val n = NodeId(123)
  private val w = WorkerId(666)
  private val rng = new Random()
  private val defaultTask = ReRegistrationAttempted(EmailAddr("haha cool"))

  private object GetMsgsAssignNode {

    val insertQ = Query[(Short, Priority, Priority, Option[Int], Option[Short], Duration, Duration, Duration), TaskId](
      "INSERT INTO msgq(type, data, priority, priority_base, node, worker, created_at, updated_at, effective_from)" +
        "VALUES(?, '{}', ?, ?, ?, ?, now()+?, now()+?, now()+?) RETURNING id")

    def insert(node: Boolean = false,
               worker: Boolean = false,
               pri: Priority = Priority(50),
               created: Duration = -1 seconds,
               updated: Duration = null,
               effective: Duration = null) =
      insertQ.toQuery0((
        rng.nextInt().toShort,
        pri,
        Priority(rng.nextInt().toShort),
        if (node) Some(rng.nextInt()) else None,
        if (worker) Some(rng.nextInt().toShort) else None,
        created,
        Option(updated) getOrElse created,
        Option(effective) getOrElse created
      )).unique

    def insertP(pris: Int*): ConnectionIO[Vector[TaskId]] =
      rng.shuffle(pris.toVector.map(p => Priority(p.toShort)).zipWithIndex)
        .traverse(pi => insert(pri = pi._1).map((_, pi._2)))
        .map(_.sortBy(_._2).map(_._1))

    def idAndPri(m: TaskHeader) = (m.id, m.priority)

    final class TestWithoutHighPriorityQueueOverrides(queue: Option[(Priority, Int)], allowResults: Boolean = true) {

      private def assertReturnOnly(a: List[TaskHeader], e: TaskId*) = {
        val ids = a.map(_.id)
        assert(ids.sortBy(_.value) == e.toList.sortBy(_.value))
      }

      private def assertAllowedReturnOnly(a: List[TaskHeader], ids: TaskId*): Unit =
        if (allowResults)
          assertReturnOnly(a, ids: _*)
        else
          assert(a.isEmpty)

      def returnNothingWhenNothingIsAvailable() = {
        val r = TestDb ! dao.getMsgsAssignNode(n, 2, 2 days, queue)
        assert(r.isEmpty)
      }

      def returnUnassignedMsgs() = {
        val (id, r) = TestDb ! (insert() product dao.getMsgsAssignNode(n, 2, 2 days, queue))
        assertAllowedReturnOnly(r, id)
      }

      def not() = {
        val r = TestDb.!(for {
          _ <- insert(created = -16 hours, updated = -15 hours, node = true)
          _ <- insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
          r <- dao.getMsgsAssignNode(n, 2, 24 hours, queue)
        } yield r)
        assert(r.isEmpty)
      }

      def ignore() = {
        val (r, a, b) = TestDb.!(for {
          a <- insert(created = -16 hours, updated = -15 hours, node = true)
          b <- insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
          r <- dao.getMsgsAssignNode(n, 2, 8 hours, queue)
        } yield (r, a, b))
        assertAllowedReturnOnly(r, a, b)
      }

      def limit() = {
        val (r, ids) = TestDb.!(for {
          ids <- insertP(0, 1, 2, 3, 4, 5, 6)
          r <- dao.getMsgsAssignNode(n, 3, 8 hours, queue)
        } yield (r, ids))
        assertAllowedReturnOnly(r, ids takeRight 3: _*)
      }

      def filter() = {
        val r = TestDb ! insert(effective = 1 minutes) *> dao.getMsgsAssignNode(n, 2, 2.days, queue)
        assert(r.isEmpty)
      }
    }
  }

  override def tests = Tests {

    "reassignWorker" - {

      val insertQ = Query[(Option[NodeId], Option[WorkerId], Duration, Duration, Duration), TaskId](
        "INSERT INTO msgq(type, data, node, worker, updated_at, created_at, effective_from, priority, priority_base)" +
          s"VALUES(0, '{}', ?, ?, now()-?, now()-?, now()-?, 5,5) RETURNING id")

      def getUpdatedAt(id: TaskId) =
        sql"select updated_at from msgq where id=$id".query[Instant].unique

      def timestampBeforeAfter(id: TaskId) =
        for {
          b <- getUpdatedAt(id)
          _ <- dao.reassignWorker(n, w, id)
          a <- getUpdatedAt(id)
        } yield (a, b)

      "when still assigned to self" - {
        def insert = insertQ.toQuery0((Some(n), Some(w), 10.minutes, 3.days, 3.days)).unique

        "return true" - {
          val result = TestDb ! insert.flatMap(dao.reassignWorker(n, w, _))
          assert(result)
        }

        "update timestamp" - {
          val (a, b) = TestDb ! insert.flatMap(timestampBeforeAfter)
          assertNotEq(a, b)
        }
      }

      "when still assigned to self" - {
        def insert = insertQ.toQuery0((Some(n), None, 10.minutes, 3.days, 3.days)).unique

        "return false" - {
          val result = TestDb ! insert.flatMap(dao.reassignWorker(n, w, _))
          assert(!result)
        }
        "not update timestamp" - {
          val (a, b) = TestDb ! insert.flatMap(timestampBeforeAfter)
          assertEq(a, b)
        }
      }
    }

    "getMsgsAssignNode" - {
      import GetMsgsAssignNode._

      def test(limit: Int, queuedAlready: Int)(pris: Int*)(expectedIndexes: Int*) = {
        val under = List(4,5,6,7,8,9).sorted.reverse
        val (actual, expect) = TestDb.!(
          for {
            allIds <- insertP(under ++ pris: _*)
            (idsU, idsP) = allIds.splitAt(under.size)
            e = expectedIndexes.map(i => {
              val (id, p) = if (i<0) {
                val j = -(i+1)
                (idsU(j), under(j))
              } else
                (idsP(i), pris(i))
              (id, Priority(p.toShort))
            })
            a <- dao.getMsgsAssignNode(n, limit, 8 hours, Some((Priority(9), queuedAlready)))
          } yield (a.map(idAndPri), e))

        assertSeq(actual.sortBy(_.toString), expect.sortBy(_.toString))
      }

      "when in-memory queue is empty" - {
        val t = new TestWithoutHighPriorityQueueOverrides(None)
        "return nothing when nothing is available" - t.returnNothingWhenNothingIsAvailable()
        "return unassigned msgs"                   - t.returnUnassignedMsgs()
        "not return recently assigned msgs"        - t.not()
        "ignore old assignment"                    - t.ignore()
        "limit results to top n highest priority"  - t.limit()
        "filter by effective_from"                 - t.filter()
      }

      "when in-memory queue is full" - {

        "ignore queue when all DB > mem" - {
          val t = new TestWithoutHighPriorityQueueOverrides(Some((Priority(0), 99)))
          "return nothing when nothing is available" - t.returnNothingWhenNothingIsAvailable()
          "return unassigned msgs"                   - t.returnUnassignedMsgs()
          "not return recently assigned msgs"        - t.not()
          "ignore old assignment"                    - t.ignore()
          "limit results to top n highest priority"  - t.limit()
          "filter by effective_from"                 - t.filter()
        }

        "ignore DB when mem > all DB" - {
          val t = new TestWithoutHighPriorityQueueOverrides(Some((Priority(200), 99)), false)
          "return nothing when nothing is available" - t.returnNothingWhenNothingIsAvailable()
          "return unassigned msgs"                   - t.returnUnassignedMsgs()
          "not return recently assigned msgs"        - t.not()
          "ignore old assignment"                    - t.ignore()
          "limit results to top n highest priority"  - t.limit()
          "filter by effective_from"                 - t.filter()
        }

        "More HP than limit" - { test(3, 99)(14, 13, 12, 11, 10)(0, 1, 2) }
        "HP equal to limit"  - { test(3, 99)(12, 11, 10        )(0, 1, 2) }
        "HP less than limit" - { test(3, 99)(11, 10            )(0, 1   ) }
      }

      "when in-memory queue is partially full" - {
        "HP count = 0"                   - { test(5, 3)(          )(-1, -2  ) }
        "HP count = free slots"          - { test(5, 3)(11, 10    )(0, 1    ) }
        "HP count < free slots"          - { test(6, 3)(11, 10    )(0, 1, -1) }
        "HP count > free slots, < limit" - { test(5, 4)(11, 10    )(0, 1    ) }
        "HP count > free slots, = limit" - { test(3, 1)(12, 11, 10)(0, 1, 2 ) }
        "HP count > free slots, > limit" - { test(2, 1)(12, 11, 10)(0, 1    ) }
      }
    }

    "getMsgAssignWorker" - {

      val insertQ = Query[(Task, Option[NodeId], Option[WorkerId], Duration, Duration, Duration), TaskId](
        "INSERT INTO msgq(type, data, node, worker, created_at, updated_at, effective_from, priority, priority_base)" +
          "VALUES(?, ?, ?, ?, now()-?, now()-?, now()-?, 5,5) RETURNING id")

      def insert(node: Option[NodeId], worker: Option[WorkerId] = None, msg: Task = defaultTask) = {
        val d: Duration = 2.days
        insertQ.toQuery0((msg, node, worker, d, d, d)).unique
      }

      def insertAssignedToOwnNode = insert(node = Some(n))

      def test(id: TaskId) =
        dao.getMsgAssignWorker(n, w, TaskHeader(id, Priority(5), Instant.now()))

      "not assign if msg has been picked up by another node" - {
        val r = TestDb ! insert(node = Some(NodeId(6789))).flatMap(test)
        assertEq(r, None)
      }

      "not assign if msg has been picked up by another worker" - {
        val r = TestDb ! insert(node = Some(n), worker = Some(WorkerId(6789))).flatMap(test)
        assertEq(r, None)
      }

      "deserialise the msg" - {
        val r = TestDb ! insertAssignedToOwnNode.flatMap(test)
        assertMatch(r) {
          case Some(TaskDetail(_, msg, _)) if msg == defaultTask => ()
        }
      }

      "assign when unassigned" - {
        val result = TestDb.!(
          for {
            id <- insertAssignedToOwnNode
            t1 <- test(id)
            t2 <- test(id)
          } yield (t1.isDefined, t2.isDefined))
        assertEq(result, (true, false))
      }
    }

  }
}
