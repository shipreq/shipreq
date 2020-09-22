package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import java.time.{Duration, Instant}

/** Repeats a task every G until time exceeds W.
  * Multiple invocations' repeats are merged.
  *
  * Example:
  *
  *       gap = 100
  *    window = 300
  *
  *      0 --+  -+-      +-- .run called; task executes; deadline=300
  *          |   |       |
  *     50 --+   |       |
  *          |   |       |
  *    100 --+   |       +-- task executes; repeats because 100 <= 300
  *          |   |       |
  *    150 --+   |  -+-  +-- .run called; task executes; deadline=450
  *          |   |   |   |
  *    200 --+   |   |   +-- task executes; repeats because 200 <= 450
  *          |   |   |   |
  *    250 --+   |   |   |
  *          |   |   |   |
  *    300 --+  -+-  |   +-- task executes; repeats because 300 <= 450
  *          |       |   |
  *    350 --+       |   |
  *          |       |   |
  *    400 --+       |   +-- task executes; repeats because 400 <= 450
  *          |       |   |
  *    450 --+      -+-  |
  *          |           |
  *    500 --+           +-- task executes; stops because 500 > 450
  */
final class TaskRepeater(task: Callback, gap: Duration, window: Duration, now: CallbackTo[Instant]) {
  private var deadline = now.runNow()
  private var queued = false

  private lazy val queueIfNecessary: Callback =
    for {
      _ <- CallbackOption.require(!queued)
      t <- now.toCBO
      _ <- CallbackOption.require(!t.isAfter(deadline))
      _ <- queuedTask.delay(gap).toCallback
      _ <- Callback { queued = true }
    } yield ()

  private lazy val queuedTask: Callback =
    for {
      _ <- Callback { queued = false }
      _ <- task.attempt
      _ <- queueIfNecessary
    } yield ()

  val run: Callback =
    Callback {
      deadline = now.runNow().plusMillis(window.toMillis)
    } >> task.attempt >> queueIfNecessary
}

object TaskRepeater {

  def apply(task: Callback, gap: Duration, window: Duration): TaskRepeater =
    new TaskRepeater(task, gap, window, ClientUtil.now)

  def millis(task: Callback, gap: Int, window: Int): TaskRepeater =
    apply(task, Duration.ofMillis(gap), Duration.ofMillis(window))
}