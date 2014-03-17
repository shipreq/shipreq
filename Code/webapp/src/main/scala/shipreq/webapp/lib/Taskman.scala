package shipreq.webapp.lib

import shipreq.webapp.app.AppConfig
import scala.slick.session.Session
import shipreq.taskman.api._
import shipreq.taskman.api.impl._
import TaskmanApiImpl._
import TaskmanApi._
import Effect._

object TaskmanImpl extends TaskmanInterface {

  val ctx = new GlobalContext(Some(AppConfig.TaskmanSchema))

  @inline private def run[A](task: Cmd[A], s: Session): A =
    compile(task, reify(ctx, s)).unsafePerformIO()

  override def submitTask(task: TaskDef, s: Session) = run(SubmitTask(task), s)
  override def submitTasks(tasks: Seq[TaskDef], s: Session) = run(SubmitTasks(tasks), s)
}

trait TaskmanInterface {
  def submitTask(task: TaskDef, s: Session): Unit
  def submitTasks(tasks: Seq[TaskDef], s: Session): Unit
}
