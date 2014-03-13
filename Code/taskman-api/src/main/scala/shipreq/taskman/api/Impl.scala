package shipreq.taskman.api

import shipreq.taskman.api.Types._
import scalaz.{~>, \/, -\/}


object Serialisation {
  type Ser = Json[_]
  type Deser = String \/ TaskDef

  def ser(t: TaskDef): Ser = ??? // json4s blah blah

  def deser(taskTypeId: Int, s: Ser): Deser = {
    TaskTypes.lookupType(taskTypeId) match {
      case Some(tt) =>
        val defClass = TaskTypes.lookupTaskDef(tt)
        ???  // json4s blah blah
      case None =>
        -\/(s"Unknown task type: $taskTypeId")
    }
  }
}

object TaskmanApiImpl {
  trait DatabaseHandle {
    def submit(t: TaskDef): Unit = {
      val n = TaskTypes.lookupType(t).id
      val d = Serialisation.ser(t)
      submitSql(n, d)
    }
    def submitSql(id: Int, data: Json[_]): Unit
  }

  import TaskmanApi._
  import Effect._

  def reify(db: DatabaseHandle): (Cmd ~> IOM) =
    new (Cmd ~> IOM) {
      def apply[A](c: Cmd[A]): IOM[A] = c match {
        case SubmitTask1(w) => iom { db submit w }
        case SubmitTask(ws) => iom { ws.foreach(db.submit(_)) }
      }
    }

  def usage(): Unit = {
    val cmd = SubmitTask1(TaskDef.RegistrationRequested("a@b.com".tag, None))
    val program = cmdLiftF(cmd)
    val db: DatabaseHandle = ???
    val io = compile(program, reify(db))
    io.unsafePerformIO()
  }
}