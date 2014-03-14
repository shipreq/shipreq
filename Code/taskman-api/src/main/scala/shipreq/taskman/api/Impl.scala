package shipreq.taskman.api

import shipreq.base.util.BiMap
import shipreq.taskman.api.Types._
import scalaz.{~>, \/-, \/, -\/}
import shipreq.taskman.api.{TaskType => T}
import shipreq.taskman.api.{TaskDef => D}



object Serialisation {
  type Ser = Json[TaskDef]
  type Deser = String \/ TaskDef

  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.{read, write}

  def fieldRenamer[A: Manifest](m: BiMap[String, String]): FieldSerializer[A] = FieldSerializer({
    case p@(name, x) =>
      Some(m.ab.get(name).map(newName => (newName, x)).getOrElse(p))
  },{
    case f@JField(name, x) =>
      m.ba.get(name).map(newName => JField(newName, x)).getOrElse(f)
  })

  implicit val formats: Formats = (
    Serialization.formats(NoTypeHints)
      + fieldRenamer[D.RegistrationRequested](BiMap("email" -> "e", "url" -> "u"))
      + fieldRenamer[D.RegistrationCompleted](BiMap("userId" -> "u"))
      + fieldRenamer[D.PasswordResetRequested](BiMap("email" -> "e", "url" -> "u"))
      + fieldRenamer[D.LandingPageHit](BiMap("email" -> "e", "name" -> "n", "msg" -> "m", "newsletter" -> "w"))
  )

  def ser(t: TaskDef): Ser = write(t).tag

  def deser(taskTypeId: Int, s: Ser): Deser = {
    TaskTypes.lookupType(taskTypeId) match {
      case Some(tt) =>
        val defClass = TaskTypes.lookupTaskDef(tt)
        try {
          val t: TaskDef = read(s)(implicitly[Formats], Manifest.classType(defClass))
          \/-(t)
        } catch {
          case e: Throwable =>
            -\/(s"Failed to parse JSON.\nError:\n  ${e.getClass.getCanonicalName}: ${e.getMessage.replaceAll("\n", "\n  ")}\nJSON Value:\n  $s")
        }
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