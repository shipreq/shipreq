package shipreq.taskman.server.logic.app

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.parser._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.std.list._
import shipreq.base.util.JsonUtil
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{TaskType => T, _}

/**
 * Submits message(s) specified on the command line.
 */
trait ManualSubmitLogic extends IOApp with HasLogger {

  protected def runner: (TaskmanApi[Fx] => Fx[Unit]) => Fx[Unit]

  final override def run(args: List[String]): IO[ExitCode] =
    parseA(args) match {
      case Ok(Nil) | Help => Fx { println(helpText); ExitCode.Success }
      case ParseError(e)  => Fx { println(s"ERROR: $e"); ExitCode.Error }
      case Ok(tasks)      => runner(submitAll(tasks)).map(_ => ExitCode.Success)
    }

  // -------------------------------------------------------------------------------------------------------------------
  // Parsing

  sealed trait ParseResult
  case class ParseError(a: String) extends ParseResult
  case class Ok(tasks: List[Task]) extends ParseResult
  case object Help extends ParseResult

  def parseA(args: List[String]): ParseResult =
    args.foldLeft(Ok(Nil): ParseResult)(parse)

  val parse: (ParseResult, String) => ParseResult = {
    case (Help, _) | (_, "-h") | (_, "--help") => Help

    case (e@ParseError(_), _) => e

    case (Ok(tasks), arg) =>
      decode(arg)(TaskJson.decoderTask) match {
        case Right(task) => Ok(task :: tasks)
        case Left(e)     => ParseError(s"Unable to parse task: ${JsonUtil.errorMsg(e)}")
      }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Help

  def helpText: String =
    s"""
      |Usage: this [<task>... | -h | --help]
      |
      |Example tasks:
      |$exampleTasks
    """.stripMargin

  def exampleTasks: String =
    T.values.map(Task.sample(_).asJson(TaskJson.encoderTask).noSpaces).sorted.mkString("\n")

  // -------------------------------------------------------------------------------------------------------------------
  // Submission

  def submitAll(tasks: List[Task]): TaskmanApi[Fx] => Fx[Unit] =
    api => Fx {
      val taskCount = tasks.size
      logger info ""

      logger info "Submitting..."
      val results = api.submitBulk(tasks).unsafeRun()
      for (((m,id),i) <- results.zipWithIndex.map(_.map2(_+1))) {
        logger info s"[$i/$taskCount] $id <= $m"
      }
      logger info "Success."

      logger info ""
    }
}