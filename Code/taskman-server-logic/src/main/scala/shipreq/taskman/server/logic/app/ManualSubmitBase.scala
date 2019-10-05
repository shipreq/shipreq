package shipreq.taskman.server.logic.app

import io.circe.parser._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.std.list._
import shipreq.base.util.JsonUtil
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{MsgType => T, _}

/**
 * Submits message(s) specified on the command line.
 */
abstract class ManualSubmitBase extends HasLogger {

  def runner: (TaskmanApi[Fx] => Fx[Unit]) => Fx[Unit]

  def main(args: Array[String]): Unit =
    parseA(args) match {
      case Ok(Nil) | Help => println(helpText)
      case ParseError(e)  => println(s"ERROR: $e"); System exit 1
      case Ok(msgs)       => runner(submitAll(msgs)).unsafeRun()
    }

  // -------------------------------------------------------------------------------------------------------------------
  // Parsing

  sealed trait ParseResult
  case class ParseError(a: String) extends ParseResult
  case class Ok(msgs: List[Msg]) extends ParseResult
  case object Help extends ParseResult

  def parseA(args: Array[String]): ParseResult =
    args.toList.foldLeft(Ok(Nil): ParseResult)(parse)

  val parse: (ParseResult, String) => ParseResult = {
    case (Help, _) | (_, "-h") | (_, "--help") => Help

    case (e@ParseError(_), _) => e

    case (Ok(msgs), arg) =>
      decode(arg)(MsgJson.decoderMsg) match {
        case Right(msg) => Ok(msg :: msgs)
        case Left(e)    => ParseError(s"Unable to parse msg: ${JsonUtil.errorMsg(e)}")
      }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Help

  def helpText: String =
    s"""
      |Usage: this [<msg>... | -h | --help]
      |
      |Example msgs:
      |$exampleMsgs
    """.stripMargin

  def exampleMsgs: String =
    T.values.map(Msg.sample(_).asJson(MsgJson.encoderMsg).noSpaces).sorted.mkString("\n")

  // -------------------------------------------------------------------------------------------------------------------
  // Submission

  def submitAll(msgs: List[Msg]): TaskmanApi[Fx] => Fx[Unit] =
    api => Fx {
      val msgCount = msgs.size
      logger info ""

      logger info "Submitting..."
      val results = api.submitMsgs(msgs).unsafeRun()
      for (((m,id),i) <- results.zipWithIndex.map(_.map2(_+1))) {
        logger info s"[$i/$msgCount] $id <= $m"
      }
      logger info "Success."

      logger info ""
    }
}