package shipreq.taskman.server.logic.app

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.std.list._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{MsgType => T, _}

/**
 * Submits message(s) specified on the command line.
 */
abstract class ManualSubmitBase extends HasLogger {

  def serialise  : Msg => JsonStr[Msg]
  def deserialise: (T, JsonStr[Msg]) => ArticulateError \/ Msg
  def runner     : (TaskmanApi[Fx] => Fx[Unit]) => Fx[Unit]

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

  private val typeAndDataRegex = """^\s*(\S+?)\s*(\{.*\})\s*$""".r.pattern

  def parseA(args: Array[String]): ParseResult =
    args.toList.foldLeft(Ok(Nil): ParseResult)(parse)

  val parse: (ParseResult, String) => ParseResult = {
    case (Help, _) | (_, "-h") | (_, "--help") => Help

    case (e@ParseError(_), _) => e

    case (Ok(msgs), arg) =>
      val m = typeAndDataRegex matcher arg
      if (!m.matches)
        ParseError(s"Unable to parse: $arg")
      else {
        val msgTypeName = m group 1
        T.lookup(msgTypeName) match {
          case None =>
            ParseError(s"Unable to parse msg type: $msgTypeName")
          case Some(msgType) =>
            val msgData = JsonStr[Msg](m group 2)
            deserialise(msgType, msgData) match {
              case -\/(e) => ParseError(e.show)
              case \/-(m) => Ok(m :: msgs)
            }
        }
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
    T.values.map(t => {
      val m = exampleFor(t)
      val name = m.getClass.getSimpleName
      val json = serialise(m).value.replace("\n", "").replace(",", ", ")
      s"  $name$json"
    }).sorted.mkString("\n")

  def exampleFor(t: T): Msg = {
    import Msg._
    val ea = EmailAddr("yoar.mum@gmail.com")
    val url = "http://hello"
    val uid = UserId(8000)
    t match {
      case T.DummyMsg                => DummyMsg("hello", failureMsg = Some("nope"))
      case T.ReRegistrationAttempted => ReRegistrationAttempted(ea)
      case T.RegistrationRequested   => RegistrationRequested(ea, url)
      case T.RegistrationCompleted   => RegistrationCompleted(uid)
      case T.PasswordResetRequested  => PasswordResetRequested(ea, url)
      case T.UserUpdated             => UserUpdated(uid)
      case T.SendDiagEmail           => SendDiagEmail(ea, "test", "hello")
      case T.LandingPageHit          => LandingPageHit(ea, "Iskaral Pust", Some("No mule can match wits with me."), false)
      case T.SyncToMailingList       => SyncToMailingList(Some("id < 100"))
      case T.WebappErrorOccurred     => WebappErrorOccurred(Some(uid), Some("/login"), "blah")
    }
  }

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