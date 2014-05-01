package shipreq.taskman.server.app

import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorOr
import shipreq.base.util.ScalaExt.Tuple2Ext
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{MsgType => T, _}

/**
 * Submits message(s) specified on the command line.
 */
abstract class ManualSubmitBase extends HasLogger {

  def serialise  : Msg => Json[Msg]
  def deserialise: (T, Json[Msg]) => ErrorOr[Msg]
  def runner     : (ApiOpReifier => Unit) => Unit

  def main(args: Array[String]): Unit =
    parseA(args) match {
      case Ok(Nil) | Help => println(helpText)
      case ParseError(e)  => println(s"ERROR: $e"); System exit 1
      case Ok(msgs)       => runner(submitAll(msgs))
    }

  // -------------------------------------------------------------------------------------------------------------------
  // Parsing

  sealed trait ParseResult
  case class ParseError(a: String) extends ParseResult
  case class Ok(msgs: List[Msg]) extends ParseResult
  case object Help extends ParseResult

  private val typeAndDataRegex = """^\s*(\S+?)\s*(\{.*\})\s*$""".r.pattern

  def parseA(args: Array[String]): ParseResult =
    ((Ok(Nil): ParseResult) /: args.toList)(parse)

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
            val msgData = m.group(2).tag[IsJsonFor[Msg]]
            deserialise(msgType, msgData) match {
              case -\/(e) => ParseError(e.msg)
              case \/-(m) => Ok(m :: msgs)
            }
        }
      }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Help

  def helpText(): String =
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
      val json = serialise(m).replace("\n", "").replace(",", ", ")
      s"  '$name$json'"
    }).sorted.mkString("\n")

  def exampleFor(t: T): Msg = {
    import Msg._
    val ea = "you@gmail.com".tag[IsEmailAddr]
    val url = "http://hello"
    val uid = 8000.tag[IsUserId]
    t match {
      case T.DummyMsg                => DummyMsg("hello", failureMsg = Some("nope"))
      case T.ReRegistrationAttempted => ReRegistrationAttempted(ea)
      case T.RegistrationRequested   => RegistrationRequested(ea, url)
      case T.RegistrationCompleted   => RegistrationCompleted(uid)
      case T.PasswordResetRequested  => PasswordResetRequested(ea, url)
      case T.SendDiagEmail           => SendDiagEmail(ea, "test", "hello")
      case T.LandingPageHit          => LandingPageHit(ea, "Iskaral Pust", Some("No mule can match wits with me."), false)
      case T.SyncToMailingList       => SyncToMailingList(Some("id < 100"))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Submission

  def submitAll(msgs: List[Msg]): ApiOpReifier => Unit =
    aopReifier => {
      val msgCount = msgs.size
      log info ""

      log info "Submitting..."
      val results = aopReifier(ApiOp.SubmitMsgs(msgs)).unsafePerformIO()
      for (((m,id),i) <- results.zipWithIndex.map(_.map2(_+1))) {
        log info s"[$i/$msgCount] $id <= $m"
      }
      log info "Success."

      log info ""
    }
}