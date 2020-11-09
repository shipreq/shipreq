package shipreq.webapp.server.snippet

sealed abstract class SnippetError extends RuntimeException

object SnippetError {
  case object MemberDataNotFound extends SnippetError
}