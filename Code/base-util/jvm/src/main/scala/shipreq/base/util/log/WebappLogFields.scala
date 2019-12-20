package shipreq.base.util.log

/** Additional fields to add to log messages.
 *
 * The whole point of this is safety in ElasticSearch wrt to its stupid auto-schema functionality.
 *
 * Rule 1: Never delete anything from the list below.
 * Rule 2: Never change the type of anything below.
 */
object WebappLogFields {
  private final val prefix = "shipreq.webapp."

  object jwt {
    val invalid       = LogField.Text           (prefix + "jwt.invalid")
  }

  object request {
    val id            = LogField.Text.uuid      (prefix + "request.id")
    val method        = LogField.Text           (prefix + "request.method")
    val securityToken = LogField.Text           (prefix + "request.security_token")
    val uri           = LogField.Text           (prefix + "request.uri")
    val url           = LogField.Text           (prefix + "request.url")
    val userAgent     = LogField.Text           (prefix + "request.user_agent")
    val xForwardedFor = LogField.Text           (prefix + "request.x_forwarded_for")
  }

  object response {
    val code          = LogField.Long           (prefix + "response.code")
    val durMs         = LogField.Long.durationMs(prefix + "response.duration.ms")
  }
}
