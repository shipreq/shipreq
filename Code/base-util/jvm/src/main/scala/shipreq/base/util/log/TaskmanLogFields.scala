package shipreq.base.util.log

/** Additional fields to add to log messages.
  *
  * The whole point of this is safety in ElasticSearch wrt to its stupid auto-schema functionality.
  *
  * Rule 1: Never delete anything from the list below.
  * Rule 2: Never change the type of anything below.
  */
object TaskmanLogFields {
  private final val prefix = "shipreq.taskman."

  object http {
    object request {
      val body   = LogField.Text          (prefix + "http.request.body")
      val method = LogField.Text          (prefix + "http.request.method")
      val url    = LogField.Text          (prefix + "http.request.url")
    }
    object response {
      val body  = LogField.Text           (prefix + "http.response.body")
      val code  = LogField.Long           (prefix + "http.response.code")
      val durMs = LogField.Long.durationMs(prefix + "http.response.duration.ms")
    }
  }

}
