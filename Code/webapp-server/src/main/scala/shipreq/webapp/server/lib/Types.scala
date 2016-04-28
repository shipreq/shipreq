package shipreq.webapp.server.lib

/**
 * @since 30/05/2013
 */
object Types {

  /** Marks a string as being an ISO-8601 representation of a datetime. */
  final case class ISO8601(value: String) extends AnyVal

  final case class ShareUrlToken(value: String) extends AnyVal
}
