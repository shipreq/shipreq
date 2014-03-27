package shipreq.taskman.server.app

/**
 * Taskman, the Server.
 * Hard-working and magnanimous.
 */
object Server extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx { ctx =>

    // Send an email
    /*
  val from: EmailAddr = "whatever@gmail.com".tag
  val to: EmailAddr = "japgolly+test@gmail.com".tag
  val io = bopImpl(SendEmail(
    Email.Envelope(from, NonEmptyList(to)),
    Email.Content("TEST from taskman", s"Hello at ${new DateTime}.")
  ))
  val r = io.unsafePerformIO()
  log.info("DONE: {}", r)
  */

      println(ctx)

  }
}