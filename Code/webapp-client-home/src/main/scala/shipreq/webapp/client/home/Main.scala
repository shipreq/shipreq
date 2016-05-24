package shipreq.webapp.client.home

import java.time.Instant
import scalajs.js

object Main extends js.JSApp {
  def main(): Unit = {
    println(Instant.now())
    println(Instant.ofEpochMilli(0L))
    println(Instant.now().toEpochMilli)
  }
}
