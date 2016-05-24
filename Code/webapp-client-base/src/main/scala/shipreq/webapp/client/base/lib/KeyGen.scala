package shipreq.webapp.client.base.lib

/**
 * Generate React keys.
 */
final class KeyGen {
  private var i = 64

  def next(): String = {
    i += 1
    i.toChar.toString
  }
}

object KeyGen {
  val global = new KeyGen
}
