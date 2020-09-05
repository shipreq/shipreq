package shipreq.base.util

/** https://blog.codinghorror.com/sorting-for-humans-natural-sort-order */
// Ported from http://www.davekoelle.com/files/AlphanumComparator.java
object NaturalSort extends Ordering[String] {

  private def isDigit(ch: Char) =
    (ch >= 48) && (ch <= 57)

  /** Length of string is passed in for improved efficiency (only need to calculate it once) * */
  final private def getChunk(s: String, slength: Int, _marker: Int) = {
    val chunk = new StringBuilder
    var c = s.charAt(_marker)
    chunk.append(c)
    var marker = _marker + 1
    if (isDigit(c)) {
      var loop = true
      while (loop && marker < slength) {
        c = s.charAt(marker)
        if (isDigit(c)) {
          chunk.append(c)
          marker += 1
        } else
          loop = false
      }
    } else {
      var loop = true
      while (loop && marker < slength) {
        c = s.charAt(marker);
        if (isDigit(c))
          loop = false
        else {
          chunk.append(c)
          marker += 1
        }
      }
    }
    chunk.toString
  }

  override def compare(s1: String, s2: String): Int = {
    if ((s1 == null) || (s2 == null)) return 0
    var thisMarker = 0
    var thatMarker = 0
    val s1Length = s1.length
    val s2Length = s2.length
    while (thisMarker < s1Length && thatMarker < s2Length) {
      val thisChunk = getChunk(s1, s1Length, thisMarker)
      thisMarker += thisChunk.length
      val thatChunk = getChunk(s2, s2Length, thatMarker)
      thatMarker += thatChunk.length
      // If both chunks contain numeric characters, sort them numerically
      var result = 0
      if (isDigit(thisChunk.charAt(0)) && isDigit(thatChunk.charAt(0))) { // Simple chunk comparison by length.
        val thisChunkLength = thisChunk.length
        result = thisChunkLength - thatChunk.length
        // If equal, the first different number counts
        if (result == 0) {
          val it = 0.until(thisChunkLength).iterator
          while (it.hasNext && result == 0) {
            val i = it.next()
            result = thisChunk.charAt(i) - thatChunk.charAt(i)
          }
        }
      }
      else
        result = thisChunk.compareTo(thatChunk)
      if (result != 0)
        return result
    }
    s1Length - s2Length
  }
}
