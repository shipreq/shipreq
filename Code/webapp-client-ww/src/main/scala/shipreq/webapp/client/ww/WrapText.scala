package shipreq.webapp.client.ww

import scala.collection.immutable.ArraySeq

object WrapText {

  def apply(text: String, maxWidth: Double): String = {
    val raw     = decompose(text)
    val wrapped = wrapAtMaxWidth(raw, maxWidth)
    recompose(wrapped)
  }

  final case class Word(text: String) {
    val width = CharWidths.string(text)
  }

  private[this] val spaceWidth: Double =
    CharWidths(' ')

  private type Decomposed = ArraySeq[ArraySeq[Word]]

  private def decompose(text: String): Decomposed = {
    // Could be faster but meh
    ArraySeq.unsafeWrapArray(text.split('\n').map(a =>
      ArraySeq.unsafeWrapArray(a.split(' ').map(Word))))
  }

  private def recompose(result: Decomposed): String = {
    // Remember that string append is very fast in JS, no need for StringBuilder
    var s = ""
    var i = 0
    var firstWord = false
    for (line <- result) {
      if (s.nonEmpty)
        s += "\n"
      firstWord = true
      for (word <- line) {
        if (firstWord)
          firstWord = false
        else
          s += " "
        i += 1
        s += word.text
      }
    }
    s
  }

  private def wrapAtMaxWidth(in: Decomposed, maxWidth: Double): Decomposed = {
    val outLines = ArraySeq.newBuilder[ArraySeq[Word]]

    for (inLine <- in) {
      var line      = ArraySeq.newBuilder[Word]
      var lineEmpty = true
      var lineW     = 0.0
      var i         = 0
      while (i < inLine.length) {
        val word = inLine(i)
        val w    = word.width
        val s    = if (lineEmpty) 0.0 else spaceWidth
        val l    = lineW + s + w
        if (l > maxWidth) {
          // add to new line
          outLines += line.result()
          line      = ArraySeq.newBuilder[Word]
          line     += word
          lineW     = w
        } else {
          // add to current line
          line += word
          lineW = l
        }
        lineEmpty = false
        i += 1
      }
      outLines += line.result()
    }

    outLines.result()
  }

}
