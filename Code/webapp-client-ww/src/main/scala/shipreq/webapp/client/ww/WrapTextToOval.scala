package shipreq.webapp.client.ww

import scala.collection.immutable.ArraySeq
import shipreq.base.util.SmartSeqSplitter
import shipreq.base.util.algorithm.GeneticEvolution.Binary.Config

object WrapTextToOval {

  object Defaults {
    final val lineHeight = 14.0 * 1.25

    val geneticConfig = Config(
      popSize      = 1024,
      eliteSize    = 512,
      maxGens      = 1000,
      mutationRate = 0.02,
      allowBrute   = true,
    )

    val splitter =
      SmartSeqSplitter.genetic[Double](_ => geneticConfig)
  }

  def apply(text: String,
            idealWidth: Double,
            lineHeight: Double                 = Defaults.lineHeight,
            splitter: SmartSeqSplitter[Double] = Defaults.splitter): String = {

    val words = text.split(' ')

    if (words.length == 1)
      text

    else {
      val widths = words.map(CharWidths.string)

      val sssResult =
        SmartSeqSplitter.fillOval(
          input        = ArraySeq.unsafeWrapArray(widths),
          getLineWidth = getLineWidth,
          lineHeight   = lineHeight,
          idealWidth   = idealWidth,
          tolerance    = 0,
          splitter     = splitter,
        )

      rebuild(words, sssResult)
    }
  }

  private def rebuild(words: Array[String], result: ArraySeq[ArraySeq[Double]]): String = {
    // Remember that string append is very fast in JS, no need for StringBuilder
    var s = ""
    var i = 0
    var firstWord = false
    for (line <- result) {
      if (s.nonEmpty)
        s += "\n"
      firstWord = true
      for (_ <- line) {
        if (firstWord)
          firstWord = false
        else
          s += " "
        val word = words(i)
        i += 1
        s += word
      }
    }
    s
  }

  private[this] val spaceWidth: Double =
    CharWidths(' ')

  private[this] val getLineWidth: ArraySeq[Double] => Double =
    ds => {
      var w = 0.0
      var i = 0
      while (i < ds.length) {
        val d = ds(i)
        w += d
        if (i != 0)
          w += spaceWidth
        i += 1
      }
      w
    }
}
