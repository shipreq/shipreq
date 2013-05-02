package com.beardedlogic.usecase.lib

import scala.collection.mutable.{ Map => MutableMap }

/**
 * @since 2/05/2013
 */
object StepLabels {

  // TODO Enforce max steps/level
  val MAX_STEPS_PER_LEVEL = 99

  private def toNumeric(i: Int) = i.toString

  private def toAlpha(i1: Int) = {
    val i = i1 - 1
    if (i >= 26)
      (i / 26 + 'a' - 1).toChar + (i % 26 + 'a').toChar.toString
    else
      (i + 'a').toChar.toString
  }

  private def toRoman(i: Int) = RomanNumeral(i)

  class LabelMaker(private val fn: (Int) => String) {
    private def create = {
      var labels = MutableMap[Int, String]()
      var indices = MutableMap[String, Int]()
      for (i <- 1 to MAX_STEPS_PER_LEVEL) {
        val l = fn(i)
        labels(i) = l
        indices(l) = i
      }
      (labels.toMap, indices.toMap)
    }

    val (label, index) = create
    def apply(i: Int) = label(i)
    def apply(l: String) = index(l)
  }

  val NUMERIC = new LabelMaker(toNumeric _)
  val ALPHA = new LabelMaker(toAlpha _)
  val ROMAN = new LabelMaker(toRoman _)

  // TODO Enforce max depth
  val LABEL_MAKERS = Vector(NUMERIC, ALPHA, ROMAN, NUMERIC)
}