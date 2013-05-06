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

  final class LabelMaker private (val min: Int, val label: Map[Int, String], val index: Map[String, Int]) {
    def apply(i: Int) = label(i)
    def apply(l: String) = index(l)
  }
  object LabelMaker {
    def apply(min: Int, fn: (Int) => String) = {
      val labels = MutableMap[Int, String]()
      val indices = MutableMap[String, Int]()
      for (i <- min to MAX_STEPS_PER_LEVEL) {
        val l = fn(i)
        labels(i) = l
        indices(l) = i
      }
      new LabelMaker(min, labels.toMap, indices.toMap)
    }
  }

  val NUMERIC_0 = LabelMaker(0, toNumeric _)
  val NUMERIC = LabelMaker(1, toNumeric _)
  val ALPHA = LabelMaker(1, toAlpha _)
  val ROMAN = LabelMaker(1, toRoman _)

  // TODO Enforce max depth
  // (1.)0.1.a.i.4
  val LABEL_MAKERS = Vector(NUMERIC_0, NUMERIC, ALPHA, ROMAN, NUMERIC)
}