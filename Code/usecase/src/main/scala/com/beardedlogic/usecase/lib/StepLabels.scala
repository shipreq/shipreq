package com.beardedlogic.usecase.lib

import scala.collection.mutable.{ Map => MutableMap }
import TypeTags._

/**
 * @since 2/05/2013
 */
object StepLabels {

  final val MaxStepsPerLevel = 99

  private def toNumeric(i: Int) = i.toString

  private def toAlpha(i1: Int) = {
    val i = i1 - 1
    if (i >= 26)
      (i / 26 + 'a' - 1).toChar + (i % 26 + 'a').toChar.toString
    else
      (i + 'a').toChar.toString
  }

  private def toRoman(i: Int) = RomanNumeral(i)

  /**
   * Bidirectional map of labels ("ii","vii","c") to their indices (which are usually one-based).
   */
  final class LabelMaker private (val min: Int, val label: Map[Int, String], val index: Map[String, Int]) {
    def apply(i: Int) = label(i).asLabel
    def apply(l: String) = index(l)
  }

  private object LabelMaker {
    def apply(min: Int, fn: (Int) => String) = {
      val labels = MutableMap[Int, String]()
      val indices = MutableMap[String, Int]()
      for (i <- min to MaxStepsPerLevel) {
        val l = fn(i)
        labels(i) = l
        indices(l) = i
      }
      new LabelMaker(min, labels.toMap, indices.toMap)
    }
  }

  final val NUMERIC_0 = LabelMaker(0, toNumeric _)
  final val NUMERIC = LabelMaker(1, toNumeric _)
  final val ALPHA = LabelMaker(1, toAlpha _)
  final val ROMAN = LabelMaker(1, toRoman _)

  // TODO Enforce max depth
  // (1.)0.1.a.i.4
  final val LabelMakers = Vector(NUMERIC_0, NUMERIC, ALPHA, ROMAN, NUMERIC)
}