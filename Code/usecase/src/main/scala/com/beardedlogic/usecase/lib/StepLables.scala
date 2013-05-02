package com.beardedlogic.usecase.lib

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

  private def labelStream(fn: (Int) => String) = "" #:: (1 to MAX_STEPS_PER_LEVEL).iterator.toStream.map(fn)

  val NUMERIC = labelStream(toNumeric _)
  val ALPHA = labelStream(toAlpha _)
  val ROMAN = labelStream(toRoman _)

  // TODO Enforce max depth
  val LABEL_MAKERS = Vector(NUMERIC, ALPHA, ROMAN, NUMERIC)
}