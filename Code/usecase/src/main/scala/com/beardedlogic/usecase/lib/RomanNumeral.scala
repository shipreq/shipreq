package com.beardedlogic.usecase.lib

/**
 * Taken from https://gist.github.com/AndyBowes/3048075
 */
object RomanNumeral {
  
  def apply(number: Int): String = {
    toRomanNumerals(number, List(
      ("m", 1000), ("cm", 900), ("d", 500), ("cd", 400), ("c", 100), ("xc", 90),
      ("l", 50), ("xl", 40), ("x", 10), ("ix", 9), ("v", 5), ("iv", 4), ("i", 1)))
  }
  
  private def toRomanNumerals(number: Int, digits: List[(String, Int)]): String = digits match {
    case Nil    => ""
    case h :: t => h._1 * (number / h._2) + toRomanNumerals(number % h._2, t)
  }
}
