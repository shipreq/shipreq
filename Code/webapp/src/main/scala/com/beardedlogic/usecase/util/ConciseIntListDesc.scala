package com.beardedlogic.usecase.util

import scalaz.NonEmptyList

object ConciseIntListDesc {

  private sealed trait Component
  private case class Alone(value: Int) extends Component
  private case class Range(from: Int, to: Int) extends Component

  def compute[T](input: NonEmptyList[T])(f: T => Int): String = {

    val comps = input.list.foldRight(List.empty[Component])((x, cs) => {
      val i = f(x)
      cs match {
        case Alone(a) :: Alone(b) :: t if i == a - 1 && a == b - 1 =>
          Range(i, b) :: t
        case Range(a, b) :: t if i == a - 1 =>
          Range(i, b) :: t
        case _ =>
          Alone(i) :: cs
      }
    })

    val sb = new StringBuilder
    for (c <- comps) {
      if (sb.nonEmpty) sb.append(", ")
      c match {
        case Alone(i)    => sb.append(i)
        case Range(a, b) => sb.append(a); sb.append('-'); sb.append(b)
      }
    }
    sb.toString
  }
}
