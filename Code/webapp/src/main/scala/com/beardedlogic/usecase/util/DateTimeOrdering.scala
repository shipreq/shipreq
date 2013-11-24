package com.beardedlogic.usecase.util

import org.joda.time.DateTime

object DateTimeOrdering {
  implicit val instance: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime) = x.compareTo(y)
  }
}
